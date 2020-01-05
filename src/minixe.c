#include "minilang/minilang.h"
#include "minilang/ml_file.h"
#include "minilang/ml_object.h"
#include "minilang/ml_iterfns.h"
#include "minilang/ml_macros.h"
#include "minilang/ml_console.h"
#include "minilang/linenoise.h"
#include <stdio.h>
#include <string.h>
#include <gc/gc.h>

typedef struct xe_node_t {
	const ml_type_t *Type;
	ml_value_t *Tag, *Attributes, *Content;
} xe_node_t;

typedef struct xe_var_t {
	const ml_type_t *Type;
	ml_value_t *Name;
} xe_var_t;

static ml_type_t *XENodeT = 0;
static ml_type_t *XEVarT = 0;

static void append_node(ml_value_t *List, ml_value_t *Node) {
	if (Node->Type == MLListT) {
		for (ml_list_node_t *Node1 = ml_list_head(Node); Node1; Node1 = Node1->Next) {
			append_node(List, Node1->Value);
		}
	} else if (Node->Type == MLStringT) {
		if (ml_list_length(List) > 0) {
			ml_list_node_t *Tail = ml_list_tail(List);
			if (Tail->Value->Type == MLStringT) {
				int Length1 = ml_string_length(Tail->Value);
				int Length2 = ml_string_length(Node);
				char *Concat = GC_malloc_atomic(Length1 + Length2 + 1);
				memcpy(Concat, ml_string_value(Tail->Value), Length1);
				memcpy(Concat + Length1, ml_string_value(Node), Length2);
				Concat[Length1 + Length2] = 0;
				Tail->Value = ml_string(Concat, Length1 + Length2);
			} else {
				ml_list_append(List, Node);
			}
		} else {
			ml_list_append(List, Node);
		}
	} else if (Node->Type == MLIntegerT) {
		ml_list_append(List, ml_string_format("%ld", ml_integer_value(Node)));
	} else if (Node->Type == MLRealT) {
		ml_list_append(List, ml_string_format("%f", ml_real_value(Node)));
	} else if (Node->Type == XENodeT) {
		ml_list_append(List, Node);
	} else if (Node->Type == XEVarT) {
		ml_list_append(List, Node);
	}
}

typedef struct xe_stream_t xe_stream_t;

struct xe_stream_t {
	const char *Next;
	const char *(*read)(xe_stream_t *);
	void *Data;
	const char *Source;
	int LineNo;
};

static ml_value_t *parse_node(xe_stream_t *Stream);

static const char *parse_escape(const char *P, ml_stringbuffer_t *Buffer) {
	switch (P[1]) {
	case '\\':
		ml_stringbuffer_add(Buffer, "\\", 1);
		break;
	case 't':
		ml_stringbuffer_add(Buffer, "\t", 1);
		break;
	case 'r':
		ml_stringbuffer_add(Buffer, "\r", 1);
		break;
	case 'n':
		ml_stringbuffer_add(Buffer, "\n", 1);
		break;
	case '\"':
		ml_stringbuffer_add(Buffer, "\"", 1);
		break;
	case '<':
		ml_stringbuffer_add(Buffer, "<", 1);
		break;
	case '>':
		ml_stringbuffer_add(Buffer, ">", 1);
		break;
	case 'x': {
		unsigned char C;
		if ('0' <= P[2] <= '9') {
			C = P[2] - '0';
		} else if ('a' <= P[2] <= 'f') {
			C = 10 + P[2] - 'a';
		} else if ('A' <= P[2] <= 'F') {
			C = 10 + P[2] - 'A';
		} else {
			return 0;
		}
		C *= 16;
		if ('0' <= P[3] <= '9') {
			C += P[3] - '0';
		} else if ('a' <= P[3] <= 'f') {
			C += 10 + P[3] - 'a';
		} else if ('A' <= P[3] <= 'F') {
			C += 10 + P[3] - 'A';
		} else {
			return 0;
		}
		ml_stringbuffer_add(Buffer, (char *)&C, 1);
		P += 2;
		break;
	}
	}
	return P + 2;
}

static ml_value_t *parse_string(xe_stream_t *Stream) {
	ml_stringbuffer_t Buffer[1] = {ML_STRINGBUFFER_INIT};
	const char *Next = Stream->Next;
	const char *P = Next;
	for (;;) {
		if (P[0] == 0) {
			return ml_error("ParseError", "End of input in string at line %d in %s", Stream->LineNo, Stream->Source);
		} else if (P[0] == '\\') {
			ml_stringbuffer_add(Buffer, Next, P - Next);
			P = Next = parse_escape(P, Buffer);
			if (!P) return ml_error("ParseError", "Invalid escape sequence at line %d in %s", Stream->LineNo, Stream->Source);
		} else if (P[0] == '\"') {
			ml_stringbuffer_add(Buffer, Next, P - Next);
			Stream->Next = P + 1;
			break;
		} else {
			++P;
		}
	}
	return ml_stringbuffer_get_string(Buffer);
}

#define SKIP_WHITESPACE \
	while (Next[0] <= ' ') { \
		if (Next[0] == 0) { \
			Next = Stream->read(Stream); \
			if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source); \
			++Stream->LineNo; \
		} else { \
			++Next; \
		} \
	}

static ml_value_t *parse_value(xe_stream_t *Stream) {
	const char *Next = Stream->Next;
	SKIP_WHITESPACE;
	if (Next[0] == '<') {
		Stream->Next = Next + 1;
		return parse_node(Stream);
	} else if (Next[0] == '[') {
		++Next;
		ml_value_t *List = ml_list();
		SKIP_WHITESPACE;
		if (Next[0] != ']') for (;;) {
			Stream->Next = Next;
			ml_value_t *Value = parse_value(Stream);
			Next = Stream->Next;
			if (Value->Type == MLErrorT) return Value;
			ml_list_put(List, Value);
			SKIP_WHITESPACE;
			if (Next[0] == ']') break;
			if (Next[0] != ',') return ml_error("ParseError", "Expected , at %d in %s", Stream->LineNo, Stream->Source);
			++Next;
		}
		Stream->Next = Next + 1;
		return List;
	} else if (Next[0] == '{') {
		return ml_error("ParseError", "Map parsing not complete yet at %d in %s", Stream->LineNo, Stream->Source);
	} else if (Next[0] == '\"') {
		Stream->Next = Next + 1;
		return parse_string(Stream);
	} else if (Next[0] == '-' || ('0' <= Next[0] && Next[0] <= '9') || Next[0] == '.') {
		char *End;
		long Value = strtol(Next, &End, 10);
		if (End[0] == '.' || End[0] == 'e' || End[0] == 'E') {
			double Value = strtod(Next, &End);
			Stream->Next = End;
			return ml_real(Value);
		} else {
			Stream->Next = End;
			return ml_integer(Value);
		}
	} else {
		return ml_error("ParseError", "Invalid value syntax at line %d in %s", Stream->LineNo, Stream->Source);
	}
}

static ml_value_t *parse_node(xe_stream_t *Stream) {
	const char *Next = Stream->Next;
	for (;;) {
		char Delim = *Next;
		if (Delim <= ' ') break;
		if (Delim == ':') break;
		if (Delim == '|') break;
		if (Delim == '>') break;
		++Next;
	}
	int TagLength = Next - Stream->Next;
	if (Stream->Next[0] == '$') {
		if (Next[0] != '>') return ml_error("ParseError", "Expected > at line %d in %s", Stream->LineNo, Stream->Source);
		--TagLength;
		char *Name = GC_malloc_atomic(TagLength + 1);
		memcpy(Name, Stream->Next + 1, TagLength);
		Name[TagLength] = 0;
		xe_var_t *Var = new(xe_var_t);
		Var->Type = XEVarT;
		Var->Name = ml_string(Name, TagLength);
		Stream->Next = Next + 1;
		return (ml_value_t *)Var;
	}
	char *Tag = GC_malloc_atomic(TagLength + 1);
	memcpy(Tag, Stream->Next, TagLength);
	Tag[TagLength] = 0;
	ml_value_t *Attributes = ml_map();
	ml_value_t *Content = ml_list();
	for (;;) {
		SKIP_WHITESPACE;
		if (Next[0] != ':' && Next[0] != '|' && Next[0] != '>' && Next[0] != '?') {
			const char *Start = Next;
			for (;;) {
				char Delim = *Next;
				if (Delim <= ' ') break;
				if (Delim == ':') break;
				if (Delim == '|') break;
				if (Delim == '>') break;
				if (Delim == '=') break;
				++Next;
			}
			int NameLength = Next - Start;
			char *Name = GC_malloc_atomic(NameLength + 1);
			memcpy(Name, Start, NameLength);
			Name[NameLength] = 0;
			while (Next[0] <= ' ') {
				if (Next[0] == 0) {
					Next = Stream->read(Stream);
					if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source);
					++Stream->LineNo;
				} else {
					++Next;
				}
			}
			if (Next[0] != '=') return ml_error("ParseError", "Expected = at line %d in %s", Stream->LineNo, Stream->Source);
			++Next;
			while (Next[0] <= ' ') {
				if (Next[0] == 0) {
					Next = Stream->read(Stream);
					if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source);
					++Stream->LineNo;
				} else {
					++Next;
				}
			}
			Stream->Next = Next;
			ml_value_t *Value = parse_value(Stream);
			if (Value->Type == MLErrorT) return Value;
			ml_map_insert(Attributes, ml_string(Name, NameLength), Value);
			Next = Stream->Next;
		} else {
			break;
		}
	}
	if (Next[0] == ':') {
		ml_stringbuffer_t Buffer[1] = {ML_STRINGBUFFER_INIT};
		const char *End = ++Next;
		for (;;) {
			if (End[0] == 0) {
				ml_stringbuffer_add(Buffer, Next, End - Next);
				//ml_stringbuffer_add(Buffer, "\n", 1);
				Next = Stream->read(Stream);
				if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source);
				++Stream->LineNo;
				End = Next;
			} else if (End[0] == '\\') {
				ml_stringbuffer_add(Buffer, Next, End - Next);
				End = Next = parse_escape(End, Buffer);
				if (!End) return ml_error("ParseError", "Invalid escape sequence at line %d in %s", Stream->LineNo, Stream->Source);
			} else if (End[0] == '<') {
				ml_stringbuffer_add(Buffer, Next, End - Next);
				if (Buffer->Length) append_node(Content, ml_stringbuffer_get_string(Buffer));
				Stream->Next = End + 1;
				ml_list_append(Content, parse_node(Stream));
				End = Next = Stream->Next;
			} else if (End[0] == '>') {
				ml_stringbuffer_add(Buffer, Next, End - Next);
				if (Buffer->Length) append_node(Content, ml_stringbuffer_get_string(Buffer));
				break;
			} else {
				++End;
			}
		}
		Stream->Next = End + 1;
	} else if (Next[0] == '|') {
		const char *End = ++Next;
		for (;;) {
			if (End[0] == 0) {
				Next = Stream->read(Stream);
				if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source);
				++Stream->LineNo;
				End = Next;
			} else if (End[0] == '<') {
				Stream->Next = End + 1;
				ml_list_append(Content, parse_node(Stream));
				End = Next = Stream->Next;
			} else if (End[0] == '>') {
				break;
			} else if (End[0] <= ' ') {
				++End;
			} else {
				return ml_error("ParseError", "Non whitespace character in | node at line %d in %s", Stream->LineNo, Stream->Source);
			}
		}
		Stream->Next = End + 1;
	} else {
		Stream->Next = Next + 1;
	}
	xe_node_t *Node = new(xe_node_t);
	Node->Type = XENodeT;
	Node->Tag = ml_string(Tag, TagLength);
	Node->Attributes = Attributes;
	Node->Content = Content;
	return (ml_value_t *)Node;
}

static ml_value_t *node_eval(ml_value_t *Value, ml_value_t *Attributes, ml_value_t *Content) {
	if (Value->Type == MLListT) {
		ml_value_t *List = ml_list();
		for (ml_list_node_t *Node = ml_list_head(Value); Node; Node = Node->Next) {
			ml_value_t *Value2 = node_eval(Node->Value, Attributes, Content);
			if (Value2->Type == MLErrorT) return Value2;
			append_node(List, Value2);
		}
		return List;
	} else if (Value->Type == XENodeT) {
		xe_node_t *Node = (xe_node_t *)Value;
		ml_value_t *Attributes2 = ml_map();
		ML_MAP_FOREACH(Node->Attributes, Attribute) {
			ml_value_t *Value2 = node_eval(Attribute->Value, Attributes, Content);
			if (Value2->Type == MLErrorT) return Value2;
			ml_map_insert(Attributes2, Attribute->Key, Value2);
		}
		ml_value_t *Content2 = ml_list();
		ML_LIST_FOREACH(Node->Content, Node2) {
			ml_value_t *Value2 = node_eval(Node2->Value, Attributes, Content);
			if (Value2->Type == MLErrorT) return Value2;
			append_node(Content2, Value2);
		}
		xe_node_t *Node2 = new(xe_node_t);
		Node2->Type = XENodeT;
		Node2->Tag = Node->Tag;
		Node2->Attributes = Attributes2;
		Node2->Content = Content2;
		return (ml_value_t *)Node2;
	} else if (Value->Type == MLIntegerT) {
		return Value;
	} else if (Value->Type == MLRealT) {
		return Value;
	} else if (Value->Type == MLStringT) {
		return Value;
	} else if (Value->Type == XEVarT) {
		xe_var_t *Var = (xe_var_t *)Value;
		if (ml_string_length(Var->Name)) {
			ml_value_t *Value2 = ml_map_search(Attributes, Var->Name);
			if (Value2 != MLNil) return Value2;
			return Value;
		} else {
			return Content;
		}
	} else if (ml_is(Value, MLFunctionT)) {
		return ml_inline(Value, 2, Attributes, Content);
	} else {
		return Value;
	}
}

typedef struct node_path_t node_path_t;

struct node_path_t {
	node_path_t *Parent;
	const char *Tag;
};

static ml_value_t *node_expand(ml_value_t *Value, node_path_t *Path);

typedef struct node_defines_t node_defines_t;

struct node_defines_t {
	stringmap_t Symbols[1];
	stringmap_t Parents[1];
};

static node_defines_t Defines[1] = {{
	{STRINGMAP_INIT},
	{STRINGMAP_INIT}
}};

static ml_value_t *define_lookup(node_defines_t *Defines, const char *Tag, node_path_t *Path) {
	if (Path) {
		node_defines_t *Parent = stringmap_search(Defines->Parents, Path->Tag);
		if (Parent) {
			ml_value_t *Value = define_lookup(Parent, Tag, Path->Parent);
			if (Value) return Value;
		}
	}
	return stringmap_search(Defines->Symbols, Tag);
}

static ml_value_t *node_expand(ml_value_t *Value, node_path_t *Path) {
	for (;;) {
		if (Value->Type == MLListT) {
			ml_value_t *List = ml_list();
			ML_LIST_FOREACH(Value, Node) {
				ml_value_t *Value2 = node_expand(Node->Value, Path);
				if (Value2->Type == MLErrorT) return Value2;
				if (Value2 != MLNil) ml_list_append(List, Value2);
			}
			return List;
		} else if (Value->Type == XENodeT) {
			xe_node_t *Node = (xe_node_t *)Value;
			const char *Tag = ml_string_value(Node->Tag);
			ml_value_t *Define = define_lookup(Defines, Tag, Path);
			if (Define) {
				Value = node_eval(Define, Node->Attributes, Node->Content);
			} else {
				node_path_t SubPath[1] = {Path, Tag};
				ML_MAP_FOREACH(Node->Attributes, MapNode) {
					MapNode->Value = node_expand(MapNode->Value, SubPath);
				}
				ml_value_t *Content = ml_list();
				ML_LIST_FOREACH(Node->Content, Node2) {
					ml_value_t *Value2 = node_expand(Node2->Value, SubPath);
					if (Value2->Type == MLErrorT) return Value2;
					append_node(Content, Value2);
				}
				Node->Content = Content;
				return (ml_value_t *)Node;
			}
		} else {
			return Value;
		}
	}
	return MLNil;
}

static void compile_string(ml_value_t *Value, ml_stringbuffer_t *Source) {
	ml_stringbuffer_add(Source, "\"", 1);
	int Length = ml_string_length(Value);
	const char *String = ml_string_value(Value);
	int I = 0;
	for (int J = 0; J < Length; ++J) {
		if (String[J] < ' ') {
			if (J > I) ml_stringbuffer_add(Source, String + I, J - I);
			switch (String[J]) {
			case '\t':
				ml_stringbuffer_add(Source, "\\t", 2);
				break;
			case '\r':
				ml_stringbuffer_add(Source, "\\r", 2);
				break;
			case '\n':
				ml_stringbuffer_add(Source, "\\n", 2);
				break;
			}
			I = J + 1;
		} else if (String[J] == '\"') {
			if (J > I) ml_stringbuffer_add(Source, String + I, J - I);
			ml_stringbuffer_add(Source, "\\\"", 2);
			I = J + 1;
		}
	}
	if (Length > I) ml_stringbuffer_add(Source, String + I, Length - I);
	ml_stringbuffer_add(Source, "\"", 1);
}

static void compile_inline_node(ml_value_t *Value, ml_stringbuffer_t *Source);

static void compile_inline_value(ml_value_t *Value, ml_stringbuffer_t *Source) {
	if (Value->Type == MLListT) {
		ml_stringbuffer_add(Source, "[", 1);
		if (ml_list_length(Value)) {
			ml_list_node_t *Node = ml_list_head(Value);
			compile_inline_node(Node->Value, Source);
			while ((Node = Node->Next)) {
				ml_stringbuffer_add(Source, ",", 1);
				compile_inline_value(Node->Value, Source);
			}
		}
		ml_stringbuffer_add(Source, "]", 1);
	} else if (Value->Type == XENodeT) {
		compile_inline_node(Value, Source);
	} else if (Value->Type == XEVarT) {
		xe_var_t *Var = (xe_var_t *)Value;
		if (ml_string_length(Var->Name)) {
			ml_stringbuffer_add(Source, "Attributes[", strlen("Attributes["));
			compile_string(Var->Name, Source);
			ml_stringbuffer_add(Source, "]", 1);
		} else {
			ml_stringbuffer_add(Source, "Content", strlen("Content"));
		}
	} else if (Value->Type == MLStringT) {
		compile_string(Value, Source);
	} else if (Value->Type == MLIntegerT) {
		ml_stringbuffer_addf(Source, "%ld", ml_integer_value(Value));
	} else if (Value->Type == MLRealT) {
		ml_stringbuffer_addf(Source, "%f", ml_real_value(Value));
	}
}

static void compile_inline_node(ml_value_t *Value, ml_stringbuffer_t *Source) {
	if (Value->Type == XENodeT) {
		xe_node_t *Node = (xe_node_t *)Value;
		if (ml_string_length(Node->Tag)) {
			ml_stringbuffer_add(Source, "node(", 5);
			compile_string(Node->Tag, Source);
			ml_stringbuffer_add(Source, ",{", 2);
			ml_map_node_t *MapNode = ml_map_head(Node->Attributes);
			if (MapNode) {
			loop:
				compile_string(MapNode->Key, Source);
				ml_stringbuffer_add(Source, " is ", 4);
				compile_inline_value(MapNode->Value, Source);
				if ((MapNode = MapNode->Next)) {
					ml_stringbuffer_add(Source, ",", 1);
					goto loop;
				}
			}
			ml_stringbuffer_add(Source, "},[", 3);
			if (ml_list_length(Node->Content)) {
				ml_list_node_t *Node2 = ml_list_head(Node->Content);
				compile_inline_node(Node2->Value, Source);
				while ((Node2 = Node2->Next)) {
					ml_stringbuffer_add(Source, ",", 1);
					compile_inline_node(Node2->Value, Source);
				}
			}
			ml_stringbuffer_add(Source, "])", 2);
		} else {
			ML_LIST_FOREACH(Node->Content, Node2) {
				ml_value_t *Value = Node2->Value;
				if (Value->Type == MLStringT) {
					ml_stringbuffer_add(Source, ml_string_value(Value), ml_string_length(Value));
				} else {
					compile_inline_node(Value, Source);
				}
			}
		}
	} else if (Value->Type == XEVarT) {
		xe_var_t *Var = (xe_var_t *)Value;
		if (ml_string_length(Var->Name)) {
			ml_stringbuffer_add(Source, "Attributes[", 11);
			compile_string(Var->Name, Source);
			ml_stringbuffer_add(Source, "]", 1);
		} else {
			ml_stringbuffer_add(Source, "Content", 7);
		}
	} else if (Value->Type == MLStringT) {
		compile_string(Value, Source);
	} else if (Value->Type == MLIntegerT) {
		ml_stringbuffer_addf(Source, "%ld", ml_integer_value(Value));
	} else if (Value->Type == MLRealT) {
		ml_stringbuffer_addf(Source, "%f", ml_real_value(Value));
	}
}

static const char *string_read(xe_stream_t *Stream) {
	const char *Next = (const char *)Stream->Data, *End = Next;
	if (!Next) return 0;
	while (End[0] >= ' ') ++End;
	int Length = (End - Next) + 1;
	char *Line = GC_malloc_atomic(Length + 1);
	memcpy(Line, Next, Length);
	Line[Length] = 0;
	if (End[0]) {
		Stream->Data = (void *)(End + 1);
	} else {
		Stream->Data = 0;
	}
	return Line;
}

static stringmap_t Globals[1] = {STRINGMAP_INIT};

static ml_value_t *global_get(void *Data, const char *Name) {
	return stringmap_search(Globals, Name) ?: MLNil;
}

static ml_value_t *compile_macro(ml_value_t *Value);

static ml_value_t *compile_macro(ml_value_t *Value) {
	if (Value->Type == MLListT) {
		ML_LIST_FOREACH(Value, Node) {
			Node->Value = compile_macro(Node->Value);
		}
	} else if (Value->Type == XENodeT) {
		ml_stringbuffer_t Source[1] = {ML_STRINGBUFFER_INIT};
		xe_node_t *Node = (xe_node_t *)Value;
		ML_MAP_FOREACH(Node->Attributes, Attribute) Attribute->Value = compile_macro(Attribute->Value);
		ML_LIST_FOREACH(Node->Content, Node2) {
			Node2->Value = compile_macro(Node2->Value);
		}
	}
	return Value;
}

static ml_value_t *xe_function(void *Data, int Count, ml_value_t **Args) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	ml_stringbuffer_t Source[1] = {ML_STRINGBUFFER_INIT};
	ml_stringbuffer_add(Source, "fun(Attributes, Content) do ", strlen("fun(Attributes, Content) do "));
	for (ml_list_node_t *Node2 = ml_list_head(Content); Node2; Node2 = Node2->Next) {
		ml_value_t *Value2 = Node2->Value;
		if (Value2->Type == MLStringT) {
			ml_stringbuffer_add(Source, ml_string_value(Value2), ml_string_length(Value2));
		} else {
			compile_inline_node(Value2, Source);
		}
	}
	ml_stringbuffer_add(Source, " end", 4);
	xe_stream_t Stream[1];
	Stream->Data = ml_stringbuffer_get(Source);
	printf("Source = %s\n", Stream->Data);
	Stream->read = string_read;
	mlc_context_t Context[1];
	Context->Globals = Globals;
	Context->GlobalGet = (ml_getter_t)global_get;
	mlc_on_error(Context) return Context->Error;
	mlc_scanner_t *Scanner = ml_scanner("node", Stream, (void *)string_read, Context);
	ml_value_t *Macro = ml_command_evaluate(Scanner, Globals, Context) ?: ml_error("ParseError", "Empty body");
	Macro = Macro->Type->deref(Macro);
	if (Macro->Type == MLErrorT) return Macro;
	ml_value_t *Name = ml_map_search(Attributes, ml_string("name", 4));
	if (Name != MLNil) {
		if (Name->Type != MLStringT) return ml_error("MacroError", "name attribute must be a string");
		node_defines_t *Scope = Defines;
		ml_value_t *Path = ml_map_search(Attributes, ml_string("path", 4));
		if (Path != MLNil) {
			if (Path->Type != MLListT) return ml_error("MacroError", "path attribute must be a list");
			for (ml_list_node_t *Node = ml_list_tail(Path); Node; Node = Node->Prev) {
				ml_value_t *Tag = Node->Value;
				if (Tag->Type != XENodeT) return ml_error("MacroError", "path entry must be a node");
				node_defines_t **Slot = (node_defines_t **)stringmap_slot(Scope->Parents, ml_string_value(((xe_node_t*)Tag)->Tag));
				if (!Slot[0]) Slot[0] = new(node_defines_t);
				Scope = Slot[0];
			}
		}
		stringmap_insert(Scope->Symbols, ml_string_value(Name), Macro);
	}
	return Macro;
}

static ml_value_t *xe_define(void *Data, int Count, ml_value_t **Args) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	ml_value_t *Name = ml_map_search(Attributes, ml_string("name", 4));
	if (Name == MLNil) return ml_error("MacroError", "define macro requires name attribute");
	if (Name->Type != MLStringT) return ml_error("MacroError", "name attribute must be a string");
	ml_value_t *Macro = compile_macro(Content);
	node_defines_t *Scope = Defines;
	ml_value_t *Path = ml_map_search(Attributes, ml_string("path", 4));
	if (Path != MLNil) {
		if (Path->Type != MLListT) return ml_error("MacroError", "path attribute must be a list");
		for (ml_list_node_t *Node = ml_list_tail(Path); Node; Node = Node->Prev) {
			ml_value_t *Tag = Node->Value;
			if (Tag->Type != XENodeT) return ml_error("MacroError", "path entry must be a node");
			node_defines_t **Slot = (node_defines_t **)stringmap_slot(Scope->Parents, ml_string_value(((xe_node_t*)Tag)->Tag));
			if (!Slot[0]) Slot[0] = new(node_defines_t);
			Scope = Slot[0];
		}
	}
	stringmap_insert(Scope->Symbols, ml_string_value(Name), Macro);
	return MLNil;
}

static ml_value_t *xe_do(void *Data, int Count, ml_value_t **Args) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	ml_stringbuffer_t Source[1] = {ML_STRINGBUFFER_INIT};
	ML_LIST_FOREACH(Content, Node) {
		ml_value_t *Value = Node->Value;
		if (Value->Type == MLStringT) {
			ml_stringbuffer_add(Source, ml_string_value(Value), ml_string_length(Value));
		} else {
			compile_inline_node(Value, Source);
		}
	}
	ml_value_t *Result = MLNil;
	xe_stream_t Stream[1];
	Stream->Data = ml_stringbuffer_get(Source);
	printf("Source = %s\n", Stream->Data);
	Stream->read = string_read;
	mlc_context_t Context[1];
	Context->Globals = Globals;
	Context->GlobalGet = (ml_getter_t)global_get;
	mlc_on_error(Context) return Context->Error;
	mlc_scanner_t *Scanner = ml_scanner("node", Stream, (void *)string_read, Context);
	for (;;) {
		ml_value_t *Value = ml_command_evaluate(Scanner, Globals, Context);
		if (!Value) break;
		if (Value->Type == MLErrorT) return Value;
		Result = Value->Type->deref(Value);
	}
	return Result;
}

static const char *file_read(xe_stream_t *Stream) {
	FILE *File = (FILE *)Stream->Data;
	char *Line = 0;
	size_t Length = 0;
	ssize_t Read = getline(&Line, &Length, File);
	if (Read < 0) {
		fclose(File);
		return 0;
	}
	return Line;
}

static ml_value_t *xe_include(void *Data, int Count, ml_value_t **Args) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	ml_value_t *FileArg = ml_map_search(Attributes, ml_string("file", 4));
	if (FileArg == MLNil) return ml_error("MacroError", "include macro requires file attribute");
	if (FileArg->Type != MLStringT) return ml_error("MacroError", "file attribute must be a string");
	const char *FileName = ml_string_value(FileArg);
	FILE *File = fopen(FileName, "r");
	if (!File) return ml_error("FileError", "Error opening file %s", FileName);
	xe_stream_t Stream[1];
	Stream->Data = (void *)File;
	Stream->LineNo = 1;
	Stream->Source = FileName;
	Stream->read = file_read;
	const char *Next = file_read(Stream);
	ml_value_t *Contents = ml_list();
	for (;;) {
		while (Next[0] <= ' ') {
			if (Next[0] == 0) {
				Next = Stream->read(Stream);
				if (!Next) return Contents;
				++Stream->LineNo;
			} else {
				++Next;
			}
		}
		if (Next[0] != '<') return ml_error("ParseError", "Node must begin with <");
		Stream->Next = Next + 1;
		ml_value_t *Node = parse_node(Stream);
		if (Node->Type == MLErrorT) return Node;
		ml_list_append(Contents, Node);
		Next = Stream->Next;
	}
	return Contents;
}

static ml_value_t *xe_map(void *Data, int Count, ml_value_t **Args) {
	return Args[0];
}

static ml_value_t *xe_list(void *Data, int Count, ml_value_t **Args) {
	return Args[1];
}

static ml_value_t *StringMethod;
static ml_value_t *AppendMethod;

static int xe_attribute_to_string(ml_value_t *Key, ml_value_t *Value, ml_stringbuffer_t *Buffer) {
	ml_stringbuffer_add(Buffer, " ", 1);
	ml_stringbuffer_add(Buffer, ml_string_value(Key), ml_string_length(Key));
	ml_stringbuffer_add(Buffer, "=", 1);
	if (Value->Type == XENodeT) {
		ml_inline(AppendMethod, 2, Buffer, Value);
	} else {
		compile_inline_value(Value, Buffer);
	}
	return 0;
}

static ml_value_t *xe_node_append(void *Data, int Count, ml_value_t **Args) {
	ml_stringbuffer_t *Buffer = (ml_stringbuffer_t *)Args[0];
	xe_node_t *Node = (xe_node_t *)Args[1];
	ml_stringbuffer_add(Buffer, "<", 1);
	ml_stringbuffer_add(Buffer, ml_string_value(Node->Tag), ml_string_length(Node->Tag));
	if (ml_map_size(Node->Attributes)) {
		ml_map_foreach(Node->Attributes, Buffer, (void *)xe_attribute_to_string);
	}
	if (ml_list_length(Node->Content)) {
		ml_stringbuffer_add(Buffer, ":", 1);
		for (ml_list_node_t *N = ml_list_head(Node->Content); N; N = N->Next) {
			ml_inline(AppendMethod, 2, Buffer, N->Value);
		}
	}
	ml_stringbuffer_add(Buffer, ">", 1);
	return Args[0];
}

static ml_value_t *xe_node_to_string(void *Data, int Count, ml_value_t **Args) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	ml_stringbuffer_t Buffer[1] = {ML_STRINGBUFFER_INIT};
	ml_stringbuffer_add(Buffer, "<", 1);
	ml_stringbuffer_add(Buffer, ml_string_value(Node->Tag), ml_string_length(Node->Tag));
	if (ml_map_size(Node->Attributes)) {
		ml_map_foreach(Node->Attributes, Buffer, (void *)xe_attribute_to_string);
	}
	if (ml_list_length(Node->Content)) {
		ml_stringbuffer_add(Buffer, ":", 1);
		for (ml_list_node_t *N = ml_list_head(Node->Content); N; N = N->Next) {
			ml_inline(AppendMethod, 2, Buffer, N->Value);
		}
	}
	ml_stringbuffer_add(Buffer, ">", 1);
	return ml_stringbuffer_get_string(Buffer);
}

static ml_value_t *xe_var_append(void *Data, int Count, ml_value_t **Args) {
	ml_stringbuffer_t *Buffer = (ml_stringbuffer_t *)Args[0];
	xe_var_t *Var = (xe_var_t *)Args[1];
	ml_stringbuffer_add(Buffer, "<$", 2);
	ml_stringbuffer_add(Buffer, ml_string_value(Var->Name), ml_string_length(Var->Name));
	ml_stringbuffer_add(Buffer, ">", 1);
	return Args[0];
}

static ml_value_t *xe_var_to_string(void *Data, int Count, ml_value_t **Args) {
	xe_var_t *Var = (xe_var_t *)Args[0];
	return ml_string_format("<$%s>", ml_string_value(Var->Name));
}

static ml_value_t *xe_parse_string(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(1);
	ML_CHECK_ARG_TYPE(0, MLStringT);
	xe_stream_t Stream[1];
	Stream->Data = (void *)ml_string_value(Args[0]);
	Stream->LineNo = 1;
	Stream->Source = "string";
	Stream->read = string_read;
	const char *Next = string_read(Stream);
	while (Next[0] <= ' ') {
		if (Next[0] == 0) {
			Next = Stream->read(Stream);
			if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source);
			++Stream->LineNo;
		} else {
			++Next;
		}
	}
	if (Next[0] != '<') return ml_error("ParseError", "Node must begin with <");
	Stream->Next = Next + 1;
	return parse_node(Stream);
}

static ml_value_t *xe_parse_file(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(1);
	ML_CHECK_ARG_TYPE(0, MLStringT);
	const char *FileName = ml_string_value(Args[0]);
	FILE *File = fopen(FileName, "r");
	if (!File) return ml_error("FileError", "Error opening file %s", FileName);
	xe_stream_t Stream[1];
	Stream->Data = (void *)File;
	Stream->LineNo = 1;
	Stream->Source = FileName;
	Stream->read = file_read;
	const char *Next = file_read(Stream);
	while (Next[0] <= ' ') {
		if (Next[0] == 0) {
			Next = Stream->read(Stream);
			if (!Next) return ml_error("ParseError", "Unexpected end of input at line %d in %s", Stream->LineNo, Stream->Source);
			++Stream->LineNo;
		} else {
			++Next;
		}
	}
	if (Next[0] != '<') return ml_error("ParseError", "Node must begin with <");
	Stream->Next = Next + 1;
	return parse_node(Stream);
}

static ml_value_t *xe_expand_node(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(1);
	return node_expand(Args[0], NULL);
}

static ml_value_t *xe_node(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(3);
	ML_CHECK_ARG_TYPE(0, MLStringT);
	ML_CHECK_ARG_TYPE(1, MLMapT);
	ML_CHECK_ARG_TYPE(2, MLListT);
	xe_node_t *Node = new(xe_node_t);
	Node->Type = XENodeT;
	Node->Tag = Args[0];
	Node->Attributes = Args[1];
	Node->Content = Args[2];
	return (ml_value_t *)Node;
}

static ml_value_t *xe_eval(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(3);
	ML_CHECK_ARG_TYPE(1, MLMapT);
	ML_CHECK_ARG_TYPE(2, MLListT);
	return node_eval(Args[0], Args[1], Args[2]);
}

static ml_value_t *xe_append(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(2);
	ML_CHECK_ARG_TYPE(0, MLListT);
	append_node(Args[0], Args[1]);
	return Args[0];
}

static ml_value_t *xe_node_tag(void *Data, int Count, ml_value_t **Args) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	return Node->Tag;
}

static ml_value_t *xe_node_attributes(void *Data, int Count, ml_value_t **Args) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	return Node->Attributes;
}

static ml_value_t *xe_node_content(void *Data, int Count, ml_value_t **Args) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	return Node->Content;
}

static ml_value_t *print(void *Data, int Count, ml_value_t **Args) {
	static ml_value_t *StringMethod = 0;
	if (!StringMethod) StringMethod = ml_method("string");
	for (int I = 0; I < Count; ++I) {
		ml_value_t *Result = Args[I];
		if (Result->Type != MLStringT) {
			Result = ml_call(StringMethod, 1, &Result);
			if (Result->Type == MLErrorT) return Result;
			if (Result->Type != MLStringT) return ml_error("ResultError", "string method did not return string");
		}
		fwrite(ml_string_value(Result), 1, ml_string_length(Result), stdout);
	}
	fflush(stdout);
	return MLNil;
}

static ml_value_t *error(void *Data, int Count, ml_value_t **Args) {
	ML_CHECK_ARG_COUNT(2);
	ML_CHECK_ARG_TYPE(0, MLStringT);
	ML_CHECK_ARG_TYPE(1, MLStringT);
	return ml_error(ml_string_value(Args[0]), "%s", ml_string_value(Args[1]));
}

static const char *xe_line_read(xe_stream_t *Stream) {
#ifdef __MINGW32__
	fputs("--> ", stdout);
	char *Line;
	if (!ml_read_line(stdin, 0, &Line)) return NULL;
#else
	const char *Line = linenoise("--> ");
	if (!Line) return NULL;
	linenoiseHistoryAdd(Line);
#endif
	int Length = strlen(Line);
	char *Buffer = snew(Length + 2);
	memcpy(Buffer, Line, Length);
	Buffer[Length] = '\n';
	Buffer[Length + 1] = 0;
	return Buffer;
}

int main(int Argc, char **Argv) {
	ml_init();
	ml_file_init(Globals);
	ml_object_init(Globals);
	ml_iterfns_init(Globals);
	stringmap_insert(Globals, "print", ml_function(0, print));
	stringmap_insert(Globals, "error", ml_function(0, error));
	stringmap_insert(Globals, "parse_string", ml_function(0, xe_parse_string));
	stringmap_insert(Globals, "parse_file", ml_function(0, xe_parse_file));
	stringmap_insert(Globals, "expand", ml_function(0, xe_expand_node));
	stringmap_insert(Globals, "node", ml_function(0, xe_node));
	stringmap_insert(Globals, "eval", ml_function(0, xe_eval));
	stringmap_insert(Globals, "append", ml_function(0, xe_append));
	XENodeT = ml_type(MLAnyT, "xe-node");
	XEVarT = ml_type(MLAnyT, "xe-variable");
	stringmap_insert(Defines->Symbols, "!function", ml_function(NULL, xe_function));
	stringmap_insert(Defines->Symbols, "!define", ml_function(NULL, xe_define));
	stringmap_insert(Defines->Symbols, "!do", ml_function(NULL, xe_do));
	stringmap_insert(Defines->Symbols, "", ml_function(NULL, xe_do));
	stringmap_insert(Defines->Symbols, "!include", ml_function(NULL, xe_include));
	stringmap_insert(Defines->Symbols, "!map", ml_function(NULL, xe_map));
	stringmap_insert(Defines->Symbols, "!list", ml_function(NULL, xe_list));
	StringMethod = ml_method("string");
	AppendMethod = ml_method("append");
	ml_method_by_name("string", NULL, xe_node_to_string, XENodeT, NULL);
	ml_method_by_name("append", NULL, xe_node_append, MLStringBufferT, XENodeT, NULL);
	ml_method_by_name("string", NULL, xe_var_to_string, XEVarT, NULL);
	ml_method_by_name("append", NULL, xe_var_append, MLStringBufferT, XEVarT, NULL);
	ml_method_by_name("tag", NULL, xe_node_tag, XENodeT, NULL);
	ml_method_by_name("attributes", NULL, xe_node_attributes, XENodeT, NULL);
	ml_method_by_name("content", NULL, xe_node_content, XENodeT, NULL);
	const char *FileName = 0;
	int Interactive = 0;
	for (int I = 1; I < Argc; ++I) {
		if (Argv[I][0] == '-') {
			switch (Argv[I][1]) {
			case 'i': Interactive = 1; break;
			}
		} else {
			FileName = Argv[I];
		}
	}
	if (Interactive) {
		xe_stream_t Stream[1];
		Stream->LineNo = 1;
		Stream->Source = "string";
		if (FileName) {
			FILE *File = fopen(FileName, "r");
			if (!File) {
				printf("Error: Error opening file %s", FileName);
				return 1;
			}
			Stream->Data = File;
			Stream->read = file_read;
		} else {
			Stream->read = xe_line_read;
		}
		Stream->Next = "";
		for (;;) {
			const char *Next = Stream->Next;
			while (Next[0] <= ' ') {
				if (Next[0] == 0) {
					Next = Stream->read(Stream);
					if (!Next) return 0;
					++Stream->LineNo;
				} else {
					++Next;
				}
			}
			ml_value_t *Result;
			if (Next[0] != '<') {
				Stream->Next = "";
				Result = ml_error("ParseError", "Node must begin with <");
			} else {
				Stream->Next = Next + 1;
				Result = parse_node(Stream);
			}
			if (Result->Type == XENodeT) Result = node_expand(Result, NULL);
			if (Result->Type == MLErrorT) {
				printf("Error: %s\n", ml_error_message(Result));
				const char *Source;
				int Line;
				for (int I = 0; ml_error_trace(Result, I, &Source, &Line); ++I) printf("\t%s:%d\n", Source, Line);
			} else {
				print(NULL, 1, &Result);
				puts("");
			}
		}
	} else if (FileName) {
		ml_value_t *Closure = ml_load(global_get, Globals, FileName, NULL);
		if (Closure->Type == MLErrorT) {
			printf("Error: %s\n", ml_error_message(Closure));
			const char *Source;
			int Line;
			for (int I = 0; ml_error_trace(Closure, I, &Source, &Line); ++I) printf("\t%s:%d\n", Source, Line);
			return 1;
		}
		ml_value_t *Result = ml_call(Closure, 0, NULL);
		if (Result->Type == MLErrorT) {
			printf("Error: %s\n", ml_error_message(Result));
			const char *Source;
			int Line;
			for (int I = 0; ml_error_trace(Result, I, &Source, &Line); ++I) printf("\t%s:%d\n", Source, Line);
			return 1;
		}
	} else {
		ml_console(global_get, Globals);
	}
	return 0;
}
