#include "minilang.h"
#include "ml_file.h"
#include "ml_object.h"
#include "ml_iterfns.h"
#include "ml_macros.h"
#include "ml_console.h"
#include "linenoise.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <gc/gc.h>

typedef struct xe_node_t {
	const ml_type_t *Type;
	ml_value_t *Tag, *Attributes, *Content;
	ml_source_t Source;
} xe_node_t;

typedef struct xe_var_t {
	const ml_type_t *Type;
	ml_value_t *Name, *Default;
} xe_var_t;

ML_TYPE(XENodeT, MLAnyT, "xe-node");
ML_TYPE(XEVarT, MLAnyT, "xe-var");

static void node_append(ml_value_t *List, ml_value_t *Node) {
	if (Node->Type == MLListT) {
		ML_LIST_FOREACH(Node, Iter) node_append(List, Iter->Value);
	} else if (Node->Type == MLStringT) {
		if (ml_list_length(List) > 0) {
			ml_value_t *Tail = ml_list_get(List, -1);
			if (Tail->Type == MLStringT) {
				int Length1 = ml_string_length(Tail);
				int Length2 = ml_string_length(Node);
				char *Concat = GC_malloc_atomic(Length1 + Length2 + 1);
				memcpy(Concat, ml_string_value(Tail), Length1);
				memcpy(Concat + Length1, ml_string_value(Node), Length2);
				Concat[Length1 + Length2] = 0;
				ml_list_set(List, -1, ml_string(Concat, Length1 + Length2));
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
	int LineNo = Stream->LineNo;
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
		--TagLength;
		char *Name = GC_malloc_atomic(TagLength + 1);
		memcpy(Name, Stream->Next + 1, TagLength);
		Name[TagLength] = 0;
		ml_value_t *Default = MLNil;
		SKIP_WHITESPACE;
		if (Next[0] == ':') {
			Default = ml_list();
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
					if (Buffer->Length) node_append(Default, ml_stringbuffer_get_string(Buffer));
					Stream->Next = End + 1;
					ml_list_append(Default, parse_node(Stream));
					End = Next = Stream->Next;
				} else if (End[0] == '>') {
					ml_stringbuffer_add(Buffer, Next, End - Next);
					if (Buffer->Length) node_append(Default, ml_stringbuffer_get_string(Buffer));
					break;
				} else {
					++End;
				}
			}
			Stream->Next = End + 1;
		} else {
			Stream->Next = Next + 1;
		}
		xe_var_t *Var = new(xe_var_t);
		Var->Type = XEVarT;
		if (!TagLength || isalpha(Name[0])) {
			Var->Name = ml_string(Name, TagLength);
		} else {
			Var->Name = ml_integer(atoi(Name));
		}
		Var->Default = Default;
		return (ml_value_t *)Var;
	} else {
		char *Tag = GC_malloc_atomic(TagLength + 1);
		memcpy(Tag, Stream->Next, TagLength);
		Tag[TagLength] = 0;
		ml_value_t *Attributes = ml_map();
		ml_value_t *Content = ml_list();
		int Index = 1;
		for (;;) {
			SKIP_WHITESPACE;
			if (Next[0] != ':' && Next[0] != '|' && Next[0] != '>' && Next[0] != '?') {
				const char *Start = Next;
				for (;;) {
					if (!isalpha(*Next)) break;
					/*char Delim = *Next;
					if (Delim <= ' ') break;
					if (Delim == ':') break;
					if (Delim == '|') break;
					if (Delim == '>') break;
					if (Delim == '=') break;*/
					++Next;
				}
				int NameLength = Next - Start;
				ml_value_t *Name;
				if (NameLength) {
					Name = ml_string(Start, NameLength);
					SKIP_WHITESPACE;
					if (Next[0] != '=') return ml_error("ParseError", "Expected = at line %d in %s", Stream->LineNo, Stream->Source);
					++Next;
					SKIP_WHITESPACE;
				} else {
					Name = ml_integer(Index++);
				}
				Stream->Next = Next;
				ml_value_t *Value = parse_value(Stream);
				if (Value->Type == MLErrorT) return Value;
				ml_map_insert(Attributes, Name, Value);
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
					if (Buffer->Length) node_append(Content, ml_stringbuffer_get_string(Buffer));
					Stream->Next = End + 1;
					ml_list_append(Content, parse_node(Stream));
					End = Next = Stream->Next;
				} else if (End[0] == '>') {
					ml_stringbuffer_add(Buffer, Next, End - Next);
					if (Buffer->Length) node_append(Content, ml_stringbuffer_get_string(Buffer));
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
		Node->Source.Name = Stream->Source;
		Node->Source.Line = LineNo;
		return (ml_value_t *)Node;
	}
}

typedef struct xe_scope_t xe_scope_t;

struct xe_scope_t {
	const ml_type_t *Type;
	stringmap_t Symbols[1];
	stringmap_t Parents[1];
};

ML_TYPE(XEScopeT, MLAnyT, "xe-scope");

static ml_value_t *node_eval(ml_value_t *Value, ml_value_t *Attributes, ml_value_t *Content, xe_scope_t *Scope) {
	if (Value->Type == MLListT) {
		ml_value_t *List = ml_list();
		ML_LIST_FOREACH(Value, Iter) {
			ml_value_t *Value2 = node_eval(Iter->Value, Attributes, Content, Scope);
			if (Value2->Type == MLErrorT) return Value2;
			node_append(List, Value2);
		}
		return List;
	} else if (Value->Type == XENodeT) {
		xe_node_t *Node = (xe_node_t *)Value;
		ml_value_t *Attributes2 = ml_map();
		ML_MAP_FOREACH(Node->Attributes, Iter) {
			ml_value_t *Value2 = node_eval(Iter->Value, Attributes, Content, Scope);
			if (Value2->Type == MLErrorT) return Value2;
			ml_map_insert(Attributes2, Iter->Key, Value2);
		}
		ml_value_t *Content2 = ml_list();
		ML_LIST_FOREACH(Node->Content, Iter) {
			ml_value_t *Value2 = node_eval(Iter->Value, Attributes, Content, Scope);
			if (Value2->Type == MLErrorT) return Value2;
			node_append(Content2, Value2);
		}
		xe_node_t *Node2 = new(xe_node_t);
		Node2->Type = XENodeT;
		Node2->Tag = Node->Tag;
		Node2->Attributes = Attributes2;
		Node2->Content = Content2;
		Node2->Source = Node->Source;
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
			if (Value2 == MLNil) return Var->Default;
			return Value2;
		} else {
			return Content;
		}
	} else if (ml_is(Value, MLFunctionT)) {
		return ml_inline(Value, 3, Attributes, Content, Scope);
	} else {
		return Value;
	}
}

typedef struct node_path_t node_path_t;

struct node_path_t {
	node_path_t *Parent;
	const char *Tag;
};

static xe_scope_t GlobalScope[1] = {{
	XEScopeT,
	{STRINGMAP_INIT},
	{STRINGMAP_INIT}
}};

static ml_value_t *define_lookup(xe_scope_t *Defines, const char *Tag, node_path_t *Path) {
	if (Path) {
		xe_scope_t *Parent = stringmap_search(Defines->Parents, Path->Tag);
		if (Parent) {
			ml_value_t *Value = define_lookup(Parent, Tag, Path->Parent);
			if (Value) return Value;
		}
	}
	return stringmap_search(Defines->Symbols, Tag);
}

static ml_value_t *node_expand(ml_value_t *Value, node_path_t *Path, xe_scope_t *Scope) {
	for (;;) {
		if (Value->Type == MLListT) {
			ml_value_t *List = ml_list();
			ML_LIST_FOREACH(Value, Node) {
				ml_value_t *Value2 = node_expand(Node->Value, Path, Scope);
				if (Value2->Type == MLErrorT) return Value2;
				node_append(List, Value2);
			}
			return List;
		} else if (Value->Type == XENodeT) {
			xe_node_t *Node = (xe_node_t *)Value;
			const char *Tag = ml_string_value(Node->Tag);
			ml_value_t *Define = define_lookup(GlobalScope, Tag, Path);
			if (Define) {
				Value = node_eval(Define, Node->Attributes, Node->Content, Scope);
			} else {
				node_path_t SubPath[1] = {Path, Tag};
				ML_MAP_FOREACH(Node->Attributes, Iter) {
					ml_map_iter_update(Iter, node_expand(Iter->Value, SubPath, Scope));
				}
				ml_value_t *Content = ml_list();
				ML_LIST_FOREACH(Node->Content, Iter) {
					ml_value_t *Value2 = node_expand(Iter->Value, SubPath, Scope);
					if (Value2->Type == MLErrorT) return Value2;
					node_append(Content, Value2);
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
		int Comma = 0;
		ML_LIST_FOREACH(Value, Iter) {
			if (Comma) ml_stringbuffer_add(Source, ",", 1);
			compile_inline_node(Iter->Value, Source);
			Comma = 1;
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
			int Comma = 0;
			ML_MAP_FOREACH(Node->Attributes, Iter) {
				if (Comma) ml_stringbuffer_add(Source, ",", 1);
				compile_string(Iter->Key, Source);
				ml_stringbuffer_add(Source, " is ", 4);
				compile_inline_value(Iter->Value, Source);
			}
			ml_stringbuffer_add(Source, "},[", 3);
			Comma = 0;
			ML_LIST_FOREACH(Node->Content, Iter) {
				if (Comma) ml_stringbuffer_add(Source, ",", 1);
				compile_inline_node(Iter->Value, Source);
				Comma = 1;
			}
			ml_stringbuffer_add(Source, "])", 2);
		} else {
			ML_LIST_FOREACH(Node->Content, Iter) {
				ml_value_t *Value = Iter->Value;
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

static ml_value_t *compile_macro(ml_value_t *Value) {
	if (Value->Type == MLListT) {
		ML_LIST_FOREACH(Value, Iter) {
			ml_list_iter_update(Iter, compile_macro(Iter->Value));
		}
	} else if (Value->Type == XENodeT) {
		ml_stringbuffer_t Source[1] = {ML_STRINGBUFFER_INIT};
		xe_node_t *Node = (xe_node_t *)Value;
		ML_MAP_FOREACH(Node->Attributes, Iter) {
			ml_map_iter_update(Iter, compile_macro(Iter->Value));
		}
		compile_macro(Node->Content);
	}
	return Value;
}

ML_FUNCTIONX(XEFunction) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	xe_scope_t *Scope = (xe_scope_t *)Args[2];
	ml_stringbuffer_t Source[1] = {ML_STRINGBUFFER_INIT};
	ml_stringbuffer_add(Source, "fun(Attributes, Content, Scope) do ", strlen("fun(Attributes, Content, Scope) do "));
	ML_LIST_FOREACH(Content, Iter) {
		ml_value_t *Value2 = Iter->Value;
		if (Value2->Type == MLStringT) {
			ml_stringbuffer_add(Source, ml_string_value(Value2), ml_string_length(Value2));
		} else {
			compile_inline_node(Value2, Source);
		}
	}
	ml_stringbuffer_add(Source, " end", 4);
	xe_stream_t Stream[1];
	Stream->Data = ml_stringbuffer_get(Source);
	//printf("Source = %s\n", Stream->Data);
	Stream->read = string_read;
	mlc_scanner_t *Scanner = ml_scanner("node", Stream, (void *)string_read, (ml_getter_t)global_get, Globals);
	ml_scanner_source(Scanner, ml_debugger_source(Caller));
	ml_value_t *Macro = ML_WRAP_EVAL(ml_command_evaluate, Scanner, Globals) ?: ml_error("ParseError", "Empty body");
	Macro = Macro->Type->deref(Macro);
	if (Macro->Type == MLErrorT) ML_RETURN(Macro);
	ml_value_t *Name = ml_map_search(Attributes, ml_integer(1));
	if (Name != MLNil) {
		if (Name->Type != MLStringT) ML_RETURN(ml_error("MacroError", "name attribute must be a string"));
		stringmap_insert(Scope->Symbols, ml_string_value(Name), Macro);
	}
	ML_RETURN(Macro);
}

ML_FUNCTION(XEDefine) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	xe_scope_t *Scope = (xe_scope_t *)Args[2];
	ml_value_t *Name = ml_map_search(Attributes, ml_integer(1));
	if (Name == MLNil) return ml_error("MacroError", "define macro requires name attribute");
	if (Name->Type != MLStringT) return ml_error("MacroError", "name attribute must be a string");
	ml_value_t *Macro = compile_macro(Content);
	stringmap_insert(Scope->Symbols, ml_string_value(Name), Macro);
	return MLNil;
}

static xe_scope_t *xe_scope_parse(xe_scope_t *Scope, xe_node_t *Path) {
	if (Path->Type != XENodeT) return (xe_scope_t *)ml_error("MacroError", "path attribute must be a node");
	xe_node_t *Next = (xe_node_t *)ml_list_get(Path->Content, 1);
	if (Next) {
		Scope = xe_scope_parse(Scope, Next);
		if (Scope->Type == MLErrorT) return Scope;
	}
	xe_scope_t **Slot = (xe_scope_t **)stringmap_slot(Scope->Parents, ml_string_value(Path->Tag));
	if (!Slot[0]) {
		Scope = Slot[0] = new(xe_scope_t);
		Scope->Type = XEScopeT;
	} else {
		Scope = Slot[0];
	}
	return Scope;
}

ML_FUNCTION(XEIn) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	xe_scope_t *Scope = (xe_scope_t *)Args[2];
	ml_value_t *Path = ml_map_search(Attributes, ml_integer(1));
	if (Path == MLNil) return ml_error("MacroError", "in macro requires path attribute");
	Scope = xe_scope_parse(Scope, (xe_node_t *)Path);
	if (Scope->Type == MLErrorT) return (ml_value_t *)Scope;
	ML_LIST_FOREACH(Content, Iter) node_expand(Iter->Value, NULL, Scope);
	return MLNil;
}

ML_FUNCTIONX(XEDo) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	ml_stringbuffer_t Source[1] = {ML_STRINGBUFFER_INIT};
	ML_LIST_FOREACH(Content, Iter) {
		ml_value_t *Value = Iter->Value;
		if (Value->Type == MLStringT) {
			ml_stringbuffer_add(Source, ml_string_value(Value), ml_string_length(Value));
		} else {
			compile_inline_node(Value, Source);
		}
	}
	ml_value_t *Result = MLNil;
	xe_stream_t Stream[1];
	Stream->Data = ml_stringbuffer_get(Source);
	//printf("Source = %s\n", Stream->Data);
	Stream->read = string_read;
	mlc_scanner_t *Scanner = ml_scanner("node", Stream, (void *)string_read, (ml_getter_t)global_get, Globals);
	ml_scanner_source(Scanner, ml_debugger_source(Caller));
	for (;;) {
		ml_value_t *Value = ML_WRAP_EVAL(ml_command_evaluate, Scanner, Globals);
		if (!Value) break;
		if (Value->Type == MLErrorT) ML_RETURN(Value);
		Result = Value->Type->deref(Value);
	}
	ML_RETURN(Result);
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

ML_FUNCTION(XEInclude) {
	ml_value_t *Attributes = Args[0];
	ml_value_t *Content = Args[1];
	ml_value_t *FileArg = ml_map_search(Attributes, ml_integer(1));
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

ML_FUNCTION(XEMap) {
	return Args[0];
}

ML_FUNCTION(XEList) {
	return Args[1];
}

static int xe_attribute_to_string(ml_value_t *Key, ml_value_t *Value, ml_stringbuffer_t *Buffer) {
	ml_stringbuffer_add(Buffer, " ", 1);
	ml_stringbuffer_append(Buffer, Key);
	ml_stringbuffer_add(Buffer, "=", 1);
	if (Value->Type == XENodeT) {
		ml_inline(MLStringBufferAppendMethod, 2, Buffer, Value);
	} else {
		compile_inline_value(Value, Buffer);
	}
	return 0;
}

ML_METHOD(MLStringBufferAppendMethod, MLStringBufferT, XENodeT) {
	ml_stringbuffer_t *Buffer = (ml_stringbuffer_t *)Args[0];
	xe_node_t *Node = (xe_node_t *)Args[1];
	ml_stringbuffer_add(Buffer, "<", 1);
	ml_stringbuffer_add(Buffer, ml_string_value(Node->Tag), ml_string_length(Node->Tag));
	if (ml_map_size(Node->Attributes)) {
		ml_map_foreach(Node->Attributes, Buffer, (void *)xe_attribute_to_string);
	}
	if (ml_list_length(Node->Content)) {
		ml_stringbuffer_add(Buffer, ":", 1);
		ML_LIST_FOREACH(Node->Content, Iter) {
			ml_inline(MLStringBufferAppendMethod, 2, Buffer, Iter->Value);
		}
	}
	ml_stringbuffer_add(Buffer, ">", 1);
	return Args[0];
}

ML_METHOD(MLStringOfMethod, XENodeT) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	ml_stringbuffer_t Buffer[1] = {ML_STRINGBUFFER_INIT};
	ml_stringbuffer_add(Buffer, "<", 1);
	ml_stringbuffer_add(Buffer, ml_string_value(Node->Tag), ml_string_length(Node->Tag));
	if (ml_map_size(Node->Attributes)) {
		ml_map_foreach(Node->Attributes, Buffer, (void *)xe_attribute_to_string);
	}
	if (ml_list_length(Node->Content)) {
		ml_stringbuffer_add(Buffer, ":", 1);
		ML_LIST_FOREACH(Node->Content, Iter) {
			ml_inline(MLStringBufferAppendMethod, 2, Buffer, Iter->Value);
		}
	}
	ml_stringbuffer_add(Buffer, ">", 1);
	return ml_stringbuffer_get_string(Buffer);
}

ML_METHOD(MLStringBufferAppendMethod, MLStringBufferT, XEVarT) {
	ml_stringbuffer_t *Buffer = (ml_stringbuffer_t *)Args[0];
	xe_var_t *Var = (xe_var_t *)Args[1];
	ml_stringbuffer_add(Buffer, "<$", 2);
	ml_stringbuffer_append(Buffer, Var->Name);
	ml_stringbuffer_add(Buffer, ">", 1);
	return Args[0];
}

ML_METHOD(MLStringOfMethod, XEVarT) {
	xe_var_t *Var = (xe_var_t *)Args[0];
	if (Var->Name->Type == MLIntegerT) {
		return ml_string_format("<$%d>", ml_integer_value(Var->Name));
	} else {
		return ml_string_format("<$%s>", ml_string_value(Var->Name));
	}
}

ML_FUNCTION(XEParseString) {
	ML_CHECK_ARG_COUNT(1);
	ML_CHECK_ARG_TYPE(0, MLStringT);
	xe_stream_t Stream[1];
	Stream->Data = (void *)ml_string_value(Args[0]);
	Stream->LineNo = 1;
	Stream->Source = "string";
	Stream->read = string_read;
	const char *Next = string_read(Stream);
	SKIP_WHITESPACE;
	if (Next[0] != '<') return ml_error("ParseError", "Node must begin with <");
	Stream->Next = Next + 1;
	return parse_node(Stream);
}

ML_FUNCTION(XEParseFile) {
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
	SKIP_WHITESPACE;
	if (Next[0] != '<') return ml_error("ParseError", "Node must begin with <");
	Stream->Next = Next + 1;
	return parse_node(Stream);
}

ML_FUNCTION(XEExpand) {
	ML_CHECK_ARG_COUNT(1);
	xe_scope_t *Scope = GlobalScope;
	if (Count > 1) {
		ML_CHECK_ARG_TYPE(1, XEScopeT);
		Scope = (xe_scope_t *)Args[1];
	}
	return node_expand(Args[0], NULL, GlobalScope);
}

ML_FUNCTIONX(XENode) {
	ML_CHECKX_ARG_COUNT(3);
	ML_CHECKX_ARG_TYPE(0, MLStringT);
	ML_CHECKX_ARG_TYPE(1, MLMapT);
	ML_CHECKX_ARG_TYPE(2, MLListT);
	xe_node_t *Node = new(xe_node_t);
	Node->Type = XENodeT;
	Node->Tag = Args[0];
	Node->Attributes = Args[1];
	Node->Content = Args[2];
	Node->Source = ml_debugger_source(Caller);
	ML_RETURN(Node);
}

ML_FUNCTION(XEEval) {
	ML_CHECK_ARG_COUNT(3);
	ML_CHECK_ARG_TYPE(1, MLMapT);
	ML_CHECK_ARG_TYPE(2, MLListT);
	xe_scope_t *Scope = GlobalScope;
	if (Count > 3) {
		ML_CHECK_ARG_TYPE(1, XEScopeT);
		Scope = (xe_scope_t *)Args[3];
	}
	return node_eval(Args[0], Args[1], Args[2], Scope);
}

ML_FUNCTION(XEAppend) {
	ML_CHECK_ARG_COUNT(2);
	ML_CHECK_ARG_TYPE(0, MLListT);
	node_append(Args[0], Args[1]);
	return Args[0];
}

ML_METHOD("tag", XENodeT) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	return Node->Tag;
}

ML_METHOD("attributes", XENodeT) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	return Node->Attributes;
}

ML_METHOD("content", XENodeT) {
	xe_node_t *Node = (xe_node_t *)Args[0];
	return Node->Content;
}

static ml_value_t *print(void *Data, int Count, ml_value_t **Args) {
	for (int I = 0; I < Count; ++I) {
		ml_value_t *Result = Args[I];
		if (Result->Type != MLStringT) {
			Result = ml_call(MLStringOfMethod, 1, &Result);
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

static ml_value_t *MainArgs[1];

static void ml_loaded_run(ml_state_t *State, ml_value_t *Result) {
	if (Result->Type == MLErrorT) {
		printf("Error: %s\n", ml_error_message(Result));
		const char *Source;
		int Line;
		for (int I = 0; ml_error_trace(Result, I, &Source, &Line); ++I) printf("\t%s:%d\n", Source, Line);
		exit(1);
	}
	Result = ml_call(Result, 1, MainArgs);
	if (Result->Type == MLErrorT) {
		printf("Error: %s\n", ml_error_message(Result));
		const char *Source;
		int Line;
		for (int I = 0; ml_error_trace(Result, I, &Source, &Line); ++I) printf("\t%s:%d\n", Source, Line);
		exit(1);
	}
}

static ml_state_t MLLoadedState[1] = {{
	MLStateT, NULL, ml_loaded_run
}};

int main(int Argc, char **Argv) {
	static const char *Parameters[] = {"Args", NULL};
	ml_init();
	ml_types_init(Globals);
	ml_file_init(Globals);
	ml_object_init(Globals);
	ml_iterfns_init(Globals);
	stringmap_insert(Globals, "print", ml_function(0, print));
	stringmap_insert(Globals, "error", ml_function(0, error));
	stringmap_insert(Globals, "parse_string", XEParseString);
	stringmap_insert(Globals, "parse_file", XEParseFile);
	stringmap_insert(Globals, "expand", XEExpand);
	stringmap_insert(Globals, "node", XENode);
	stringmap_insert(Globals, "eval", XEEval);
	stringmap_insert(Globals, "append", XEAppend);
	stringmap_insert(GlobalScope->Symbols, "!function", XEFunction);
	stringmap_insert(GlobalScope->Symbols, "!define", XEDefine);
	stringmap_insert(GlobalScope->Symbols, "!in", XEIn);
	stringmap_insert(GlobalScope->Symbols, "!do", XEDo);
	stringmap_insert(GlobalScope->Symbols, "", XEDo);
	stringmap_insert(GlobalScope->Symbols, "!include", XEInclude);
	stringmap_insert(GlobalScope->Symbols, "!map", XEMap);
	stringmap_insert(GlobalScope->Symbols, "!list", XEList);
#include "minixe_init.c"
	ml_value_t *Args = ml_list();
	const char *FileName = 0;
	int Interactive = 0;
	for (int I = 1; I < Argc; ++I) {
		if (Argv[I][0] == '-') {
			switch (Argv[I][1]) {
			case 'i': Interactive = 1; break;
			}
		} else if (!FileName) {
			FileName = Argv[I];
		} else {
			ml_list_append(Args, ml_string(Argv[I], -1));
		}
	}
	MainArgs[0] = Args;
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
			if (Result->Type == XENodeT) Result = node_expand(Result, NULL, GlobalScope);
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
		ml_load(MLLoadedState, global_get, Globals, FileName, Parameters);
	} else {
		ml_console(global_get, Globals, "--> ", "... ");
	}
	return 0;
}
