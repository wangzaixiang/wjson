%language "Java"
%define api.package {wjson.parser}
%define api.parser.class {Program}
%define api.parser.public

%define parse.error verbose

%code imports {
 import static wjson.patterns.ast.Tree.*;
 import java.util.*;
}
%code {
  public PatternWithName root;
}

%type <PatternWithName> program bind_jsval
%type <Pattern> jsval
%type <List<PatternWithName>> opt_arr_items arr_items
%type <Map<String, PatternWithName>> opt_fields fields
%type <Field> field
%type <String> opt_bind ID LiteralString path
%type <String[]> TagedString
%type <double> LiteralNumber

%token ID NULL TRUE FALSE LiteralString LiteralNumber STRING NUMBER BOOLEAN INTEGER Any1 Anys TagedString

%%

program		: bind_jsval { $$ = $1; root = $1; }
            ;

bind_jsval 	: opt_bind jsval { $$ = $2.bind($1);}
            ;

opt_bind	:  { $$ = null; }
		| ID '@' { $$ = $1;}
		;

jsval	: NULL { $$ = new NullPattern(); }
		| TRUE { $$ = new BooleanPattern(true); }
		| FALSE { $$ = new BooleanPattern(false); }
		| LiteralString { $$ = new StringPattern($1); }
		| LiteralNumber { $$ = new NumberPattern($1); }
		| STRING { $$ = new AnyVal("string"); }
		| NUMBER { $$ = new AnyVal("number"); }
		| BOOLEAN { $$ = new AnyVal("boolean"); }
		| INTEGER { $$ = new AnyVal("integer"); }
		| Any1 { $$ = new AnyVal("any"); }
		| Anys { $$ = new AnyVals(); }
		| TagedString { $$ = new TagedString($1[0], $1[1]); }
		| '[' opt_arr_items ']' { $$ = new ArrayPattern($2); }
		| '(' opt_arr_items ')' { $$ = new ArrayPattern($2); }
		| '{' opt_fields '}' { $$ = new ObjPattern($2); }
		;

opt_arr_items 	:  { $$ = new ArrayList<PatternWithName>(); }
		| 	arr_items { $$ = $1; }
		;

arr_items	: 	bind_jsval { $$ = Arrays.asList($1); }
		|	arr_items ',' bind_jsval { $$ = append($1, $3); }
		;

opt_fields	:	{ $$ = new HashMap<String, PatternWithName>(); }
		|	fields { $$ = $1; }
		|   fields ',' { $$ = $1; }
		;

fields		:	field { $$ = append( new HashMap<String, PatternWithName>(), $1.name, $1.pattern);  }
		|	fields ',' field { $$ = append($1, $3.name, $3.pattern); }
		;

field		:	opt_bind path { $$ = new Field($2, new PatternWithName($1, new AnyVal("any")) ); }
		|	path ':' opt_bind jsval { $$ = new Field($1, new PatternWithName($3, $4)); }
		|   opt_bind Anys { $$ = new Field("_*", new PatternWithName($1, new AnyVals())); }
		;	

path		:	ID { $$ = $1; }
		|	LiteralString { $$ = $1; }
		|	path '/' ID { $$ = $1 + "/" + $3; }
		|	path '/' LiteralString { $$ = $1 + "/" + $3; }
		;
