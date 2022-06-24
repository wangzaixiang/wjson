%language "Java"
%define api.package {rejson.parser}
%define api.parser.class {Program}
%define api.parser.public

%define parse.error verbose

%code imports {
 import static rejson.ast.Tree.*;
 import java.util.*;
}
%code {
  public JsValWithId root;
}

%type <JsValWithId> rejson jsval jsval1
%type <List<JsValWithId>> opt_arr_items arr_items
%type <Map<String, JsValWithId>> opt_fields fields field
%type <String> opt_bind ID LiteralString path StringContext
%type <double> LiteralNumber

%token ID NULL TRUE FALSE LiteralString LiteralNumber STRING NUMBER BOOLEAN INTEGER Any1 Anys StringContext

%%

rejson 		: jsval { $$ = $1; root = $1; };

jsval 		: opt_bind jsval1 { $$ = $2.identity($1);};

opt_bind	:  { $$ = null; }
		| ID '@' { $$ = $1;};

jsval1		: NULL { $$ = new JsValWithId(null, new JsNull()); }
		| TRUE { $$ = new JsValWithId(null, new JsBoolean(true)); }
		| FALSE { $$ = new JsValWithId(null, new JsBoolean(false)); }
		| LiteralString { $$ = new JsValWithId(null, new JsString($1)); }
		| LiteralNumber { $$ = new JsValWithId(null, new JsNumber($1)); }
		| STRING { $$ = new JsValWithId(null, new JsAny("string")); }
		| NUMBER { $$ = new JsValWithId(null, new JsAny("number")); }
		| BOOLEAN { $$ = new JsValWithId(null, new JsAny("boolean")); }
		| INTEGER { $$ = new JsValWithId(null, new JsAny("integer")); }
		| Any1 { $$ = new JsValWithId(null, new JsAny("any")); }
		| Anys { $$ = new JsValWithId(null, new JsAny("anys")); }
		| StringContext { $$ = new JsValWithId(null, new JsString($1)); }
		| '[' opt_arr_items ']' { $$ = new JsValWithId(null, new JsArr($2)); }
		| '(' opt_arr_items ')' { $$ = new JsValWithId(null, new JsArr($2)); }
		| '{' opt_fields '}' { $$ = new JsValWithId(null, new JsObj($2)); }
		;

opt_arr_items 	:  { $$ = new ArrayList<JsValWithId>(); }
		| 	arr_items { $$ = $1; };

arr_items	: 	jsval { $$ = Arrays.asList($1); }
		|	arr_items ',' jsval { $$ = append($1, $3); };

opt_fields	:	{ $$ = new HashMap<String, JsValWithId>(); }
		|	fields { $$ = $1; };

fields		:	field { $$ = $1; }
		|	fields ',' field { $$ = append($1, $3); }
		;

field		:	opt_bind path { $$ = append( new HashMap<String, JsValWithId>(), $2, new JsValWithId($1, new JsAny("any"))); }
		|	path ':' opt_bind jsval1 { $$ = append( new HashMap<String, JsValWithId>(), $1, $4.identity($3)); }
		|   Any1 { $$ = new HashMap<String,JsValWithId>(); }
		;	

path		:	ID { $$ = $1; }
		|	LiteralString { $$ = $1; }
		|	path '/' ID { $$ = $1 + "/" + $3; }
		|	path '/' LiteralString { $$ = $1 + "/" + $3; }
		;
