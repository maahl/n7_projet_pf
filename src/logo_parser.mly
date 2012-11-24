
%{

  open Logo_types;;

%}

%token MOVE JUMP ROTATE CALL DEF BEGIN END IF THEN ELSE REPEAT VIRG COLOR
%token EQU INFEQ SUPEQ INF SUP AND OR NOT
%token PLUS MOINS
%token MULT DIV
%token PARO PARF COSINUS SINUS TANGENTE
%token FIN
%token <float> NUM
%token <string> IDENT
%left PLUS MOINS OR
%left MULT DIV AND
%right NOT COSINUS SINUS TANGENTE
%nonassoc UNAIRE
%type  <Logo_types.mot list> parse
%start parse
%%

expression:
  PARO expression PARF             { $2 }
| PLUS expression  %prec UNAIRE    { $2 }
| MOINS expression %prec UNAIRE    { Moins (Const(0.0), $2) }
| expression PLUS  expression      { Plus  ($1, $3) }
| expression MOINS expression      { Moins ($1, $3) }
| expression MULT  expression      { Mult  ($1, $3) }
| expression DIV   expression      { Div   ($1, $3) }
| NUM                              { Const $1 }
| IDENT				   { Var $1}
| COSINUS expression  %prec UNAIRE { Cosinus $2 }
| SINUS expression    %prec UNAIRE { Sinus $2 }
| TANGENTE expression %prec UNAIRE { Tangente $2 }
;

condition:
  PARO cond PARF                  { $2 }
;

cond:
  expression EQU expression       { Equal ($1, $3) }
| expression SUP expression       { InfEq (Plus(Const(1.0), $3), $1) }
| expression INF expression       { InfEq (Plus(Const(1.0), $1), $3) }
| expression SUPEQ expression     { InfEq ($3, $1) }
| expression INFEQ expression     { InfEq ($1, $3) }
| cond AND cond                   { And ($1, $3) }
| cond OR  cond                   { Or  ($1, $3) }
| NOT cond                        { Not $2 }
| PARO cond PARF                  { $2 }
;

liste_expr: 
 PARO PARF         		{ [] }
| PARO expression suite_expr	{ (EXPR $2)::$3 }
;

suite_expr:
  PARF                  	{ [] }
| VIRG expression suite_expr	{ (EXPR $2)::$3 }
;

liste_ident: 
 PARO PARF         		{ [] }
| PARO IDENT suite_ident 	{ (IDENT $2)::$3 }
;

suite_ident:
  PARF                  	{ [] }
| VIRG IDENT suite_ident 	{ (IDENT $2)::$3 }
;

parse :
| defs bloc			{$1@$2}
;

defs :
| def defs			{$1@$2}
|				{[]}
;

def :
| DEF IDENT liste_ident bloc	{[DEF;(IDENT $2)]@$3@$4}
;

bloc :
| BEGIN instructions END 	{BEGIN::$2@[END]}
;

instructions :
| instruction instructions	{$1@$2}
| 				{[]}
;

instruction :
| ROTATE expression		{[ROTATE;EXPR $2]}
| MOVE expression		{[MOVE;EXPR $2]}
| JUMP expression		{[JUMP;EXPR $2]}
| COLOR liste_expr			{COLOR::$2}
| bloc				{$1}
| CALL IDENT liste_expr		{[CALL; IDENT $2]@$3}
| IF condition THEN bloc ELSE bloc	{[IF;TEST $2;THEN]@$4@[ELSE]@$6}
| REPEAT expression bloc	{[REPEAT;EXPR $2]@$3}
;



%%
