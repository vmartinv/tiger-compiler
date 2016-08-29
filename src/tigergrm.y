%{
open tigerabs

fun P() = !tigernlin.num_linea

fun nombre(SimpleVar s) = s
| nombre _ = raise Fail "Imposible que no sea SimpleVar!"

fun fundeLFunTipos(TypeDec[dt], (TypeDec(hdt))::t) =
	TypeDec(dt::hdt)::t
| fundeLFunTipos(FunctionDec[dt], (FunctionDec(hdt))::t) =
	FunctionDec(dt::hdt)::t
| fundeLFunTipos(d1, dl) =
	d1::dl

%}
%token EOF
%token TYPE ARRAY OF VAR FUNCTION
%token LET IN END IF THEN ELSE WHILE DO FOR TO BREAK
%token PTO DOSP DOSPIG COMA PCOMA IGUAL PI PD CI CD LI LD
%token AMPER PIPE MENOR MENIG MAYOR MAYIG DIST
%token MAS MENOS POR DIV MENOS NIL
%token<int> NRO
%token<string> LITERAL ID

%type<tigerabs.exp> prog
%type<tigerabs.ty> ty
%type<string> id
%type<tigerabs.field> tyfield
%type<tigerabs.field list> tyflds
%type<tigerabs.exp> exp
%type<tigerabs.exp list> explist
%type<(tigerabs.symbol*tigerabs.exp) list> rec_fields 
%type<tigerabs.exp list> args 
%type<var> l_value
%type<tigerabs.dec> dec
%type<tigerabs.dec> vardec
%type<tigerabs.dec> fundec 
%type<tigerabs.dec list> decs 

%nonassoc THEN 
%left ELSE
%nonassoc DO
%nonassoc OF
%nonassoc DOSPIG
%left PIPE
%left AMPER
%nonassoc IGUAL MENOR MENIG MAYOR MAYIG DIST
%left MAS MENOS
%left POR DIV

%start prog
%%
prog : exp EOF				{ $1 }
	;
exp : NRO					{ IntExp($1, P()) }
	| PI PD					{ UnitExp(P()) }
	| NIL					{ NilExp(P()) }
	| LITERAL				{ StringExp($1, P()) }
	| BREAK					{ BreakExp(P()) }
	| l_value				{ VarExp($1, P()) }
	| l_value DOSPIG exp	{ AssignExp({var=$1, exp=$3}, P()) }
	| PI exp PCOMA explist PD
							{ SeqExp($2::$4, P()) }
	| exp PIPE exp			{ IfExp({test=$1, then'=IntExp(1, P()),
								else'=SOME $3}, P()) }
	| exp AMPER exp			{ IfExp({test=$1, then'=$3,
								else'=SOME(IntExp(0, P()))}, P()) }
	| exp IGUAL exp			{ OpExp({left=$1, oper=EqOp, right=$3}, P()) }
	| exp MENOR exp			{ OpExp({left=$1, oper=LtOp, right=$3}, P()) }
	| exp MENIG exp			{ OpExp({left=$1, oper=LeOp, right=$3}, P()) }
	| exp MAYOR exp			{ OpExp({left=$1, oper=GtOp, right=$3}, P()) }
	| exp MAYIG exp			{ OpExp({left=$1, oper=GeOp, right=$3}, P()) }
	| exp DIST exp			{ OpExp({left=$1, oper=NeqOp, right=$3}, P()) }
	| exp MAS exp			{ OpExp({left=$1, oper=PlusOp, right=$3}, P()) }
	| exp MENOS exp			{ OpExp({left=$1, oper=MinusOp, right=$3}, P()) }
	| exp POR exp			{ OpExp({left=$1, oper=TimesOp, right=$3}, P()) }
	| exp DIV exp			{ OpExp({left=$1, oper=DivideOp, right=$3}, P()) }
	| MENOS exp				{ OpExp({left=IntExp(0, P()),
								oper=MinusOp, right=$2}, P()) }
	| PI exp PD				{ $2 }
	| id PI args PD			{ CallExp({func=$1, args=$3}, P()) }
	| IF exp THEN exp		{ IfExp({test=$2, then'=$4, else'=NONE}, P()) }
	| IF exp THEN exp ELSE exp
							{ IfExp({test=$2, then'=$4, else'=SOME $6}, P()) }
	| WHILE exp DO exp		{ WhileExp({test=$2, body=$4}, P()) }
	| FOR id DOSPIG exp TO exp DO exp
							{ ForExp({var=$2, escape=ref false,
								lo=$4, hi=$6, body=$8}, P()) }
	| LET decs IN END		{ LetExp({decs=$2, body=UnitExp(P())}, P()) }
	| LET decs IN exp END	{ LetExp({decs=$2, body=$4}, P()) }
	| LET decs IN exp PCOMA explist END
							{ LetExp({decs=$2,
								body=SeqExp($4::$6, P())}, P()) }
	| l_value CI exp CD OF exp
							{ ArrayExp({typ=nombre $1, size=$3, init=$6}, P()) }
	| id LI rec_fields LD	{ RecordExp({fields=$3, typ=$1}, P()) }
	;
explist: exp PCOMA explist	{ $1::$3 }
	| exp					{ [$1] }
	;
rec_fields : id IGUAL exp COMA rec_fields
							{ ($1, $3)::$5 }
	| id IGUAL exp			{ [($1, $3)] }
	|						{ [] }	
	;
decs : dec decs				{ fundeLFunTipos($1, $2) }
	|						{ [] }
	;
dec : TYPE id IGUAL ty		{ TypeDec[({name=$2, ty=$4}, P())] }
	| vardec				{ $1 }
	| fundec				{ $1 }
	;
ty : id						{ NameTy $1 }		
	| LI tyflds LD			{ RecordTy $2 }
	| ARRAY OF id			{ ArrayTy $3 }
	;
id : ID						{ $1 }
	;
tyflds : tyfield COMA tyflds
							{ $1::$3 }
	| tyfield				{ [$1] }
	|						{ [] }
	;
vardec : VAR id DOSPIG exp	{ VarDec({name=$2, escape=ref false,
								typ=NONE, init=$4}, P()) }
	| VAR id DOSP id DOSPIG exp
							{ VarDec({name=$2, escape=ref false,
								typ=SOME $4, init=$6}, P()) }
	;
fundec : FUNCTION id PI tyflds PD IGUAL exp
							{ FunctionDec[({name=$2, params=$4,
								result=NONE, body=$7}, P())] }
	| FUNCTION id PI tyflds PD DOSP id IGUAL exp
							{ FunctionDec[({name=$2, params=$4,
								result=SOME $7, body=$9}, P())] }
	;
tyfield : id DOSP id		{ {escape=ref false, name=$1, typ=NameTy $3} }
	;
args : exp COMA args		{ $1::$3 }
	| exp					{ [$1] }
	|						{ [] }
	;
l_value : id				{ SimpleVar $1 }
	| l_value PTO id		{ FieldVar($1, $3) }
	| l_value CI exp CD		{ SubscriptVar($1, $3) }
	;
%%
