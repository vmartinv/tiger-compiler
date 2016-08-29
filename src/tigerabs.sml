structure tigerabs = 
struct

type symbol = string
type pos = int

datatype var = SimpleVar of symbol
	| FieldVar of var * symbol
	| SubscriptVar of var * exp

and exp = VarExp of var * pos
	| UnitExp of pos
	| NilExp of pos
	| IntExp of int * pos
	| StringExp of string * pos
	| CallExp of {func: symbol, args: exp list} * pos
	| OpExp of {left: exp, oper: oper, right: exp} * pos
	| RecordExp of {fields: (symbol * exp) list, typ: symbol} * pos
	| SeqExp of exp list * pos
	| AssignExp of {var: var, exp: exp} * pos
	| IfExp of {test: exp, then': exp, else': exp option} * pos
	| WhileExp of {test: exp, body: exp} * pos
	| ForExp of {var: symbol, escape: bool ref,
		     lo: exp, hi: exp, body: exp} * pos
	| LetExp of {decs: dec list, body: exp} * pos
	| BreakExp of pos
	| ArrayExp of {typ: symbol, size: exp, init: exp} * pos

and dec = FunctionDec of ({name: symbol, params: field list,
		result: symbol option, body: exp} * pos) list
	| VarDec of {name: symbol, escape: bool ref,
		     typ: symbol option, init: exp} * pos
	| TypeDec of ({name: symbol, ty: ty} * pos) list

and ty = NameTy of symbol
	| RecordTy of field list
	| ArrayTy of symbol
and oper = PlusOp | MinusOp | TimesOp | DivideOp
	| EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, typ: ty}
end
