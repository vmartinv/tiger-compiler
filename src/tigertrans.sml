structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
	
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
	frame=newFrame{name="_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
	{
	parent=SOME frame,
	frame=newFrame{name=name, formals=formals},
	level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tigerframe.formals frame

datatype exp =
	Ex of tigertree.exp
	| Nx of tigertree.stm
	| Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
	| unEx (Nx s) = ESEQ(s, CONST 0)
	| unEx (Cx cf) =
	let
		val r = newtemp()
		val t = newlabel()
		val f = newlabel()
	in
		ESEQ(seq [MOVE(TEMP r, CONST 1),
			cf (t, f),
			LABEL f,
			MOVE(TEMP r, CONST 0),
			LABEL t],
			TEMP r)
	end

fun unNx (Ex e) = EXP e
	| unNx (Nx s) = s
	| unNx (Cx cf) =
	let
		val t = newlabel()
		val f = newlabel()
	in
		seq [cf(t,f),
			LABEL t,
			LABEL f]
	end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
	| unCx (Cx cf) = cf
	| unCx (Ex (CONST 0)) =
	(fn (t,f) => JUMP(NAME f, [f]))
	| unCx (Ex (CONST _)) =
	(fn (t,f) => JUMP(NAME t, [t]))
	| unCx (Ex e) =
	(fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
	let	fun aux(Ex e) = tigerit.tree(EXP e)
		| aux(Nx s) = tigerit.tree(s)
		| aux _ = raise Fail "bueno, a COMPLETAR_EXP!"
		fun aux2(PROC{body, frame}) = aux(Nx body)
		| aux2(STRING(l, "")) = l^":\n"
		| aux2(STRING("", s)) = "\t"^s^"\n"
		| aux2(STRING(l, s)) = l^":\t"^s^"\n"
		fun aux3 [] = ""
		| aux3(h::t) = (aux2 h)^(aux3 t)
	in	aux3 e end
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
	val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
	val pushSalida = tigerpila.pushPila salidas
	fun popSalida() = tigerpila.popPila salidas
	fun topSalida() =
		case tigerpila.topPila salidas of
		SOME l => l
		| NONE => raise Fail "break incorrecto!"			
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
	let	val label = STRING(name(#frame level), "")
		val body' = PROC{frame= #frame level, body=unNx body}
		val final = STRING(";;-------", "")
	in	datosGlobs:=(!datosGlobs@[label, body', final]) end
fun getResult() = !datosGlobs
fun clearResult() = datosGlobs := []

fun stringLen s =
	let	fun aux[] = 0
		| aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
		| aux(_::t) = 1+aux(t)
	in	aux(explode s) end

fun stringExp(s: string) =
	let	val l = newlabel()
		val len = ".long "^makestring(stringLen s)
		val str = ".string \""^s^"\""
		val _ = datosGlobs:=(!datosGlobs @ [STRING(l, len), STRING("", str)])
	in	Ex(NAME l) end
fun preFunctionDec() =
	(pushSalida(NONE);
	actualLevel := !actualLevel+1)
fun functionDec(e, l, proc) =
	let	val body =
				if proc then unNx e
				else MOVE(TEMP rv, unEx e)
		val body' = procEntryExit1(#frame l, body)
		val () = procEntryExit{body=Nx body', level=l}
	in Nx body' end (*COMPLETAR_EXP*)
fun postFunctionDec() =
	(popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun auxDiff 0 = TEMP fp
  | auxDiff n = MEM (BINOP (PLUS, CONST fpPrev, auxDiff(n-1)))

fun simpleVar(acc, nivel) = (* nivel = nivel de anidamiento, puede estar en otro frame *)
	Ex (tigerframe.exp acc (auxDiff(getActualLev() - nivel))) (*COMPLETAR_EXP_DONE*)
    
fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = 
	Ex (CONST 0) (*COMPLETAR_EXP*)

fun subscriptVar(arr, ind) =
let
	val a = unEx arr
	val i = unEx ind
	val ra = newtemp()
	val ri = newtemp()
in
	Ex( ESEQ(seq[MOVE(TEMP ra, a),
		MOVE(TEMP ri, i),
		EXP(externalCall("_checkindex", [TEMP ra, TEMP ri]))],
		MEM(BINOP(PLUS, TEMP ra,
			BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

fun recordExp l =
	Ex (CONST 0) (*COMPLETAR_EXP*)

fun arrayExp{size, init} =
let
	val s = unEx size
	val i = unEx init
in
	Ex (externalCall("allocArray", [s, i]))
end

fun callExp (name,external,isproc,lev:level,ls) = 
	Ex (CONST 0) (*COMPLETAR_EXP*)

fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

fun breakExp() = 
	Ex (CONST 0) (*COMPLETAR_EXP*)

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
	| seqExp (exps:exp list) =
		let
			fun unx [e] = []
				| unx (s::ss) = (unNx s)::(unx ss)
				| unx[] = []
		in
			case List.last exps of
				Nx s =>
					let val unexps = map unNx exps
					in Nx (seq unexps) end
				| Ex e => Ex (ESEQ(seq(unx exps), e))
				| cond => Ex (ESEQ(seq(unx exps), unEx cond))
		end

fun preWhileForExp() = pushSalida(SOME(newlabel()))

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
	val cf = unCx test
	val expb = unNx body
	val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
	Nx (seq[LABEL l1,
		cf(l2,l3),
		LABEL l2,
		expb,
		JUMP(NAME l1, [l1]),
		LABEL l3])
end

fun forExp {lo, hi, var, body} =
	Ex (CONST 0) (*COMPLETAR_EXP*)

fun ifThenExp{test, then'} =
    let val test' = unCx test
        val t' = unNx then'
        val (lv, lf) = (newlabel(), newlabel())
    in
        Nx(seq[test'(lv, lf),
                   LABEL lv, t',
                   LABEL lf])
    end  (*COMPLETAR_EXP_DONE*)

fun ifThenElseExp {test,then',else'} = 
    let val test' = unCx test
        val t' = unEx then'
        val e' = unEx else'
        val (lv, lf, le) = (newlabel(), newlabel(), newlabel())
        val tmp = newtemp()
    in
        Ex(ESEQ(seq[test'(lv, lf),
                   LABEL lv, MOVE(TEMP tmp, t'), JUMP(NAME le, [le]),
                   LABEL lf, MOVE(TEMP tmp, e'),
                   LABEL le],
                   TEMP tmp))
    end (*COMPLETAR_EXP_DONE*)
fun ifThenElseExpUnit {test,then',else'} =
    let val test' = unCx test
        val t' = unNx then'
        val e' = unNx else'
        val (lv, lf, le) = (newlabel(), newlabel(), newlabel())
    in
        Nx(seq[test'(lv, lf),
               LABEL lv, t', JUMP(NAME le, [le]),
               LABEL lf, e',
               LABEL le])
    end (*COMPLETAR_EXP_DONE*)

fun assignExp{var, exp} =
	let
		val v = unEx var
		val vl = unEx exp
	in
		Nx (MOVE(v,vl))
	end

fun binOpIntExp {left, oper, right} = 
	let
		val l = unEx left
		val r = unEx right
	in
		case oper of
		  PlusOp   => Ex (BINOP(PLUS , l, r))
		| MinusOp  => Ex (BINOP(MINUS, l, r))
		| TimesOp  => Ex (BINOP(MUL  , l, r))
		| DivideOp => Ex (BINOP(DIV  , l, r))
	    | _ => raise Fail ("No debe ocurrir\n")
	end(*COMPLETAR_EXP_DONE*)

fun binOpIntRelExp {left,oper,right} =
	let
		val l = unEx left
		val r = unEx right
	in
	case oper of 
		  EqOp  => Cx (fn (lt,lf) => CJUMP(EQ, l, r, lt, lf) )
	    | NeqOp => Cx (fn (lt,lf) => CJUMP(NE, l, r, lt, lf) )
	    | LtOp  => Cx (fn (lt,lf) => CJUMP(LT, l, r, lt, lf) )
	    | LeOp  => Cx (fn (lt,lf) => CJUMP(LE, l, r, lt, lf) )
	    | GtOp  => Cx (fn (lt,lf) => CJUMP(GT, l, r, lt, lf) )
	    | GeOp  => Cx (fn (lt,lf) => CJUMP(GE, l, r, lt, lf) )
	    | _ => raise Fail ("No debe ocurrir\n")
    end
    (*COMPLETAR_EXP_DONE*)
fun binOpStrExp {left,oper,right} = (* aca se va a necesitar llamar a una funcion del runtime que compare strings (_stringCompare)
Obs: runtime.c es de la etapa 3*)
	let
		val l = unEx left
		val r = unEx right
		val cmp = externalCall("_stringCompare", [l, r])
	in
	case oper of 
		  EqOp  => Cx (fn (lt,lf) => CJUMP(EQ, cmp, CONST 0, lt, lf) )
	    | NeqOp => Cx (fn (lt,lf) => CJUMP(NE, cmp, CONST 0, lt, lf) )
	    | LtOp  => Cx (fn (lt,lf) => CJUMP(LT, cmp, CONST 0, lt, lf) )
	    | LeOp  => Cx (fn (lt,lf) => CJUMP(LE, cmp, CONST 0, lt, lf) )
	    | GtOp  => Cx (fn (lt,lf) => CJUMP(GT, cmp, CONST 0, lt, lf) )
	    | GeOp  => Cx (fn (lt,lf) => CJUMP(GE, cmp, CONST 0, lt, lf) )
	    | _ => raise Fail ("No debe ocurrir\n")
    end
    (*COMPLETAR_EXP_DONE*)


end
