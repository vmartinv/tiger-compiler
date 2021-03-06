structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs
open tigerutils

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
        frame=newFrame{
            name=name,
            formals=(true::formals)  (*true x static link*)
        },
        level=level+1
    }
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

fun printExp (Ex e) = tigerit.tree(EXP e)
    | printExp (Nx s) = tigerit.tree(s)
    | printExp _ = raise Fail "No debe ocurrir 3324435\n"

fun nombreFrame frame = ";;-PROC-" ^ tigerframe.name frame ^ "--:\n"
fun Ir(e) =
    let fun aux2(PROC{body, frame}) = nombreFrame frame ^printExp(Nx body)^";;-END-PROC--:\n"
        | aux2(STRING(l, s)) = tigerassem.formatString (l, s)
    in concat (map aux2 e) end

(* While y for necesitan la u'ltima etiqueta para un break *)
local
    val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
    val pushSalida = tigerpila.push salidas
    fun popSalida() = tigerpila.pop salidas
    fun topSalida() =
        case tigerpila.top salidas of
        SOME l => l
        | NONE => raise Fail "break incorrecto!"            
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
    let 
        val body' = PROC{frame= #frame level, body=unNx body}
    in  
        datosGlobs:=(!datosGlobs@[body']) 
    end
fun getResult() = !datosGlobs
fun clearResult() = datosGlobs := []

fun stringLen s =
    let 
        fun aux[] = 0
        | aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
        | aux(_::t) = 1+aux(t)
    in  
        aux(explode s) 
    end

fun stringExp(s: string) =
    let 
        val l = newlabel()
        val _ = datosGlobs:=(!datosGlobs @ [STRING(l, s)])
    in  
        Ex(NAME l) 
    end
fun preFunctionDec() =
    (pushSalida(NONE);
    actualLevel := !actualLevel+1)
fun functionDec(e, l, proc) =
    let 
        val body =
                if proc then unNx e
                else MOVE(TEMP rv, unEx e)
        val body' = procEntryExit1(#frame l, body)
        val () = procEntryExit{body=Nx body', level=l}
    in
        Nx body' 
    end (*COMPLETAR_EXP*)
fun postFunctionDec() =
    (popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun trepar 0 = TEMP fp 
  | trepar n = MEM(BINOP(PLUS, CONST fpPrevLev, trepar(n-1)))
                           
fun simpleVar(acc, nivel) = (* nivel = nivel de anidamiento, puede estar en otro frame *)
    Ex (tigerframe.exp acc (trepar(getActualLev() - nivel))) (*COMPLETAR_EXP_DONE*)
    
fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = 
let
    val var' = unEx var
    val t = newtemp()
in    
    Ex( ESEQ(seq[MOVE(TEMP t, var'),
        EXP(externalCall("_checkNil", [TEMP t]))],
        MEM(BINOP(PLUS, TEMP t, CONST (tigerframe.wSz*field))))) (*COMPLETAR_EXP_DONE*)
end

fun subscriptVar(arr, ind) =
let
    val a = unEx arr
    val i = unEx ind
    val ra = newtemp()
    val ri = newtemp()
in
    Ex( ESEQ(seq[MOVE(TEMP ra, a),
        MOVE(TEMP ri, i),
        EXP(externalCall("_checkIndexArray", [TEMP ra, TEMP ri]))],
        MEM(BINOP(PLUS, TEMP ra,
            BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

fun recordExp linit =
let
    fun first (x,y) = x
    fun comp ((_,a), (_,b)) = Int.compare (a,b)
    val slinit = Listsort.sort comp linit
    val linit' = map (unEx o first) slinit    
in
    Ex (externalCall("_allocRecord", [CONST (length(linit'))]@linit')) (*COMPLETAR_EXP_DONE*)
end

fun arrayExp{size, init} =
let
    val s = unEx size
    val i = unEx init
in
    Ex (externalCall("_initArray", [s, i]))
end

(*  name     = etiqueta (nombre de la funcion)
    ext = si es externa, en este caso no esta esperando un sl
    isproc   = si retorna algo
    lev      = nivel de anidamiento
    params   = lista de argumentos
    *)
fun callExp (name,extern:bool,isproc,lev:level,params) = 
let val name'   = NAME name
    val static_link = trepar(getActualLev() - #level lev + 1)
    val params' =  if (not extern) then static_link::(map unEx params) else (map unEx params)
    val tmps = map (fn _ => TEMP (newtemp())) params'
    val moves = map MOVE (ListPair.zip(tmps, params'))   (*Mueven los argumentos a temporarios*)
    val rt = TEMP (newtemp())
in
    if isproc then Nx(seq(moves@[EXP(CALL(name', tmps))]))
    else Ex(ESEQ(seq(moves@[EXP(CALL(name',tmps)), MOVE(rt, TEMP rv)]), rt) )
end (*COMPLETAR_EXP*)

fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

fun breakExp() = 
    Nx (JUMP (NAME (topSalida()), [topSalida()])) (*COMPLETAR_EXP_DONE*)

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
    | seqExp ([Nx e]:exp list) = Nx e
    | seqExp ([Ex e]:exp list) = Ex e
    | seqExp ([cond]:exp list) = Ex (unEx cond)
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

(*  if (lo <= hi) {//ifi
        linit:
        var = lo;//MOVE(var, lo')
        lsigue:
        expb;
        if (var==hi) goto lfin;//if2
        laumenta:
        var++;
        goto lsigue;
    }
    lfin:
*)
fun forExp {lo, hi, var, body} =
let
    val lo' = unEx lo
    val hi' = unEx hi
    val var' = unEx var
    val (linit, lsigue, laumenta, lfin) = (newlabel(), newlabel(),  newlabel(), topSalida())
    val ifi = CJUMP(LE, lo', hi', linit, lfin)
    val if2 = CJUMP(EQ, var', hi', lfin, laumenta)
    val expb = unNx body
in
    Nx (seq[ifi, LABEL linit,
        MOVE(var', lo'),
        LABEL lsigue,
        expb,
        if2,
        LABEL laumenta, MOVE(var', BINOP(PLUS, var', CONST 1)),
        JUMP(NAME lsigue, [lsigue]),
        LABEL lfin])
end (*COMPLETAR_EXP_DONE*)

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
