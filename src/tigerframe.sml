(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

val rax = "rax"
val rdx = "rdx"
val fp =  "rbp"				(* frame pointer  (ebp en el 386) *)
val sp =  "rsp"				(* stack pointer  (esp en el 386) *)
val rv =  "rax"			 	(* return value   (eax en el 386) *)
val ov =  "rdx"				(* overflow value (edx en el 386) *)

val wSz = 8					(* word size in bytes *)
val log2WSz = 3				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = ~wSz		(* offset (bytes) *)
(*
val argsInicial = 0			(* words *)
val argsOffInicial = 0		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *)
val localsInicial = 0		(* words *)
*)
val localsGap = ~wSz 			(* bytes *)
val calldefs = [rv]
val specialregs = [fp, sp]
val argregs = ["rdi","rsi","rdx","rcx","r8","r9"] (* registros donde van los primeros argumentos segun la convención de llamada *)
val callersaves = ["rax","rdx","rcx","r10","r11"] @ argregs (* registros preservador por el invocador *) (* DUDA: En el libro pg 208 dice que deberia ser disjunto con argregs *)
val calleesaves = ["rbx","r12","r13","r14","r15"] (* registros preservador por la funcion invocada *)
val calldefs = callersaves @ [rv]
val coloredregisters = [](*COMPLETAR*)
(*En lo de Mariano
val coloredregisters = callersaves @ calleesaves
Ya lo de arriba lo modifique por si necesito algun regintro en codegen*)

type register = string
datatype access = InFrame of int | InReg of tigertemp.label      (* Describe args y vars locales que pueden estar en el marco o en registros *)
                                                                 (* InFrame(n) indica que se corresponde con una locacion de mem con offset n desde el fp *)
                                                                 (* InReg(t) indica que se corresponde con el registro t *)
type frame = {
	name: string, (* nombre de la función a la que pertenece - Se necesita para construir un frame *)
	formals: bool list, (* true si el parametro escapa, falso sino - Se necesita para construir un frame *)
	argsAcc: access list ref, (*lista de argumentos*)
	cantLocalsInFrame: int ref (*offset en words del proximo potencial local*)
}
datatype frag = PROC of {body: tigertree.stm, frame: frame}      (*  *)
	| STRING of tigertemp.label * string

fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame( (!(#cantLocalsInFrame f)+1) * localsGap)
		in	#cantLocalsInFrame f:=(!(#cantLocalsInFrame f)+1); ret end
	| false => InReg(tigertemp.newtemp())

fun newFrame{name, formals} =
	let val f = { 
					name=name,
				    formals=formals,
				    argsAcc = ref ([]:access list),
				    cantLocalsInFrame=ref 0
				}
		 val _ = #argsAcc f := List.map (fn b => allocLocal f b) formals
	 in f end
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"

(* old formals function *)
fun formals({argsAcc, ...}: frame) = !argsAcc
fun exp(InFrame k) efp = MEM(BINOP(PLUS, efp, CONST k))
| exp(InReg l) e = TEMP l
fun offset(InFrame k) = k
| offset(InReg l) = raise Fail "Ooops"

fun externalCall(s, l) = let val raux = tigertemp.newtemp() in ESEQ(SEQ(EXP(CALL(NAME s, l)),MOVE(TEMP raux,TEMP rv)),TEMP raux) end


fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)

fun procEntryExit1 ( fr : frame,body) = 
	body
(*
   let val argsAcc = #argsAcc fr
       fun aux [] _ = []
       |   aux (acc::accs) n = MOVE( exp acc (TEMP fp), if n < List.length argregs then TEMP (List.nth(argregs,n)) else MEM(BINOP(PLUS, CONST ((n-List.length argregs)*8+16), TEMP fp)) ) :: aux accs (n+1)
       val moveargs = aux (!argsAcc) 0 (*Instrucciones para mover de los argumentos a los locals donde la función ve internamente las cosas *)
       val freshtmps = List.tabulate (List.length calleesaves , fn _ => TEMP (tigertemp.newtemp()))
       val saveregs = List.map MOVE (ListPair.zip(freshtmps,List.map TEMP calleesaves)) (* Instrucciones para salvar en temporarios los callee saves *)
       val restoreregs = List.map MOVE(ListPair.zip(List.map TEMP calleesaves,freshtmps)) (* Restaurar los callee saves *)
       in seq( saveregs @ moveargs @ [body] @ restoreregs ) end   
*)
fun procEntryExit2(frame:frame,instrs) = instrs @ [tigerassem.OPER{assem="",src=[rv,sp,fp]@calleesaves, dst=[], jump=NONE}]

end
