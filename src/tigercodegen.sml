structure tigercodegen :> tigercodegen =
struct

open tigerframe
open tigertree
open tigerassem
open tigertemp
open tigerutils

fun salto EQ = "je" 
| salto NE = "jne"
| salto LT = "jl"
| salto GE = "jge"
| salto GT = "jg"
| salto LE = "jle"

(*
val codegen: tigerframe.frame -> tigertree.stm -> tigerassem.instr list
*)
fun codegen frame stm = 
	let val ilist = ref ([]:(instr list)) (*lista de instrucciones que va a ir mutando*)
        fun emit x = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        fun result gen = let val t = tigertemp.newtemp() in (gen t; t) end
        fun munchExp (CONST i) = result (fn r => emit(OPER{assem = "mov %d0, "^(toString i), src = [], dst = [r], jump = NONE}))
        | munchExp (NAME lab) = result (fn r => emit(OPER{assem = "mov %d0, "^(makeString lab), src = [], dst = [r], jump = NONE})) (*se podria retornar lab directo pero por las dudas*)
        | munchExp (MEM m) = result (fn r => emit(OPER{assem = "mov %d0, %s0", src =[munchExp m] , dst=[r], jump=NONE}))
        | munchExp (TEMP t) = t
        | munchExp (CALL _) = raise Fail "CALL no debería aparecer luego de canonizar 234235"
        | munchExp (ESEQ _) = raise Fail "ESEQ no debería aparecer luego de canonizar 3453453"
        | munchExp (BINOP (PLUS, CONST i, e1)) = result ( fn r => (emit(MOV{assem = "mov %d0, %s0", src = munchExp e1, dst=r}); emit(OPER{assem = "add %d0, "^(toString i), src = [r], dst = [r], jump = NONE})))
        | munchExp (BINOP (PLUS, e1, CONST i)) = munchExp (BINOP (PLUS, CONST i, e1))
        | munchExp (BINOP (PLUS, e1, e2)) = result ( fn r => (emit(MOV{assem = "mov %d0, %s0", src=munchExp e1, dst=r}); emit(OPER{assem = "add %d0, %s1", src = [r, munchExp e2], dst = [r], jump = NONE})))
        | munchExp (BINOP (MINUS, CONST i, e1)) = result ( fn r => (emit(OPER{assem = "mov %d0, "^(toString i), src = [], dst=[r], jump=NONE}); emit(OPER{assem = "sub %d0, %s1", src = [r, munchExp e1], dst = [r], jump = NONE})))
        | munchExp (BINOP (MINUS, e1, CONST i)) = result ( fn r => (emit(MOV{assem = "mov %d0, %s0", src = munchExp e1, dst=r}); emit(OPER{assem = "sub %d0, "^(toString i), src = [r], dst = [r], jump = NONE})))
        | munchExp (BINOP (MINUS, e1, e2)) = result ( fn r => (emit(MOV{assem = "mov %d0, %s0", src=munchExp e1, dst=r}); emit(OPER{assem = "sub %d0, %s1", src = [r, munchExp e2], dst = [r], jump = NONE})))
        | munchExp (BINOP (MUL, CONST i, e1)) = munchExp (BINOP (MUL, e1, CONST i))
        | munchExp (BINOP (MUL, e1, CONST i)) = result ( fn r => (emit(OPER{assem = "imul %d0, %s0, "^(toString(i)), src=[munchExp e1], dst=[r], jump=NONE})))
        | munchExp (BINOP (MUL, e1, e2)) = result ( fn r => (emit(OPER{assem = "imul %d0, %s0, %s1", src=[munchExp e1, munchExp e2], dst=[r], jump=NONE})))
        | munchExp (BINOP (DIV, CONST i, e1)) = result ( fn r => (
			let val m1 = munchExp e1
				val _ = emit(OPER{assem = "xor %d0, %s0", src=["edx"], dst=["edx"], jump = NONE})
				val _ = emit(OPER{assem = "mov %d0, "^(toString i), src=[], dst=["eax"], jump = NONE})
				val _ = emit(OPER{assem = "idiv %s1", src = ["eax", "edx", m1], dst = ["eax", "edx"], jump = NONE})
				val _ = emit(MOV{assem = "mov %d0, %s0", src="eax", dst=r})
			in
				()
			end))
        | munchExp (BINOP (DIV, e1, CONST i)) = result ( fn r => (
			let val m1 = munchExp e1
				val _ = emit(OPER{assem = "xor %d0, %s0", src=["edx"], dst=["edx"], jump = NONE})
				val _ = emit(OPER{assem = "mov %d0, %s0", src=[m1], dst=["eax"], jump = NONE})
				val _ = emit(OPER{assem = "idiv "^(toString i), src = ["eax", "edx"], dst = ["eax", "edx"], jump = NONE})
				val _ = emit(MOV{assem = "mov %d0, %s0", src="eax", dst=r})
			in
				()
			end))
		| munchExp (BINOP (DIV, e1, e2)) = result ( fn r => (
			let val m1 = munchExp e1
				val m2 = munchExp e2
				val _ = emit(OPER{assem = "xor %d0, %s0", src=["edx"], dst=["edx"], jump = NONE})
				val _ = emit(OPER{assem = "mov %d0, %s0", src=[m1], dst=["eax"], jump = NONE})
				val _ = emit(OPER{assem = "idiv %s1", src = ["eax", "edx", m2], dst = ["eax", "edx"], jump = NONE})
				val _ = emit(MOV{assem = "mov %d0, %s0", src="eax", dst=r})
			in
				()
			end))
		| munchExp exp = raise Fail "Casos no cubiertos en tigercodegen.munchExp"
	    and munchStm (SEQ (a,b)) = (munchStm a; munchStm b)
	    | munchStm (MOVE(MEM e1, e2)) = emit (MOV{assem="mov %d0, %s0", src=munchExp e2, dst=munchExp e1})
	    | munchStm (MOVE(TEMP l, e)) = emit (MOV{assem="mov %d0, %s0", src=munchExp e, dst=l})
	    | munchStm (LABEL lab) = emit (aLABEL{assem = (makeString lab) ^ ":", lab=lab })
        | munchStm (JUMP (NAME l, [lp])) = if l <> lp then raise Fail "jump que no salta al nombre de su etiqueta 23987\n" else 
            emit(OPER{assem="jmp %j0", src=[], dst=[], jump=SOME [l]})
        | munchStm (JUMP _) = raise Fail "jump invalido 98798\n"
        | munchStm (CJUMP (rop, e1, e2, l1, l2)) =
(*ojo que tal vez el cmp tiene los argumentos al reves*)
			(emit(OPER{assem = "cmp %s0, %s1", src=[munchExp e1, munchExp e2], dst=[], jump=NONE});
            emit(OPER{assem = (salto rop) ^ " %j0", src = [], dst = [], jump = SOME [l1,l2]}))
        | munchStm (EXP (CALL (NAME lab,args))) =
			let val _ = emit(OPER{assem="call "^(makeString lab), src=munchArgs args, dst=tigerframe.calldefs, jump=NONE})
			    val spoffset = List.length args * tigerframe.wSz (* vamos a recuperar el sp en caso de haber hecho pushq antes del call*)
            in
				if spoffset>0
					then emit(OPER{assem = "add %d0, "^(toString spoffset), src = [tigerframe.sp], dst = [tigerframe.sp], jump = NONE})
					else ()
				end
		| munchStm (EXP e) = (munchExp e ; ())
		| munchStm stm = raise Fail "Casos no cubiertos en tigercodegen.munchStm"
	and munchArgs([]) = []
        | munchArgs(x::xs) =
			(munchArgs(xs); emit(OPER{assem = "pushq %s0", src=[munchExp x] , dst=[], jump=NONE}); [])
    in
		munchStm stm;
		rev(!ilist)
	end

(*
codegens : tigerframe.frame -> tigertree.stm list -> tigerassem.instr list
*)
fun codegens frame stms = flatten (map (fn b => codegen frame b) stms)

end
