structure tigerit =
struct

open tigertab
open tigertree

fun tree s0 =
	let	fun say s = s
		fun sayln s = s^"\n"
		fun indent 0 = ""
		| indent i = " "^indent(i-1)
		fun stm(SEQ(a,b),d) =
			indent(d)^sayln("SEQ(")^stm(a,d+1)^
			sayln(",")^stm(b,d+1)^say(")")
		| stm(LABEL lab, d) = indent(d)^say("LABEL ")^say(lab)
		| stm(JUMP (e,_), d) =  indent(d)^say("JUMP(")^exp(e,d+1)^say(")")
		| stm(CJUMP(r,a,b,t,f),d) = indent(d)^say("CJUMP(")^
			relop(r)^sayln(",")^
			exp(a,d+1)^sayln(",")^exp(b,d+1)^sayln(",")^
			indent(d+1)^say(t)^say(",")^say(f)^say(")")
		| stm(MOVE(a,b),d) = indent(d)^sayln("MOVE(")^exp(a,d+1)^
			sayln(",")^exp(b,d+1)^say(")")
		| stm(EXP e, d) = indent(d)^sayln("EXP(")^exp(e,d+1)^say(")")
		and exp(BINOP(p,a,b),d) = indent(d)^say("BINOP(")^binop(p)^sayln(",")^
			exp(a,d+1)^sayln(",")^exp(b,d+1)^say(")")
		| exp(MEM(e),d) = indent(d)^sayln("MEM(")^exp(e,d+1)^say(")")
		| exp(TEMP t, d) = indent(d)^say("TEMP ")^say(t)
		| exp(ESEQ(s,e),d) = indent(d)^sayln("ESEQ(")^stm(s,d+1)^sayln(",")^
			exp(e,d+1)^say(")")
		| exp(NAME lab, d) = indent(d)^say("NAME ")^say(lab)
		| exp(CONST i, d) = indent(d)^say("CONST ")^say(Int.toString i)
		| exp(CALL(e,el),d) = indent(d)^sayln("CALL(")^(exp(e,d+1))^
			concat(map (fn a => sayln(",")^exp(a,d+2)) el)^say(")")

  		and binop PLUS = say "PLUS"
		| binop MINUS = say "MINUS"
		| binop MUL = say "MUL"
		| binop DIV = say "DIV"
		| binop AND = say "AND"
		| binop OR = say "OR"
		| binop LSHIFT = say "LSHIFT"
		| binop RSHIFT = say "RSHIFT"
		| binop ARSHIFT = say "ARSHIFT"
		| binop XOR = say "XOR"

		and relop EQ = say "EQ"
		| relop NE = say "NE"
		| relop LT = say "LT"
		| relop GT = say "GT"
		| relop LE = say "LE"
		| relop GE = say "GE"
		| relop ULT = say "ULT"
		| relop ULE = say "ULE"
		| relop UGT = say "UGT"
		| relop UGE = say "UGE"

 in	stm(s0,0)^sayln("") end

end

