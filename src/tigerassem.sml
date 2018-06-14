structure tigerassem :> tigerassem =
struct

type reg = string
type temp = tigertemp.temp
type label = tigertemp.label

datatype instr = OPER of {assem: string,
                          dst: temp list,
                          src: temp list,
                          jump: label list option}
               | aLABEL of {assem: string,
                           lab: label}
               | MOV of {assem: string,
                          dst: temp,
                          src: temp}

fun format _ ins = ""
	(*COMPLETAR*)
	
fun printInstr (OPER {assem,dst,src,jump=NONE}) = "OPER: "^assem^" D:["^(String.concatWith "," dst)^"] S:["^(String.concatWith "," src)^"]"
	 | printInstr (OPER {assem,dst,src,jump=SOME j}) = "OPER: "^assem^" D:["^(String.concatWith "," dst)^"] S:["^(String.concatWith "," src)^"] J:["^(String.concatWith "," j)^"]"
	 | printInstr (MOV {assem,dst,src}) = "MOVE: "^assem^" D:"^dst^" S:"^src
	 | printInstr (aLABEL {lab,...}) = "LABEL: "^lab

(*
printCode : instr list -> string
*)
fun printCode instrs = concat (map (fn instr => printInstr instr^"\n") instrs)

fun formatString(l, "") = l ^ ":\n"
	| formatString(l, s) = l^":\t.long "^tigerutils.toString (size s)^"\n"^"\t.ascii \"" ^ s ^ "\"\n"

end
