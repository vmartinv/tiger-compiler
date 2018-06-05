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
(*
printCode : instr list -> string list
*)
fun printCode instrs = 
	let fun printAssem (OPER {assem,dst,src,jump=NONE}) = "OPER: "^assem^" D:["^(String.concatWith "," dst)^"] S:["^(String.concatWith "," src)^"]\n"
		 | printAssem (OPER {assem,dst,src,jump=SOME j}) = "OPER: "^assem^" D:["^(String.concatWith "," dst)^"] S:["^(String.concatWith "," src)^"] J:["^(String.concatWith "," j)^"]\n"
		 | printAssem (MOV {assem,dst,src}) = "MOVE: "^assem^" D:"^dst^" S:"^src^"\n"
		 | printAssem (aLABEL {lab,...}) = "LABEL: "^lab^"\n"
	in
		map printAssem instrs
	end
end
