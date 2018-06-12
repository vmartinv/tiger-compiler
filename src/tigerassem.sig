signature tigerassem =
sig

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


val formatString : tigertemp.label * string -> string
val format: (temp->string) -> instr -> string
val printInstr: instr -> string
val printCode: instr list -> string

end
