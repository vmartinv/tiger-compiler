signature tigercodegen =
sig

val codegens: tigerframe.frame -> tigertree.stm list -> tigerassem.instr list
val codegen: tigerframe.frame -> tigertree.stm -> tigerassem.instr list

end
