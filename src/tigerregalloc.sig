signature tigerregalloc =
sig

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

val alloc : tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation

end
