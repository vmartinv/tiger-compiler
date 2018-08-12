signature tigercolor =
sig

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

(* Given an ig, colors (registers) and precolored nodes, returns a register allocation + lists of spills
val color : {interference: tigerliveness.igraph,   (* interference graph *)
             initial: allocation,                  (* initial allocation - precolored nodes *)
             spillCost: tigergraph.node -> int,    (* Specifies the spilling cost of each temporary *)
             registers: tigerframe.register list}  (* list of colors (registers) *)
              -> allocation * tigertemp.temp list
*)

val alloc : tigerassem.instr list * tigerframe.frame -> tigerassem.instr list * allocation
             
end
