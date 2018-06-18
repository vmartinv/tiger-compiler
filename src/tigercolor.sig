signature tigercolor =
sig

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

val color : {interference: tigerliveness.igraph,   (* interference graph *)
             initial: allocation,                  (* initial allocation - precolored nodes *)
             spillCost: tigergraph.node -> int,
             registers: tigerframe.register list}  (* list of colors (registers) *)
              -> allocation * tigertemp.temp list
             
end
