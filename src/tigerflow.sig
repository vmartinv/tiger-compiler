signature tigerflow =
sig

datatype flowgraph =
	FGRAPH of {control: tigergraph.graph, (*flow graph*)
	           def: (tigertemp.temp list) tigergraph.table, (*temporarios definidos en cada nodo*)
	           use: (tigertemp.temp list) tigergraph.table, (*temporarios usados en cada nodo*)
	           ismove: bool tigergraph.table} (*dice si cada nodo es un MOVE - se puede borrar si def y use son iguales*)

val getGraph: flowgraph -> tigergraph.graph
val getDef: flowgraph -> (tigertemp.temp list) tigergraph.table
val getUse: flowgraph -> (tigertemp.temp list) tigergraph.table
val getMov: flowgraph -> bool tigergraph.table

val instrs2graph: tigerassem.instr list -> flowgraph * tigergraph.node list

val printGraph: tigerassem.instr list * flowgraph -> string


end
