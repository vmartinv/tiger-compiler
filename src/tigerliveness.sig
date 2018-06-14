signature tigerliveness =
sig

datatype igraph =
	IGRAPH of {graph: tigergraph.graph,                         (* interference graph *)
	           tnode: tigertemp.temp -> tigergraph.node,        (* mapea temporarios del assembler a nodos *)
	           gtemp: tigergraph.node -> tigertemp.temp,        (* mapping inverso al anterior *)
	           moves: (tigergraph.node * tigergraph.node) list} (* en lo posible asignar el mismo registro a cada par *)

(* Takes a flow graph and returns the corresponding interference graph and a table mapping each flow graph node to the set of temps that are live-out at that node *)
val interferenceGraph: tigerflow.flowgraph -> igraph * (tigergraph.node -> tigertemp.temp list)

(* For debugging *)
val printInter: tigerassem.instr list * igraph * (tigergraph.node -> tigertemp.temp list) -> string

end

