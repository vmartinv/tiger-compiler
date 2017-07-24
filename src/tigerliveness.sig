signature tigerliveness =
sig

datatype igraph =
	IGRAPH of {graph: tigergraph.graph, (*interference graph*)
	           tnode: tigertemp.temp -> tigergraph.node, (*mapea temporarios del assembler a nodos*)
	           gtemp: tigergraph.node -> tigertemp.temp, (*mapea nodos a temporarios del assembler*)
	           moves: (tigergraph.node * tigergraph.node) list} (*en lo posible asignar el mismo registro a cada par*)
(*
val interferenceGraph: tigerflow.flowgraph -> igraph * (tigergraph.node -> tigertemp.temp list) (* grafo de interferencia + los temporarios que salen vivos de cada nodo del flow graph *)
*)
end

