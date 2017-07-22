signature tigergraph =
sig

type graph
type node

val nodes: graph -> node list
val succ: node -> node list
val pred: node -> node list
val adj: node -> node list
val eq: node*node -> bool

val newGraph: unit -> graph
val newNode: graph -> node
exception GraphEdge
val mk_edge: {from: node, to: node} -> unit
val rm_edge: {from: node, to: node} -> unit

type 'a table = (node, 'a) Splaymap.dict

(* Faltan funciones para debug *)

end

