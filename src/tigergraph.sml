structure tigergraph :> tigergraph =
struct

open Dynarray
open tigerutils

(* Graph types and operations *)

type nodeId = int

type nodeInfo = {pred: nodeId list, succ: nodeId list}

type graph = nodeInfo array

type node = graph * nodeId (*como las operaciones no toman como argumento el grafo, el tipo nodo debe incluir a qué grafo pertenece *)

val fakeNode = {pred = [~1], succ = []}

fun isFake({pred=[~1], succ = []}) = true
|   isFake _ = false

val emptyNode = {pred = [], succ = []}


fun nodes(g:graph) =
	let fun aux i = if isFake(sub(g,i)) then [] else (g, i)::aux(i+1)	
	in aux 0
	end

fun succ(n:node) = map (fn id => (#1 n, id)) (#succ (sub(#1 n, #2 n)))

fun pred(n:node) = map (fn id => (#1 n, id)) (#pred (sub(#1 n, #2 n)))

fun adj(n:node) =
	let 
		val succid = #succ (sub(#1 n, #2 n)) 
		val predid = #pred (sub(#1 n, #2 n))
		val adjsid = elimRep (succid @ predid) (*elimino repetidos, pues un nodo puede ser succ y pred de otro al mismo tiempo*)
	in map (fn id => (#1 n, id)) adjsid
	end

(* no creo que necesitemos esto, hasta liveness no lo usé
fun eqGraph g h = ....
fun eq ((g,n), (h,m)) = if eqGraph g h then n = m else false*)
fun eq ((_,n), (_,m)) = n = m

fun cmp ((_,n), (_,m)) = Int.compare(n,m)

fun newGraph () = array(0, fakeNode)

fun newNode(g:graph) = (*agrego el nodo al final porque no hay una operación de destrucción de nodos en la interfaz*)
    let fun look(lo,hi) =
               (* i < lo indicates i in use
                  i >= hi indicates i not in use *)
            if lo=hi then (update(g,lo,emptyNode); (g,lo))
            else let val m = (lo+hi) div 2
                  in if isFake(sub(g,m)) then look(lo,m) else look(m+1,hi)
                 end
    in look(0, 1 + bound g)
    end

exception GraphEdge

fun mk_edge {from=n:node, to=m:node} =
	let val g = #1 n
		val i = #2 n
		val j = #2 m
	in update(g, i, {pred = #pred(sub(g,i)), succ = j::(#succ(sub(g,i)))});
	   update(g, j, {pred = i::(#pred(sub(g,j))), succ = #succ(sub(g,j))})
	end

fun rm_edge {from=n:node, to=m:node} =
	let val g = #1 n
		val i = #2 n
		val j = #2 m
	in update(g, i, {pred = #pred(sub(g,i)), succ = remove j (#succ(sub(g,i)))});
	   update(g, j, {pred = remove i (#pred(sub(g,j))), succ = #succ(sub(g,j))})
	end
	
(* Tables *)
type 'a table = (node, 'a) Splaymap.dict

(* For debug *)
fun nodename (g, id) = "n"^(toString(id))


end

