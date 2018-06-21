structure tigercolor :> tigercolor =
struct

open tigertemp
open tigerliveness
open tigerset
open tigerpila


(********** Types **********)

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

type node = tigertemp.temp
type edge = tigertemp.temp * tigertemp.temp
type move = tigertemp.temp * tigertemp.temp
type nodeSet = node tigerset.set
type edgeSet = edge tigerset.set
type moveSet = move tigerset.set
type nodeStack = node tigerpila.Pila


fun nodeEq (t1, t2) = String.compare (t1, t2) = EQUAL 
fun nodeCmp (t1, t2) = String.compare (t1, t2)

fun edgeEq ((t1, t2), (t3, t4)) =
    (nodeEq (t1, t3) andalso nodeEq (t2, t4)) orelse (nodeEq (t1, t4) andalso nodeEq (t2, t3))
fun edgeCmp ((t1, t2), (t3, t4)) =
    if (nodeCmp (t1, t3) = EQUAL) then nodeCmp (t2, t4) else nodeCmp (t1, t3)

fun moveEq ((t1, t2), (t3, t4)) =
    (nodeEq (t1, t3) andalso nodeEq (t2, t4))
fun moveCmp ((t1, t2), (t3, t4)) = edgeCmp ((t1, t2), (t3, t4))



(********** Data structures **********)

(***** Temporaries work-lists and sets *****)

(* Machine registers *)
val precolored : nodeSet =
    tigerset.listToSet(tigerframe.coloredregisters, nodeCmp)

(* util?:
val notPrecolored : nodeSet =
    ...
    *)
    
(* Temporaries not precolored and not yet processed *)
val initial : nodeSet = 
    tigerset.emptySet nodeCmp

(* Low-degree non-move-related nodes *)
val simplifyWorkList : nodeSet =
    tigerset.emptySet nodeCmp

(* Low-degree move-related nodes *)
val freezeWorkList : nodeSet =
    tigerset.emptySet nodeCmp

(* High-degree nodes *)
val spillWorkList : nodeSet =
    tigerset.emptySet nodeCmp

(* Nodes marked for spilling during this round *)
val spilledNodes : nodeSet =
    tigerset.emptySet nodeCmp

(* Nodes that have been coalesced *)
val coalescedNodes : nodeSet =
    tigerset.emptySet nodeCmp

(* Nodes successfully colored *)
val coloredNodes : nodeSet =
    tigerset.emptySet nodeCmp

(* Stack containing temporaries remove from the graph by simplification *)
val selectStack : nodeStack = 
    tigerpila.nuevaPila()


(***** Move sets *****)


(***** Other data structure *****)
        
(* Interference edge *)
val adjSet : edgeSet = 
    tigerset.emptySet edgeCmp

(* Adjacency list - just not precolored *)
val adjList : (node, nodeSet) Splaymap.dict = 
    Splaymap.mkDict nodeCmp

(* Node degree *)
val degree : (node, int) Splaymap.dict =
    Splaymap.mkDict nodeCmp

(* Move list *)
val moveList : (node, moveSet) Splaymap.dict =
    Splaymap.mkDict nodeCmp

(* Alias *)
val alias : (node, nodeSet) Splaymap.dict =
    Splaymap.mkDict nodeCmp

(* Color *)
val color : (node, tigerframe.register) Splaymap.dict =
    Splaymap.mkDict nodeCmp


(********** Coloring Algorithm **********)


(* Simplify function
fun simplify () =
    val n = get(simplifyWorkList)
    delete(simplifyWorkList, n)
    push(n, selectStack)
    forall m in adj(n)
        decrementDegree(m)
end
*)



(* Coloring function *)
fun color c = (* COMPLETAR *)
    let 
        val precolored = ["a", "b", "c", "d", "e", "f"]

    in
        (Splaymap.mkDict(String.compare), [])
    end
end
