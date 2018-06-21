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

(* Number of registers *)
val K = 32 (* completar *)

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

(* Set of nodes removed from the graph *)
val selectStackNodes : nodeSet =
    tigerset.emptySet nodeCmp

(***** Move sets *****)

(* Moves that have been coalesced *)
val coalescedMoves : moveSet =
    tigerset.emptySet moveCmp
    
(* Moves whose source and target interfere *)
val constrainedMoves : moveSet =
    tigerset.emptySet moveCmp

(* Moves that will no longer be considered for coalescing *)
val frozenMoves : moveSet =
    tigerset.emptySet moveCmp
    
(* Moves enables for possible coalescing *)
val worklistMoves : moveSet =
    tigerset.emptySet moveCmp
    
(* Moves not yet ready for coalescing *)
val activeMoves : moveSet =
    tigerset.emptySet moveCmp
    
    
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

(* Adjacent nodes *)
fun adjacent (n:node) =
    tigerset.diff (Splaymap.find(adjList, n)) (tigerset.union selectStackNodes coalescedNodes)

(* Completar:

(* Node moves *)
fun nodeMoves (n:node) =

(* Move Related *)
fun moveRelated (n:node) =

(* Enable Moves *)
fun enableMoves (ns : nodeSet) =

(* Decrement degree *)
fun decrementDegree (n:node) =
    let
        val d = Splaymap.find(degree, n)
    in 
        Splaymap.insert(degree, n, d-1);
        if (d = K) then (
            enableMoves (tigerset.add (adjacent n) n)
            tigerset.delete spillWorkList n
            if moveRelated(n) then
                tigerset.add freezeWorkList n
            else
                tigerset.add simplifyWorkList n            
        ) else
            ()
    end
*)

(* Simplify function *)
fun simplify () =
    let 
        val n = tigerset.get(simplifyWorkList)
    in
       tigerset.delete simplifyWorkList n;
       tigerpila.pushPila selectStack n (*;
       tigerset.app decrementDegree (adjacent n) *)
    end





(* Coloring function *)
fun color c = (* COMPLETAR *)
    let 
        val precolored = ["a", "b", "c", "d", "e", "f"]

    in
        (Splaymap.mkDict(String.compare), [])
    end
end
