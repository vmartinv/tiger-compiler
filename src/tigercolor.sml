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
    tigerset.empty nodeCmp

(* Low-degree non-move-related nodes *)
val simplifyWorkList : nodeSet =
    tigerset.empty nodeCmp

(* Low-degree move-related nodes *)
val freezeWorkList : nodeSet =
    tigerset.empty nodeCmp

(* High-degree nodes *)
val spillWorkList : nodeSet =
    tigerset.empty nodeCmp

(* Nodes marked for spilling during this round *)
val spilledNodes : nodeSet =
    tigerset.empty nodeCmp

(* Nodes that have been coalesced *)
val coalescedNodes : nodeSet =
    tigerset.empty nodeCmp

(* Nodes successfully colored *)
val coloredNodes : nodeSet =
    tigerset.empty nodeCmp

(* Stack containing temporaries remove from the graph *)
val selectStack : nodeStack = 
    tigerpila.nuevaPila()

(* Set of nodes removed from the graph (stack nodes) *)
val selectStackNodes : nodeSet =
    tigerset.empty nodeCmp

(***** Move sets *****)

(* Moves that have been coalesced *)
val coalescedMoves : moveSet =
    tigerset.empty moveCmp
    
(* Moves whose source and target interfere *)
val constrainedMoves : moveSet =
    tigerset.empty moveCmp

(* Moves that will no longer be considered for coalescing *)
val frozenMoves : moveSet =
    tigerset.empty moveCmp
    
(* Moves enables for possible coalescing *)
val workListMoves : moveSet =
    tigerset.empty moveCmp
    
(* Moves not yet ready for coalescing *)
val activeMoves : moveSet =
    tigerset.empty moveCmp
    
    
(***** Other data structure *****)
        
(* Interference edge *)
val adjSet : edgeSet = 
    tigerset.empty edgeCmp

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
fun Adjacent (n:node) =
    tigerset.difference (Splaymap.find(adjList, n)) (tigerset.union selectStackNodes coalescedNodes)

(* Node moves *)
fun NodeMoves (n:node) =
    tigerset.intersection (Splaymap.find(moveList, n)) (tigerset.union activeMoves workListMoves)

(* Move Related *)
fun MoveRelated (n:node) =
    not (tigerset.equal (NodeMoves n) (tigerset.empty moveCmp))

(* Enable Moves *)
fun EnableMoves (ns : nodeSet) =
    tigerset.app (fn n => tigerset.app
                          (fn m => if (tigerset.member activeMoves m) then
                                       (tigerset.delete activeMoves m)
                                   else
                                       (tigerset.add workListMoves m))
                          (NodeMoves n))
                  ns

(* Decrement degree *)
fun DecrementDegree (n:node) =
    let
        val d = Splaymap.find(degree, n)
        val nSet = tigerset.empty nodeCmp
    in 
        Splaymap.insert(degree, n, d-1);
        tigerset.add nSet n;
        if (d = K) then (
            EnableMoves (tigerset.union (Adjacent n) nSet);
            tigerset.delete spillWorkList n;
            if (MoveRelated n) then
                tigerset.add freezeWorkList n
            else
                tigerset.add simplifyWorkList n            
        ) else
            ()
    end

(* Simplify function *)
fun simplify () =
    let 
        val n = tigerset.get simplifyWorkList
    in
       tigerset.delete simplifyWorkList n;
       tigerpila.pushPila selectStack n;
       tigerset.app DecrementDegree (Adjacent n)
    end





(* Coloring function *)
fun color c = (* COMPLETAR *)
    let 
        val precolored = ["a", "b", "c", "d", "e", "f"]

    in
        (Splaymap.mkDict(String.compare), [])
    end
end
