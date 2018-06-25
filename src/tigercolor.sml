structure tigercolor :> tigercolor =
struct

open tigertemp
open tigerliveness
open tigerassem
open tigerset
open tigermap
open tigerpila
open tigerutils


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
    tigerset.listToSet tigerframe.coloredregisters nodeCmp

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
val adjList : (node, nodeSet) tigermap.map = 
    tigermap.empty nodeCmp

(* Node degree *)
val degree : (node, int) tigermap.map =
    tigermap.empty nodeCmp

(* Move list *)
val moveList : (node, moveSet) tigermap.map =
    tigermap.empty nodeCmp

(* Alias *)
val alias : (node, nodeSet) tigermap.map =
    tigermap.empty nodeCmp

(* Color *)
val color : (node, tigerframe.register) tigermap.map =
    tigermap.empty nodeCmp



(********** Coloring Algorithm **********)

(* AddEdge *)
fun AddEdge (u : node, v : node) =
    if ((not (tigerset.member adjSet (u, v))) andalso (not (nodeEq(u, v)))) then (
        tigerset.add adjSet (u, v);
        tigerset.add adjSet (v, u);
        if (not (tigerset.member precolored u))
        then (
            tigermap.insert adjList u (tigerset.union (tigermap.get adjList u) (tigerset.singleton nodeCmp v));
            tigermap.insert degree u ((tigermap.get degree u) + 1)
            )
        else ();
        if (not (tigerset.member precolored v))
        then (
            tigermap.insert adjList v (tigerset.union (tigermap.get adjList v) (tigerset.singleton nodeCmp u));
            tigermap.insert degree v ((tigermap.get degree v) + 1)
            )
        else ()
        )
    else ()

(* Build, levemente distinto al libro porque no tenemos bloques, tenemos instrucciones simples *)
fun Build (instrs : tigerassem.instr list) =
    let
        val (fg, fgnodes) = tigerflow.instrs2graph instrs
        val (tigerflow.FGRAPH{control, def, use, ismove}) = fg
        val (_, liveOut) = tigerliveness.interferenceGraph fg
        fun buildNode i =
            let 
                val live = listToSet (liveOut i) nodeCmp
                val def_i = Splaymap.find(def, i)
                val use_i = Splaymap.find(use, i)
            in
                if (Splaymap.find(ismove, i))
                then (
                    List.app (fn n => tigerset.delete live n)
                             use_i;
                    List.app (fn n => tigermap.insert moveList n (tigerset.union (tigermap.get moveList n) (tigerset.singleton moveCmp (List.hd (def_i), List.hd use_i))))
                             (def_i @ use_i);
                    tigerset.add workListMoves (List.hd def_i, List.hd use_i)
                )
                else ();
                List.app (fn t => tigerset.add live t)
                         def_i;
                List.app (fn d => (tigerset.app (fn l => AddEdge(l, d))
                                                live))
                         def_i
            end
    in
        List.app buildNode fgnodes
    end

(* Node moves *)
fun NodeMoves (n:node) =
    tigerset.intersection (tigermap.get moveList n) (tigerset.union activeMoves workListMoves)

(* Move Related *)
fun MoveRelated (n:node) =
    not (tigerset.equal (NodeMoves n) (tigerset.empty moveCmp))
    
(* MakeWorkList *)
fun MakeWorkList () =
    tigerset.app (fn n => (tigerset.delete initial n;
                           if ((tigermap.get degree n) >= K)
                           then tigerset.add spillWorkList n
                           else if (MoveRelated n)
                                then tigerset.add freezeWorkList n
                                else tigerset.add simplifyWorkList n
                          ))
                 initial

(* Adjacent nodes *)
fun Adjacent (n:node) =
    tigerset.difference (tigermap.get adjList n) (tigerset.union selectStackNodes coalescedNodes)

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
        val d = tigermap.get degree n
        val nSet = tigerset.empty nodeCmp
    in 
        tigermap.insert degree n (d-1); (*degree := Splaymap.insert(degree, n, d-1);*)
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
fun Simplify () =
    let 
        val n = tigerset.get simplifyWorkList
    in
       tigerset.delete simplifyWorkList n;
       tigerpila.pushPila selectStack n;
       tigerset.app DecrementDegree (Adjacent n)
    end

(* Coloring function 
fun color (instrs : tigerassem.instr list) = (* COMPLETAR *)
    LivenessAnalysis();
    Init instrs;
    Build();
    MakeWorkList();
    repeat 
*)

(* Register Allocation *)
fun alloc (body : tigerassem.instr list, fr : tigerframe.frame) = (* COMPLETAR *)
    ([], Splaymap.mkDict(String.compare))


end

