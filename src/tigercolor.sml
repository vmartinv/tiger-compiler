structure tigercolor :> tigercolor =
struct

open tigertemp
open tigerliveness
open tigerassem
open tigerset
open tigermap
open tigerpila
open tigerutils
open tigerframe

(********************************************************************************)
(************************************ Types *************************************)
(********************************************************************************)

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


(********************************************************************************)
(****************************** Coloring Algorithm ******************************)
(********************************************************************************)

fun color (instrs : tigerassem.instr list, fr : tigerframe.frame) = (* COMPLETAR *)
let
    
    (************************************************************)
    (********************* Data structures **********************)
    (************************************************************)
    
    (* Number of registers *)
    val K : int = 32
    
    (* precolored nodes *)
    val precolored : nodeSet = tigerset.listToSet tigerframe.coloredregisters nodeCmp
    
    (* temporary registers, not precolored and not yet processed *)
    val initial : nodeSet = tigerset.empty nodeCmp
    
    (* low-degree non-move-related nodes *)
    val simplifyWorklist : nodeSet = tigerset.empty nodeCmp
    
    (* low-degree move-related nodes *)
    val freezeWorklist : nodeSet = tigerset.empty nodeCmp
    
    (* high-degree nodes *)
    val spillWorklist : nodeSet = tigerset.empty nodeCmp
    
    (* nodes marked for spilling *)
    val spilledNodes : nodeSet = tigerset.empty nodeCmp
    
    (* nodes that have been coalesced *)
    val coalescedNodes : nodeSet = tigerset.empty nodeCmp
    
    (* successfully colored nodes *)
    val coloredNodes : nodeSet = tigerset.empty nodeCmp
    
    (* stack with temporaries removed from the graph *)
    val selectStack : nodeStack = tigerpila.nuevaPila()
    
    (* nodes in the stack *)
    val selectStackSet : nodeSet = tigerset.empty nodeCmp
    
    (* moves that have been coalesced *)
    val coalescedMoves : moveSet = tigerset.empty moveCmp
    
    (* moves whose source and target interfere *)
    val constrainedMoves : moveSet = tigerset.empty moveCmp

    (* moves that will no longer be considered for coalescing *)
    val frozenMoves : moveSet = tigerset.empty moveCmp
    
    (* moves enables for possible coalescing *)
    val worklistMoves : moveSet = tigerset.empty moveCmp
    
    (* moves not yet ready for coalescing *)
    val activeMoves : moveSet = tigerset.empty moveCmp
    
    (* interference edges *)
    val adjSet : edgeSet = tigerset.empty edgeCmp

    (* adjacency list - precolored nodes not included *)
    val adjList : (node, nodeSet) tigermap.map = tigermap.empty nodeCmp

    (* nodes degree *)
    val degree : (node, int) tigermap.map = tigermap.empty nodeCmp

    (* mapping from a node to the moves it is associated with *)
    val moveList : (node, moveSet) tigermap.map = tigermap.empty nodeCmp

    (* alias *)
    val alias : (node, nodeSet) tigermap.map = tigermap.empty nodeCmp

    (* color chosen by the algorithm for each node *)
    val color : (node, tigerframe.register) tigermap.map = tigermap.empty nodeCmp 
    
    
    (************************************************************)
    (******************** Auxiliar functions ********************)
    (************************************************************)
    
    (* Init initializes color and initial *)
    fun Init () =
	app (fn n => insert color n n) precolored (* TODO: QUÉ REL HAY ENTRE TEMP Y REGISTER, ACA SE DEBE DEVOLVER REGISTER, NO TEMP *)
    
    (* AddEdge *)
    fun AddEdge (u: node, v: node) =
	if ((not (tigerset.member adjSet (u, v))) andalso (not (nodeEq(u,v))))
	then (
	    tigerset.add adjSet (u, v);
	    tigerset.add adjSet (v, u);
	    if (not (tigerset.member precolored u))
	    then (
		tigermap.insert adjList u (tigerset.union (tigermap.get adjList u) (tigerset.singleton nodeCmp v));
		tigermap.insert degree u ((tigermap.get degree u) + 1))
	    else ();
	    if (not (tigerset.member precolored v))
	    then (
		tigermap.insert adjList v (tigerset.union (tigermap.get adjList v) (tigerset.singleton nodeCmp u));
		tigermap.insert degree v ((tigermap.get degree v) + 1))
	    else ())
	else ()
	
    (* Build constructs the interference graph (adjSet, adjList, degree, moveList) and initializes initial and worklistMoves.
       This is simpler than the book's algorithm because we don't have blocks, just simple instructions *)
    fun Build () =
	let
	    val (fg, fgnodes) = tigerflow.instrs2graph instrs
	    val (tigerflow.FGRAPH{control, def, use, ismove}) = fg
	    val (_, liveOut) = tigerliveness.interferenceGraph fg
	    fun buildNode n =
		let
		    val live_n = listToSet (liveOut n) nodeCmp
		    val use_n  = Splaymap.find(use, n)
		    val def_n  = Splaymap.find(def, n)
		in
		    if (Splaymap.find(ismove, n))
		    then (
			List.app (fn n => tigermap.insert moveList n (tigerset.union (tigermap.get moveList n) (tigerset.singleton moveCmp (List.hd def_n, List.hd use_n)))) (def_n @ use_n);
			tigerset.add worklistMoves (List.hd def_n, List.hd use_n))
		    else ();
		    List.app (fn n => tigerset.add initial n) (def_n @ use_n);
		    List.app (fn d => tigerset.app (fn l => AddEdge(l,d)) live_n) def_n
		end
	in
	    List.app buildNode fgnodes
	end
    
    (* ANTES ESTABA ASÍ, PERO CREO QUE TIENE COSAS QUE CORRESPONDEN A LOS BLOQUES
    
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
                             use_i; BLOQUE
                    List.app (fn n => tigermap.insert moveList n (tigerset.union (tigermap.get moveList n) (tigerset.singleton moveCmp (List.hd (def_i), List.hd use_i))))
                             (def_i @ use_i);
                    tigerset.add workListMoves (List.hd def_i, List.hd use_i)
                )
                else ();
                List.app (fn t => tigerset.add live t)
                         def_i; BLOQUE
                List.app (fn d => (tigerset.app (fn l => AddEdge(l, d))
                                                live))
                         def_i
            end
    in
        List.app buildNode fgnodes
    end
    *)
    
    (* *)
    fun NodeMoves (n:node) =
		tigerset.intersection (tigermap.get moveList n) (tigerset.union activeMoves worklistMoves)
    
    (* *)
    fun MoveRelated (n:node) =
		tigerset.notEmpty (NodeMoves n)
	
    (*  MakeWorklist initializes worklists *)
    fun MakeWorklist () =
		tigerset.app (fn n => (tigerset.delete initial n;
		                      if (tigermap.get degree n >= K) then
								tigerset.add spillWorklist n
		                      else if (MoveRelated n) then
								tigerset.add freezeWorklist n
		                      else
								tigerset.add simplifyWorklist n))
		             initial
    
    (* *)
    fun Adjacent (n:node) =
		tigerset.difference (tigermap.get adjList n)
		                    (tigerset.union selectStackSet coalescedNodes)
    
    (* *)
    fun EnableMoves (s:nodeSet) =
		tigerset.app (fn n => tigerset.app (fn m => (if (tigerset.member activeMoves m) then (
														tigerset.delete activeMoves m;
														tigerset.add worklistMoves m)
		                                             else ()))
		                                   (NodeMoves n))
		             s
	
    (* *)
    fun DecrementDegree (n:node) =
		let
			val d = tigermap.get degree n
		in
			tigermap.insert degree n (d-1);
			if (d = K) then (
				EnableMoves (tigerset.union (Adjacent n) (tigerset.singleton nodeCmp n));
				tigerset.delete spillWorklist n;
				if (MoveRelated n) then
					tigerset.add freezeWorklist n
				else
					tigerset.add simplifyWorklist n)
			else ()
		end
		
    (* Simplify function *)
    fun Simplify () =
		let
			val n = tigerset.get simplifyWorklist
		in
			tigerset.delete simplifyWorklist n;
			tigerpila.push selectStack n;
			tigerset.add selectStackSet n;
			tigerset.app DecrementDegree (Adjacent n)
		end
    
    (*  *)
    fun Coalesce () = ()
    
    (*  *)
    fun Freeze () = ()
    
    (*  *)
    fun SelectSpill () = ()
    
    (*  *)
    fun AssignColors () = ()
    
in
    (************************************************************)
    (************************ Algorithm *************************)
    (************************************************************)
    
    Init();
    Build();
    MakeWorklist();
    while( (notEmpty simplifyWorklist) orelse (notEmpty worklistMoves) orelse (notEmpty freezeWorklist) orelse (notEmpty spillWorklist) )
    do (
    if (notEmpty simplifyWorklist) then Simplify()
    else if (notEmpty worklistMoves) then Coalesce()
    else if (notEmpty freezeWorklist) then Freeze()
    else if (notEmpty spillWorklist) then SelectSpill()
    else ()
    );
    AssignColors();
    (!color, setToList spilledNodes)
end


(* Register Allocation - Esto es de regalloc
fun alloc (body : tigerassem.instr list, fr : tigerframe.frame) =
    let
        val (allocation, spilledNodes) = tigercolor.color(body, fr)
    in
        if (null spilledNodes)
        then
            (body, allocation)
        else
            let val newbody =  RewriteProgram(spilledNodes, body, fr)
            in  alloc(newbody, fr)
            end
    end
    
end
*)

end
