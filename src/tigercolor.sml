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

(************************************ Types *************************************)

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


(****************************** Coloring Algorithm ******************************)

fun color (body : tigerassem.instr list, fr : tigerframe.frame) = (* COMPLETAR *)
let
    (**** Data structures ****)
    
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
    
    
    (**** Auxiliar functions ****)
    
    (* Init insert precolered nodes in color *)
    fun Init () = ()
    
    (*  *)
    fun Build () = ()
    
    (*  *)
    fun MakeWorklist () = ()
    
    (*  *)
    fun Simplify () = ()
    
    (*  *)
    fun Coalesce () = ()
    
    (*  *)
    fun Freeze () = ()
    
    (*  *)
    fun SelectSpill () = ()
    
    (*  *)
    fun AssignColors () = ()
    
in
    (**** Algorithm ****)
    
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


(* Register Allocation - Esto iría en regalloc
fun alloc (body : tigerassem.instr list, fr : tigerframe.frame) = (* COMPLETAR *)
    let
        val (allocation, spilledNodes) = color(body, fr)
    in
        if (isEmpty spilledNodes)
        then
            (body, allocation)
        else
            let val newbody =  RewriteProgram(spilledNodes, body, fr)
            in  alloc(newbody, fr)
    end
*)

end
