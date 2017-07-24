structure tigerflow :> tigerflow =
struct

open tigergraph
open tigerassem

datatype flowgraph =
	FGRAPH of {control: tigergraph.graph, (*flow graph*)
	           def: (tigertemp.temp list) tigergraph.table, (*temporarios definidos en cada nodo*)
	           use: (tigertemp.temp list) tigergraph.table, (*temporarios usados en cada nodo*)
	           ismove: bool tigergraph.table} (*dice si cada nodo es un MOVE - se puede borrar si def y use son iguales*)

fun getGraph (FGRAPH{control, ...}) = control
(* Necesario?:
fun getDef (FGRAPH{def, ...}) = def
fun getUse (FGRAPH{use, ...}) = use
fun getMov (FGRAPH{ismove, ...}) = ismove *)

fun makeFlowGraph instrs = (* tigerassem.instr list -> flowgraph *)
    let
        (* control *)
        val g = newGraph ()
        val nodes = List.map (fn _ => newNode g) instrs
        val labelDict:(label, node) Splaymap.dict = (* diccionario de labels *)
			let val emptyl: (label,node) Splaymap.dict = Splaymap.mkDict (String.compare)
                fun aux((LABEL{lab, ...},n),map) = Splaymap.insert(map, lab, n)
                  | aux(_, map) = map
            in List.foldl aux emptyl (tigerutils.zip instrs nodes)
            end
        fun makeEdges (OPER{assem, dst, src, jump = SOME ls}, curr, next) =
            let val labelnodes = map (fn l => Splaymap.find(labelDict, l)) ls
            in List.app (fn l => mk_edge {from = curr, to = l}) labelnodes
            end
          | makeEdges (_, curr, next) = mk_edge {from = curr, to = next}
        val _ = List.app makeEdges (tigerutils.zip3R instrs nodes (tl nodes))
        
        (* def *)
        val emptyt: (tigertemp.temp list) tigergraph.table = Splaymap.mkDict(cmp)
        fun addDef ((OPER{dst, ...}, node), map) = Splaymap.insert(map, node, dst)
          | addDef ((MOVE{dst, ...}, node), map) = Splaymap.insert(map, node, [dst])
          | addDef ((_,node), map) = Splaymap.insert(map, node, [])
        val d = List.foldl addDef emptyt (tigerutils.zip instrs nodes)
        
        (* use *)
        fun addUse ((OPER{src, ...}, node), map) = Splaymap.insert(map, node, src)
          | addUse ((MOVE{src, ...}, node), map) = Splaymap.insert(map, node, [src])
          | addUse ((_,node), map) = Splaymap.insert(map, node, [])
        val u = List.foldl addUse emptyt (tigerutils.zip instrs nodes)
        
        (* ismove *)
        val emptym: bool tigergraph.table = Splaymap.mkDict(cmp)
        fun addMove ((MOVE{...}, node), map) = Splaymap.insert(map, node, true)
          | addMove ((_,node), map) = Splaymap.insert(map, node, false)
        val m = List.foldl addMove emptym (tigerutils.zip instrs nodes)
    in
        FGRAPH{control = g, def = d, use = u, ismove = m}
    end

fun instrs2graph instrs =
    let val g = makeFlowGraph instrs
        val n = tigergraph.nodes (getGraph g)
    in (g, n)
    end

end

