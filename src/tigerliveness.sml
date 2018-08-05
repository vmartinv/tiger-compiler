structure tigerliveness :> tigerliveness =
struct

open tigergraph
open tigerflow
open tigertemp
open tigerutils

datatype igraph =
    IGRAPH of {graph: tigergraph.graph,                         (* interference graph *)
               tnode: tigertemp.temp -> tigergraph.node,        (* mapea temporarios del assembler a nodos *)
               gtemp: tigergraph.node -> tigertemp.temp,        (* mapping inverso al anterior *)
               moves: (tigergraph.node * tigergraph.node) list} (* en lo posible asignar el mismo registro a cada par *)

(* computation of liveness by iteration *)
fun livenessCalc (FGRAPH {control, def, use, ismove}) =
    let (* auxiliar values *)
        val nodes = nodes control
        val empty: (node, temp Splayset.set) Splaymap.dict = Splaymap.mkDict(cmp)
        val set_use = List.foldl (fn (n,map) => Splaymap.insert(map, n, fromListtoSet(String.compare, Splaymap.find(use, n)))) empty nodes
        val set_def = List.foldl (fn (n,map) => Splaymap.insert(map, n, fromListtoSet(String.compare, Splaymap.find(def, n)))) empty nodes
        
        (* initialize *)
        val live_in = List.foldl (fn (n,map) => Splaymap.insert(map, n, Splaymap.find(set_use, n))) empty nodes
        val live_out = List.foldl (fn (n,map) => Splaymap.insert(map, n, Splayset.empty String.compare)) empty nodes
        val live_in' = List.foldl (fn (n,map) => Splaymap.insert(map, n, Splayset.empty String.compare)) empty nodes
        val live_out' = List.foldl (fn (n,map) => Splaymap.insert(map, n, Splayset.empty String.compare)) empty nodes
        
        (* repeat until fixed point is reached *)
        fun checkEq(map1, map2) = List.all (fn n => Splayset.equal(Splaymap.find(map1, n), Splaymap.find(map2, n))) nodes
        fun repeatIt lIn lIn' lOut lOut' =
            if checkEq(lIn, lIn') andalso checkEq(lOut, lOut')
            then (lIn, lOut) (* fixed point reached! *)
            else let val newIn =  List.foldl (fn (n,map) => Splaymap.insert(map, n, Splayset.union(Splaymap.find(set_use, n), (Splayset.difference(Splaymap.find(lOut, n), Splaymap.find(set_def, n)))))) empty nodes
                     val newOut = List.foldl (fn (n,map) => Splaymap.insert(map, n, (List.foldl (fn (m,s) => Splayset.union(s, Splaymap.find(newIn, m))) (Splayset.empty String.compare) (succ n)))) empty nodes (* antes decía lIn en vez de newIn *)
                 in repeatIt newIn lIn newOut lOut
                 end
    in  repeatIt live_in live_in' live_out live_out'
    end

(* interference graph *)
fun interferenceGraph(cfg) = 
    let (* 1ro : calculo de liveness *)
        val (l_in, l_out) = livenessCalc cfg
        fun live_out n = Splayset.listItems (Splaymap.find(l_out, n))
        
        (* 2do : interference graph *)
        
        (* auxiliar values *)
        val cfnodes = nodes (getGraph cfg)
        val def_tab = getDef cfg
        val use_tab = getUse cfg
        val temps = (* conjunto de todos los temporarios del programa *)
                    let val def_temps = List.foldl (fn (n, s) => Splayset.union(s, fromListtoSet(String.compare, Splaymap.find(def_tab, n)))) (Splayset.empty String.compare) cfnodes
                        (* necesario o con los de defs estamos? Usás un temporario que no definís?: *) (* En la pág 213 dice que una variable puede venir de antes, ej: formal parameter *)
                        val use_temps = List.foldl (fn (n, s) => Splayset.union(s, fromListtoSet(String.compare, Splaymap.find(use_tab, n)))) (Splayset.empty String.compare) cfnodes
                    in Splayset.union(def_temps, use_temps)
                    end
                    
        (* creo grafo de interferencia *)
        val g = tigergraph.newGraph ()
        
        (* creo los nodos del grafo de interferencia y los mapeos correspondientes a tnode y gtemp *)
        val (tnode_tab, gtemp_tab) = Splayset.foldl (fn (t,(tmapn, nmapt)) => let val n = newNode g in (Splaymap.insert(tmapn, t, n), Splaymap.insert(nmapt, n, t)) end) (Splaymap.mkDict String.compare, Splaymap.mkDict cmp) temps
        
        (* creo tnode y gtemp *)
        fun tnode t = Splaymap.find(tnode_tab, t)
        fun gtemp n = Splaymap.find(gtemp_tab, n)
        
        (* creo las aristas del grafo de interferencia, con tratamiento especial para MOVE (ver pags 221,222) y creo moves *)
        val ismove_tab = getMov cfg        
        fun addEdges t ts = List.app (fn x => mk_edge{from = tnode t, to = tnode x}) (remove t ts)     
        fun addEdgesMov n ms = let val a = List.hd (Splaymap.find(def_tab, n))
                                   val c = List.hd (Splaymap.find(use_tab, n))
                                   val _ = addEdges a (remove c (live_out n))
                               in (tnode a, tnode c)::ms
                               end                               
        fun addEdgesNoMov n ms = let val a = Splaymap.find(def_tab, n)
                                     val b = live_out n
                                     val _ = List.app (fn t => addEdges t b) a
                                 in ms
                                 end                              
        val moves = List.foldl (fn (n,mvs) => if Splaymap.find(ismove_tab, n) then addEdgesMov n mvs else addEdgesNoMov n mvs) [] cfnodes
        
        (* finalmente, el grafo de interferencia *)
        val ig = IGRAPH{graph = g, tnode = tnode, gtemp = gtemp, moves = moves}
        
    in (ig, live_out)
    end

fun printInter (IGRAPH{graph = g, tnode = tnode, gtemp = gtemp, moves = moves}, live_out) =
    let
        fun visNodeGraph node = "\t"^gtemp node^": "^String.concatWith ", " (Listsort.sort String.compare (map (gtemp) (adj node)))^"\n"
        val succsStr = "adj:\n"^concat(map visNodeGraph (nodes g))
        val movesStr = "moves:\n\t"^String.concatWith ", " (map (fn (a,b) => "("^gtemp a ^", "^gtemp b^")") moves)^"\n"
    in succsStr^movesStr end

end

