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
    let
		(* functions over ordered lists *)
		fun ordListUnion ([], []) = []
		  | ordListUnion (xs, []) = xs
		  | ordListUnion ([], ys) = ys
		  | ordListUnion (x::xs, y::ys) = case String.compare (x,y) of
				  LESS => x::ordListUnion(xs, y::ys)
				| GREATER => y::ordListUnion(x::xs, ys)
				| EQUAL => x::ordListUnion(xs, ys)
		fun ordListDiff ([], []) = []
		  | ordListDiff (xs, []) = xs
		  | ordListDiff ([], ys) = []
		  | ordListDiff (x::xs, y::ys) = case String.compare (x,y) of
				  LESS => x::ordListDiff(xs, y::ys)
				| GREATER => ordListDiff(x::xs, ys)
				| EQUAL => ordListDiff(xs, ys)

        fun dfs visited node =
            let val _ = visited := Splayset.add (!visited, node)
                fun recDfs n = if Splayset.member(!visited, n) then [] else dfs visited n
            in
                node :: (flatten (map recDfs (succ node)))
            end
        fun dfsAll nodes =
            let val visited = ref (Splayset.empty cmp)
                fun runDfs node:(node list) = if Splayset.member(!visited, node) then [] else dfs visited node
            in
                flatten (map runDfs nodes)
            end
        fun ArrayToList vec = Array.foldr (fn (e, ac) => e::ac) [] vec
		(* auxiliar values *)
        val nodes:(node list) = List.rev (dfsAll (nodes control))
        val (invNodes, _) = List.foldl (fn (n, (dict, i)) => (Splaymap.insert(dict, n, i), i+1)) (Splaymap.mkDict(cmp), 0) nodes
        val list_use:(string list list) = List.map (fn n => Listsort.sort String.compare (Splaymap.find(use, n))) nodes
        val list_def:(string list list) = List.map (fn n => Listsort.sort String.compare (Splaymap.find(def, n))) nodes
        val list_succ:(int list list) = List.map (fn n => map (fn ss => Splaymap.find(invNodes, ss)) (succ n)) nodes
        val list_zipped = ListPair.zip(List.tabulate (length nodes, fn i => i), ListPair.zip(ListPair.zip(list_use, list_def), list_succ))
        
    (*
        in = use
        repeat for until fixed point is reached:
            for i = 0 to N
                n = sorted[i]
                out = U suc[n] in[s]
                in[n] = use[n] U (out — def[n])
    *)
        fun repeatIt inArr =
            let
                fun updIn (i, ((use, def), succ)) =
                    let
                        val out = List.foldl (fn (inp, ac) => ordListUnion(ac, Array.sub(inArr, inp))) [] succ
                        val newIn = ordListUnion(use, ordListDiff(out, def))
                    in
                        if Array.sub(inArr, i)=newIn then false
                        else (Array.update (inArr, i, newIn); true)
                    end
                val updates = List.exists (fn x => x) (map updIn list_zipped)
            in
                if updates
                then repeatIt inArr
                else inArr
            end
       val inArr = repeatIt (Array.fromList list_use)
       val (lIn, lOut) = (ArrayToList inArr, map (fn succ => List.foldl (fn (inp, ac) => ordListUnion(ac, Array.sub(inArr, inp))) [] succ) list_succ)
       val empty: (node, temp Splayset.set) Splaymap.dict = Splaymap.mkDict(cmp)
       fun toMap l = List.foldl (fn ((k, v), ac) => Splaymap.insert(ac, k, fromListtoSet(String.compare, v))) empty (ListPair.zip(nodes, l))
    in  (toMap lIn, toMap lOut)
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

