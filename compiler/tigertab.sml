structure tigertab :> tigertab =
struct
open Polyhash

type ('a, 'b) Tabla = ('a, 'b) hash_table

exception yaExiste of string
exception noExiste
exception noExisteS of string

fun tabNueva() = mkPolyTable(100, noExiste)
fun fromTab t =
	let	val t' = tabNueva()
	in	apply (fn x => insert t' x) t; t' end
fun name x = x
fun tabEsta(s, t) = 
	case peek t s of
	SOME _ => true
	| NONE => false
fun tabInserta(s, e, t) = let val t' = copy t in (peekInsert t' (s, e); t') end
fun tabRInserta(s, e, t) = let val t' = copy t in (insert t' (s, e); t') end
fun tabBusca(s, t) = peek t s
fun tabSaca(s, t) =
	case tabBusca(s, t) of
	SOME t => t
	| NONE => raise noExiste
fun tabAplica(f, t) = map(fn(_, e) => f e) t
fun tabAAplica(f, g, t) = 
	let	val l' = listItems t
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k, e))
			(List.map(fn(k, e) => (f k, g e)) l');
		t'
	end
fun tabRAAplica(f, g, t) = 
	let	val l' = rev(listItems t)
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k, e))
			(List.map(fn(k, e) => (f k, g e)) l');
		t'
	end
fun tabInserList(t, l) = 
	let val t' = copy t in (List.app(fn(s, e) => insert t' (s, e)) l; t') end
fun tabAList t = listItems t
fun tabFiltra(f, t) =
	let	val l = listItems t
		val t' = mkPolyTable(100, noExiste)
	in
		List.app(fn(k, e) => insert t' (k,e))
			(List.filter (fn(a, b) => f b) l);
		t'
	end
fun tabPrimer(f, t) = hd(List.filter (fn(a, b) => f b) (listItems t))
fun tabClaves t = List.map (fn(x, y) => x) (listItems t)
end
