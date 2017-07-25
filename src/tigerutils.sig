signature tigerutils =
sig
	val join: string list -> string -> string
    val endswith: string -> string -> bool
    val zip: 'a list -> 'b list -> ('a * 'b) list (*zip 2*)
    val zip3R: 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list (*zip 3 relajado*)
    val flatten: 'a list list -> 'a list
    val error: string * string -> 'a
    val reps: ''a list -> ''a option (*Dada una lista, devuelve un elemento repetido*)
    val elimRep: ''a list -> ''a list (*Dada una lista, devuelve la misma, eliminando elementos repetidos*)
    val remove: ''a -> ''a list -> ''a list (*Dados un elemento y una lista, elimina todas las apariciones del elemento de la lista*)
    val fromListtoSet : ('a * 'a -> order) * 'a list -> 'a Splayset.set (* Transforma una lista en un conjunto *)
end
