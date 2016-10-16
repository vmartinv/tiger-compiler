signature tigerutils =
sig
	val join: string list -> string -> string
    val endswith: string -> string -> bool
    val zip: 'a list -> 'b list -> ('a * 'b) list
    val flatten: 'a list list -> 'a list
    val error: string * string -> 'a
    val reps: ''a list -> ''a option (*Dada una lista, devuelve un elemento repetido*)
end
