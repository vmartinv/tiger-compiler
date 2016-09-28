signature tigerutils =
sig
	val join: string list -> string -> string
    val zip: 'a list -> 'b list -> ('a * 'b) list
    val error: string * string -> 'a
end
