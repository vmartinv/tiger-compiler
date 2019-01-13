signature tigermap =
sig

(* Abstract Data Type for maps *)

type ('_key, '_a) map = ('_key, '_a) Splaymap.dict ref

val empty : ('_key * '_key -> order) -> ('_key, '_a) map
val get : ('_key, '_a) map -> '_key -> string -> '_a
val getDef : ('_key, '_a) map -> '_key -> '_a -> '_a
val insert : ('_key, '_a) map -> '_key -> '_a -> unit

end
