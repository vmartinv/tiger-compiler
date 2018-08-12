signature tigermap =
sig

(* Abstract Data Type for maps *)

type ('_key, '_a) map = ('_key, '_a) Splaymap.dict ref

exception NotFound

val empty : ('_key * '_key -> order) -> ('_key, '_a) map
val get : ('_key, '_a) map -> '_key -> '_a
val insert : ('_key, '_a) map -> '_key -> '_a -> unit

end
