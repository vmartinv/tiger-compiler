signature tigerset =
sig

(* Abstract Data Type for sets *)

type 'item set = 'item Splayset.set ref

exception NotFound

val empty : ('_item * '_item -> order) -> '_item set
val singleton : ('_item * '_item -> order) -> '_item -> '_item set
val isEmpty : '_item set -> bool
val notEmpty : '_item set -> bool
val equal : '_item set -> '_item set -> bool
val listToSet : '_item list -> ('_item * '_item -> order) -> '_item set
val setToList : '_item set -> '_item list
val member : '_item set -> '_item -> bool
val get : '_item set -> '_item
val add : '_item set -> '_item -> unit
val delete : '_item set -> 'item -> unit
val intersection : '_item set -> '_item set -> '_item set
val union : '_item set -> '_item set -> '_item set
val difference : '_item set -> '_item set -> '_item set
val app : ('_item -> unit) -> '_item set -> unit

end
