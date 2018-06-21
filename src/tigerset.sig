signature tigerset =
sig

(* Abstract Data Type for sets *)

type 'item set = 'item Splayset.set ref

exception NotFound

val emptySet : ('_item * '_item -> order) -> '_item set
val listToSet : ('_item list * ('_item * '_item -> order)) -> '_item set
val isEmpty : '_item set -> bool
val get : '_item set -> '_item
val add : '_item set -> '_item -> '_item
val delete : '_item set -> 'item -> '_item set
val union : '_item set -> '_item set -> '_item set
val diff : '_item set -> '_item set -> '_item set
val app : ('_item -> unit) -> '_item set -> unit

end
