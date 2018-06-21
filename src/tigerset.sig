signature tigerset =
sig

(* Abstract Data Type for sets *)

type 'item set = 'item Splayset.set ref

val emptySet : ('_item * '_item -> order) -> '_item set
val listToSet : ('_item list * ('_item * '_item -> order)) -> '_item set

end
