structure tigerset:> tigerset =
struct


type 'item set = 'item Splayset.set ref

exception NotFound

fun emptySet cmp = ref (Splayset.empty cmp)

fun listToSet (l, cmp) = ref (Splayset.addList(Splayset.empty cmp, l))

val isEmpty s =
    Splayset.isEmpty(!s)

val get s =
    case Splayset.find true (!s) of
          SOME i => i
        | _      => raise NotFound
        
val add s n =
    s := Splayset.add(!s, n)
    
val delete s n =
    if Splayset.member(!s, n) then s := Splayset.delete(!s, n) else () (* ok? *)

val union s t =
    ref (Splayset.union(!s,!t))
    
val diff s t =
    ref (Splayset.difference(!s,!t))

val app f s =
    Splayset.app f (!s)

end
