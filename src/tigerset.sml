structure tigerset:> tigerset =
struct


type 'item set = 'item Splayset.set ref

exception NotFound

fun empty cmp =
    ref (Splayset.empty cmp)

fun isEmpty s =
    Splayset.isEmpty(!s)

fun equal s t =
    Splayset.equal(!s, !t)

fun listToSet (l, cmp) =
    ref (Splayset.addList(Splayset.empty cmp, l))

fun member s n =
    Splayset.member(!s, n)

fun get s =
    case Splayset.find true (!s) of
          SOME i => i
        | _      => raise NotFound
        
fun add s n =
    s := Splayset.add(!s, n)
    
fun delete s n =
    if Splayset.member(!s, n) then s := Splayset.delete(!s, n) else ()

fun intersection s t =
    ref (Splayset.intersection(!s,!t))
    
fun union s t =
    ref (Splayset.union(!s,!t))
    
fun difference s t =
    ref (Splayset.difference(!s,!t))

fun app f s =
    Splayset.app f (!s)

end
