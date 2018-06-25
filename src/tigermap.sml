structure tigermap:> tigermap =
struct

type ('_key, '_a) map = ('_key, '_a) Splaymap.dict ref

exception NotFound

fun empty cmp =
    ref (Splaymap.mkDict cmp)

fun get m k =
    Splaymap.find(!m, k)
    
fun insert m k v =
    m := Splaymap.insert(!m, k, v)


end
