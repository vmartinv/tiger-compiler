structure tigermap:> tigermap =
struct

type ('_key, '_a) map = ('_key, '_a) Splaymap.dict ref

fun empty cmp =
    ref (Splaymap.mkDict cmp)

fun get m k notfoundText =
    Splaymap.find(!m, k)
    handle Splaymap.NotFound => raise Fail ("tigermap.get not found: "^notfoundText)
    
fun getDef m k default =
    Splaymap.find(!m, k)
    handle Splaymap.NotFound => default
    
fun insert m k v =
    m := Splaymap.insert(!m, k, v)


end
