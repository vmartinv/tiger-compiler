structure tigerregalloc :> tigerregalloc =
struct

open tigerutils

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

fun alloc (body, fr) = (* COMPLETAR *)
    ([], Splaymap.mkDict(String.compare))

end
