structure tigercolor :> tigercolor =
struct

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

fun color c = (* COMPLETAR *)
    (Splaymap.mkDict(String.compare), [])
end
