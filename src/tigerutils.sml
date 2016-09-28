structure tigerutils:> tigerutils =
struct

fun join [] sep = ""
|   join (x::[]) sep = x
|   join (x::y::xs) sep = x^sep^join (y::xs) sep

fun zip [] [] = []
|   zip (x::xs) (y::ys) = (x,y)::zip xs ys
|   zip _ _ = raise Fail "No deberia pasar\n"

fun flatten xxs = List.foldr (op@) [] xxs

fun error(s, p) = raise Fail ("Error -- línea "^p^": "^s^"\n")

end
