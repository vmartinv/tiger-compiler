structure tigerutils:> tigerutils =
struct

fun join [] sep = ""
|   join (x::[]) sep = x
|   join (x::y::xs) sep = x^sep^join (y::xs) sep

fun endswith ext s = (ext = String.extract (s, size(s)-size(ext), NONE))

fun zip [] [] = []
|   zip (x::xs) (y::ys) = (x,y)::zip xs ys
|   zip _ _ = raise Fail "No deberia pasar\n"

fun unzip [] = ([], [])
|   unzip ((x, y)::xs) = ((x: ((fst o unzip) xs)), (y: ((snd o unzip) xs)))

fun flatten xxs = List.foldr (op@) [] xxs

fun reps [] = NONE
  | reps (x::xs) = if List.exists (fn y => x = y) xs then SOME x else reps xs

fun error(s, p) = raise Fail ("Error -- línea "^p^": "^s^"\n")

end
