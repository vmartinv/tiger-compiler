structure tigerutils:> tigerutils =
struct

fun join [] sep = ""
|   join (x::[]) sep = x
|   join (x::y::xs) sep = x^sep^join (y::xs) sep

fun endswith ext s = (ext = String.extract (s, size(s)-size(ext), NONE))

fun zip [] [] = []
|   zip (x::xs) (y::ys) = (x,y)::zip xs ys
|   zip _ _ = raise Fail "No deberia pasar\n"

fun zip3R [] ys zs = []
|   zip3R xs [] zs = []
|   zip3R xs ys [] = []
|   zip3R (x::xs) (y::ys) (z::zs) = (x,y,z)::zip3R xs ys zs

fun flatten xxs = List.foldr (op@) [] xxs

fun reps [] = NONE
  | reps (x::xs) = if List.exists (fn y => x = y) xs then SOME x else reps xs

fun error(s, p) = raise Fail ("Error -- línea "^p^": "^s^"\n")

fun elimRep []      = []
  | elimRep (x::xs) = if List.exists (fn y => y = x) xs then elimRep xs else x::(elimRep xs)

fun remove e xs = List.filter (fn x => x <> e) xs

end
