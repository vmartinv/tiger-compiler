structure tigerutils:> tigerutils =
struct

fun join [] sep = ""
|   join (x::[]) sep = x
|   join (x::y::xs) sep = x^sep^join (y::xs) sep

fun endswith ext s = size(s)>=size(ext) andalso (ext = String.extract (s, size(s)-size(ext), NONE))

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

fun fromListtoSet(cmp, xs) = List.foldl (fn (x,s) => Splayset.add(s, x)) (Splayset.empty cmp) xs

fun toString x = if (x < 0) then ("-"^Int.toString(~x)) else Int.toString(x)

fun measure name f = fn x =>
    let val timer = Timer.startCPUTimer()
        val result = f x
        val time = Timer.checkCPUTimer timer
        val time_ms = Time.toMilliseconds (#usr time)
        val str =  name^" tardo "^Int.toString(time_ms)^" ms.\n"
        val _ = print(str)
    in result end

fun elimList cmp x [] = []
|   elimList cmp x (y::ys) = if (cmp(x,y) = EQUAL) then (elimList cmp x ys) else (y::(elimList cmp x ys))

fun diffList cmp xs [] = xs
|   diffList cmp xs (y::ys) = diffList cmp (elimList cmp y xs) ys

end
