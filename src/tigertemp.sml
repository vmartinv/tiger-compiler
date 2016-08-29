structure tigertemp :> tigertemp = struct
type label = string
type temp = string
fun makeString s = s
local
	val i = ref 0
	val j = ref 0
in
	fun newtemp() =
		let
			val s = "T"^Int.toString(!i)
		in
			i := !i+1;
			s
		end
	fun newlabel() =
		let
			val s = "L"^Int.toString(!j)
		in
			j := !j+1;
			s
		end
end
end
