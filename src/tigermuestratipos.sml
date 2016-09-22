structure tigermuestratipos:> tigermuestratipos =
struct
open tigertips

fun join [] sep = ""
|   join (x::[]) sep = x
|   join (x::y::xs) sep = x^sep^join (y::xs) sep

fun tipoToString(t) =
	let
    	fun prnt TUnit = "unit"
    	| prnt TNil = "nil"
    	| prnt TInt = "int"
    	| prnt TString = "string"
    	| prnt(TArray(ref t, _)) = "array of "^prnt t
    	| prnt(TRecord(l, u)) =
			let fun  aux (sr, ref(TTipo tr), ir) =
								"record(tipo "^tr^" "^Int.toString(ir)^")"
				|    aux (_, ref(TRecord(_, u)), ir) = Int.toString ir
				|    aux (_, ref(TArray(_, u)), ir) =  Int.toString ir
				|    aux (_, ref tr , _) = prnt tr
			in "record[" ^ join (map aux l) ", " ^ "]" end
		| prnt(TTipo s) = "tipo "^s
    in prnt t end
(*
fun printTTipos tips = List.app (fn(s,t) => printTipo(s, t, tips)) tips
*)
end
