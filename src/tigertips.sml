structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
    | TNil
    | TInt
    | TString
    | TArray of Tipo ref  * unique
    | TRecord of (string * Tipo ref * int) list * unique
    | TTipo of string 

end

(* unique está porque cada record y array es distinto (aún si tiene los mismos campos y tipos) *)
