structure tigersres =
struct

open tigerabs
open tigertab
open tigertips

datatype EnvEntry =
	VIntro	(* int readonly *)
	| Var of {ty: Tipo}
	| Func of {level: unit, label: tigertemp.label,
		formals: Tipo list, result: Tipo, extern: bool}

(* level: nivel de anidamiento
label: etiqueta, por posible colision de nombre
formals: parametros formales
result: tipo de retorno de la función
extern: si es externa (externa <-> de libreria - no definida en el cod fuente)
*)

val mainLevel = ()
end
