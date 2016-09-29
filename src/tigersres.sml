structure tigersres =
struct

open tigerabs
open tigertab
open tigertips

datatype EnvEntry =
	VIntro of {access: tigertrans.access, level: int}	(* int readonly - solo en for *)
	| Var of {ty: Tipo, access: tigertrans.access, level: int}
	| Func of {level: tigertrans.level, label: tigertemp.label,
		formals: Tipo list, result: Tipo, extern: bool}

(* level: nivel de anidamiento
access: dónde está alojada la variable
label: etiqueta, por posible colision de nombre
formals: parametros formales
result: tipo de retorno de la función
extern: si es externa (externa <-> de libreria - no definida en el cod fuente)
*)


end
