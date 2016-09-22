structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertopsort

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString), ("arr", TArray (ref TInt, ref ()))]) (* ACORDARSE DE SACAR EL arr *)

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=mainLevel, label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=mainLevel, label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=mainLevel, label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=mainLevel, label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=mainLevel, label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=mainLevel, label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=mainLevel, label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=mainLevel, label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=mainLevel, label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=mainLevel, label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

fun tipoReal (TTipo s, (env : tenv)) : Tipo = 
    (case tabBusca(s , env) of 
         NONE => raise Fail "tipoReal Ttipo"
       | SOME t => t)
  | tipoReal (t, _) = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo _) b =
		(* let *)
		(* 	val a = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (1)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (1)"
  | tiposIguales a (TTipo _) =
		(* let *)
		(* 	val b = case !r of *)
		(* 		SOME t => t *)
		(* 		| NONE => raise Fail "No debería pasar! (2)" *)
		(* in *)
		(* 	tiposIguales a b *)
		(* end *)raise Fail "No debería pasar! (2)"
  | tiposIguales a b = (a=b)

fun zip [] [] = []
|   zip (x::xs) (y::ys) = (x,y)::zip xs ys
|   zip _ _ = raise Fail "No deberia pasar\n"

fun join [] sep = ""
|   join (x::[]) sep = x
|   join (x::y::xs) sep = x^sep^join (y::xs) sep

fun transExp(venv, tenv) =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(VarExp v) = trvar(v)
		| trexp(UnitExp _) = {exp=(), ty=TUnit}
		| trexp(NilExp _)= {exp=(), ty=TNil}
		| trexp(IntExp(i, _)) = {exp=(), ty=TInt}
		| trexp(StringExp(s, _)) = {exp=(), ty=TString}
		| trexp(CallExp({func, args}, nl)) = (*COMPLETAR*)
            let
                val (typR, typArgs) = case tabBusca(func,venv) of
                                          NONE => error("Funcion inexistente ("^func^")",nl)
                                        | SOME (Func{result=typR,formals=typArgs,...}) => (typR, typArgs)
                                        | SOME _ => error(func^" no es una función",nl)
                val callArgs:Tipo list = map #ty (map trexp args)
                val _ = if typArgs = callArgs
                        then ()
                        else error("Los argumentos deberían ser: "^join (map tigermuestratipos.tipoToString typArgs) "->"^"\n"
                                 ^ "y se recibio: "^join (map tigermuestratipos.tipoToString callArgs) "->", nl)
                
            in
                {exp=(), ty=typR}
            end
        | trexp(OpExp({left, oper=EqOp, right}, nl)) =
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
					else error("Tipos no comparables", nl)
			end
		| trexp(OpExp({left, oper, right}, nl)) = 
			let
				val {exp=_, ty=tyl} = trexp left
				val {exp=_, ty=tyr} = trexp right
			in
				if tiposIguales tyl tyr then
					case oper of
						PlusOp => if tipoReal(tyl, tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| MinusOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| TimesOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| DivideOp => if tipoReal(tyl,tenv)=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| LtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| LeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| GtOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| GeOp => if tipoReal(tyl,tenv)=TInt orelse tipoReal(tyl,tenv)=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
						| _ => raise Fail "No debería pasar! (3)"
				else error("Error de tipos", nl)
			end
		| trexp(RecordExp({fields, typ}, nl)) =
			let
				(* Traducir cada expresión de fields *)
				val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

				(* Buscar el tipo *)
				val (tyr, cs) = case tabBusca(typ, tenv) of
					SOME t => (case tipoReal(t,tenv) of
						TRecord (cs, u) => (TRecord (cs, u), cs)
						| _ => error(typ^" no es de tipo record", nl))
					| NONE => error("Tipo inexistente ("^typ^")", nl)
				
				(* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
				fun verificar [] [] = ()
				  | verificar (c::cs) [] = error("Faltan campos", nl)
				  | verificar [] (c::cs) = error("Sobran campos", nl)
				  | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
						if s<>sy then error("Error de campo", nl)
						else if tiposIguales ty (!t) then verificar cs ds
							 else error("Error de tipo del campo "^s, nl)
				val _ = verificar cs tfields
			in
				{exp=(), ty=tyr}
			end
		| trexp(SeqExp(s, nl)) =
			let	val lexti = map trexp s
				val exprs = map (fn{exp, ty} => exp) lexti
				val {exp, ty=tipo} = hd(rev lexti)
			in	{ exp=(), ty=tipo } end
		| trexp(AssignExp({var=SimpleVar s, exp}, nl)) = (*COMPLETAR_DONE*)
			let
				val {exp=_, ty=tye} = trexp exp
				val {exp=_, ty=tyv} = trvar (SimpleVar s, nl)
				val tyr = case tabBusca(s, venv) of
					  SOME VIntro => error("Variable de solo lectura ("^s^")", nl)
					| SOME (Func _) => error("No es una variable ("^s^")", nl)
					| SOME (Var _) => if tiposIguales tye tyv
									  then tye
									  else error("Error de tipos en asignación", nl)
					| NONE => error("Variable inexistente ("^s^")", nl)
			in
				{exp=(), ty=TUnit}
			end
		| trexp(AssignExp({var, exp}, nl)) = (*COMPLETAR_DONE*)
			let
				val {exp=_, ty=tye} = trexp exp
				val {exp=_, ty=tyv} = trvar (var, nl)
			in
				if tiposIguales tye tyv
				then {exp=(), ty=TUnit}
				else error("Error de tipos en asignación", nl)
			end
		| trexp(IfExp({test, then', else'=SOME else'}, nl)) =
			let val {exp=testexp, ty=tytest} = trexp test
			    val {exp=thenexp, ty=tythen} = trexp then'
			    val {exp=elseexp, ty=tyelse} = trexp else'
			in
				if tipoReal(tytest,tenv)=TInt andalso tiposIguales tythen tyelse then {exp=(), ty=tythen}
				else error("Error de tipos en if" ,nl)
			end
		| trexp(IfExp({test, then', else'=NONE}, nl)) =
			let val {exp=exptest,ty=tytest} = trexp test
			    val {exp=expthen,ty=tythen} = trexp then'
			in
				if tipoReal(tytest,tenv)=TInt andalso tythen=TUnit then {exp=(), ty=TUnit}
				else error("Error de tipos en if", nl)
			end
		| trexp(WhileExp({test, body}, nl)) =
			let
				val ttest = trexp test
				val tbody = trexp body
			in
				if tipoReal(#ty ttest, tenv) = TInt andalso #ty tbody = TUnit then {exp=(), ty=TUnit}
				else if tipoReal(#ty ttest, tenv) <> TInt then error("Error de tipo en la condición", nl)
				else error("El cuerpo de un while no puede devolver un valor", nl)
			end
		| trexp(ForExp({var, escape, lo, hi, body}, nl)) = (*COMPLETAR_DONE*)
			let
				val {exp=_, ty=tyl} = trexp lo
				val {exp=_, ty=tyh} = trexp hi
				val _ = if tiposIguales tyl TInt andalso tiposIguales tyh TInt
						then ()
						else error("Las cotas del for deben ser de tipo int", nl)
				val venv' = tabRInserta(var, VIntro, venv)
				val {exp=_, ty=tyb} = transExp (venv', tenv) body
			in
				if tiposIguales tyb TUnit then {exp=(), ty=TUnit} else error("El cuerpo de un for no debe devolver valor", nl)
			end
		| trexp(LetExp({decs, body}, _)) =
			let
				val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
				val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
			in 
				{exp=(), ty=tybody}
			end
		| trexp(BreakExp nl) = (*COMPLETAR_DONE*)
			{exp=(), ty=TUnit}
		| trexp(ArrayExp({typ, size, init}, nl)) = (*COMPLETAR_DONE*) (* testeo con tipo arr trucho *)
			let
				val (tya, cs) = case tabBusca(typ, tenv) of
						  SOME t => (case tipoReal(t,tenv) of
									   TArray (cs, u) => (TArray (cs, u), cs)
							         | _ => error(typ^" no es de tipo array", nl))
						| NONE => error("Tipo inexistente ("^typ^")", nl)
				val {exp=_, ty=tys} = trexp size
				val {exp=_, ty=tyi} = trexp init
				val _ = if (tiposIguales tys TInt)
						then ()
						else error("El tamaño del arreglo debe ser de tipo int", nl)
				val _ = if tiposIguales tyi (!cs)
						then ()
						else error("El valor inicial no corresponde con el tipo del arreglo", nl)
			in
				{exp=(), ty=tya}
			end
		and trvar(SimpleVar s, nl) = (*COMPLETAR_TO_TEST*)
			let
				val tyv = case tabBusca (s,venv) of
						    NONE => error("Variable inexistente ("^s^")", nl)
						  | SOME VIntro => TInt
						  | SOME (Var {ty}) => ty
				   	      | _ => error(s^" no es una variable." , nl)
			in
				{exp=(), ty=tyv}
			end
		| trvar(FieldVar(v, s), nl) = (*COMPLETAR_TO_TEST*)
			let
				val {exp=_, ty=tyv} = trvar(v, nl)
				val t = (case tyv of
						   TRecord (l, _) => (case List.filter (fn x => #1 x = s) l of
						                       [] => error(s^" no es un campo del record", nl)
						                     | (c::_) => #2 c)
						 | _ => error("No es una variable de tipo record, no se puede indexar" , nl))
			in
				{exp=(), ty=(!t)}
			end
		| trvar(SubscriptVar(v, e), nl) = (*COMPLETAR_TO_TEST*)
			let
				val {exp=_, ty=tyv} = trvar(v, nl)
				val t = (case tyv of
						   TArray (tr, _) => tr
						 | _ => error("No es una variable de tipo array, no se puede indexar" , nl))
				val {exp=_, ty=tye} = trexp e
				val _ = if tiposIguales tye TInt
						then ()
						else error("El indice no es de tipo entero" , nl)
			in
				{exp=(), ty=(!t)}
			end
		and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) =  (*COMPLETAR_DONE*)
			let
                val {exp=_, ty=tyi} = transExp (venv, tenv) init
                val _ = case tyi of
                          TNil => error("No se puede inferir el tipo de nil en la declaracion de "^name, pos)
                        | _ => ()
                val venv' = tabRInserta(name, Var {ty = tyi}, venv)
            in
                (venv', tenv, []) (*lista vacia, que es? para dsp, para llevar los efectos laterales*)
            end            
		| trdec (venv, tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =  (*COMPLETAR_DONE*)
			let
                val {exp=_, ty=tyi} = transExp (venv, tenv) init
                val _ = case tabBusca(s, tenv) of
                          NONE => error("El tipo "^s^" no esta declarado.", pos)
                        | SOME t => if tiposIguales t tyi
                                    then ()
                                    else error("Los tipos de la declaracion no coinciden", pos)                        
                val venv' = tabRInserta(name, Var {ty = tyi}, venv)
            in
                (venv', tenv, [])
            end
		| trdec (venv, tenv) (FunctionDec fs) = (* fs = ({name: symbol, params: field list, result: symbol option, body: exp} * pos) list*)   (*COMPLETAR_hacer en dos pasadas*)
			let
				(* checkeo de repetición de nombres: no se pueden sobreescribir funciones dentro de un mismo batch. *)
				fun reps [] = NONE
                  | reps (x::xs) = if List.exists (fn y => x = y) xs then SOME x else reps xs
                
				val _ = case reps (map (fn x => (#name (#1 x))) fs) of
                            NONE => ()
                        |   SOME x => error("No se permite la repetición de nombre de función en un mismo batch",
                                                #2 (valOf (List.find (fn y => x = (#name (#1 y))) (rev fs))))
				
				(* 1ra pasada: checkeo de tipo de los parametros formales y retorno para actualizar venv *)
				fun toTipoRet nl r = case r of
                                        NONE => TUnit
                                      | SOME typ => case tabBusca(typ, tenv) of
                                                    NONE => error("Tipo inexistente ("^typ^")", nl)
                                                  | SOME t => t

				fun toTipoArg nl {name, escape, typ=NameTy typ} =
						(case tabBusca(typ, tenv) of
					 	   SOME t => t
						 | _ => error("Tipo inexistente ("^typ^")", nl))
                |   toTipoArg nl _ = error("Error en los tipos de los parametros.", nl) (* La sintaxis de tiger no permite que los argumentos tengan explicitamente tipo record o array *)
                
                fun toTipoArgs nl xs  = map (toTipoArg nl) xs
                fun haceHeader ({name = n, params = p, result = r,...},nl) =
                    (n, Func {level = (), label = tigertemp.newlabel(), formals = toTipoArgs nl p , result = toTipoRet nl r, extern = false})
                fun agregaHeader ((name, header), venv) = tabRInserta (name, header, venv)
				
                val venv' = foldl agregaHeader venv (map haceHeader fs)
                
				(* 2da pasada: checkeo de tipo de retorno y cuerpo de la función *)                
				fun checkFunc ({name = n, params = p, result = r, body = b},nl) =
						let
							val tipos = toTipoArgs nl p
							val nombres = map #name p
                            fun agregaArg ((name, typ), venv) = tabRInserta (name, Var{ty=typ}, venv)
                            val venv'' = foldl agregaArg venv' (zip nombres tipos)
							val {ty = tipoB,...} = transExp (venv'',tenv) b
							val tipoR = case tabBusca(n,venv') of
										  NONE => error("No deberia pasar",nl)
                                        | SOME (Func{result=r,...}) => r
                                        | SOME _ => error("No deberia pasar",nl)
							val _ = if tiposIguales tipoB tipoR
									then ()
									else error("El tipo de retorno de la funcion y el de su definición no coinciden", nl)
						in
							()
						end
				val _ = map checkFunc fs
			in
				(venv', tenv, [])
			end
		| trdec (venv, tenv) (TypeDec ts) =(*COMPLETAR_CORREGIR*)
            let
                val tenv' = tigertopsort.fijaTipos (map #1 ts) tenv handle Ciclo => error("Hay un ciclo en la declaracion de tipos", #2 (hd ts))
            in
                (venv, tenv', [])
            end
			
	in trexp end
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=NONE, body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) ex (*ojo*)
	in	print "bien!\n" end
end
