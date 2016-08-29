structure tigerescap :> tigerescap =
struct

open tigerabs
open tigertab

type depth = int
type escEnv = (string, depth * bool ref) tigertab.Tabla

fun travVar env d s =
	case s of
	SimpleVar s =>
		(case tabBusca(s, env) of
		SOME (dd, b) => if d>dd then b:=true else ()
		| NONE => raise Fail ("escape?? "^s^" inexist."))
	| FieldVar(v, s) => travVar env d v
	| SubscriptVar(v, e) =>
		(travVar env d v; travExp env d e)
and travExp env d s =
	case s of
	VarExp(v, _) => travVar env d v
	| CallExp({func, args}, nl) => travExp env d (SeqExp(args, nl))
	| OpExp({left, oper, right}, _) =>
		(travExp env d left; travExp env d right)
	| RecordExp({fields, typ}, _) =>
		List.app ((travExp env d) o #2) fields
	| SeqExp(le, nl) =>
		(List.foldl (fn (e, (v, d)) => (travExp v d e; (v, d)))
			(env, d) le; ())
	| AssignExp({var, exp}, _) =>
		(travVar env d var; travExp env d exp)
	| IfExp({test, then', else'=NONE}, _) =>
		(travExp env d test; travExp env d then')
	| IfExp({test, then', else'=SOME e}, _) =>
		(travExp env d test; travExp env d then'; travExp env d e)
	| WhileExp({test, body}, _) =>
		(travExp env d test; travExp env d body)
	| ForExp({var, escape, lo, hi, body}, _) =>
		let	val env' = tabRInserta(var, (d, escape), env);
		in	travExp env d  lo;
			travExp env d  hi;
			travExp env' d  body
		end
	| LetExp({decs, body}, _) =>
		travExp (travDecs env d decs) d body
	| ArrayExp({typ, size, init}, _) => travExp env d init
	| _ => ()
and travDecs env d [] = env
| travDecs env d (s::t) =
	let	fun aux s =
			case s of
			FunctionDec l =>
				let	fun aux(({name, params, result, body}, _), env) =
					let	fun aux1(x, e) =
							tabRInserta(#name(x), (d+1, #escape(x)), e)
						val env' = foldr aux1 env params
					in travExp env' (d+1) body; env end
				in	foldl aux env l end
			| VarDec({name, escape, typ, init}, _) =>
				(travExp env d init; tabRInserta(name, (d, escape), env))
			| TypeDec _ => env
		val env' = aux s
	in	travDecs env' d t end

fun findEscape prog = travExp (tigertab.tabNueva()) 0 prog
end
