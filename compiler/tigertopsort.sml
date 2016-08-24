structure tigertopsort:> tigertopsort =
struct

open tigerabs
open tigertab
open tigertips

infix rs ls -- ---
fun x ls f = fn y => f(x, y)
fun f rs y = fn x => f(x, y)
fun l -- e = List.filter (op<> rs e) l
fun fst(x, _) = x and snd(_, y) = y
fun lp --- e = List.filter ((op<> rs e) o fst) lp
exception Ciclo

(* P: pares.  E: elementos.
St: Stack de no tratados.
Res: resultado.  *)

fun topsort P =
	let	fun candidato E P =
			List.filter (fn e => List.all((op<> rs e) o snd) P) E
		fun tsort P [] Res = rev Res
		| tsort [] St Res = rev(St @ Res)
		| tsort P (St as (h::t)) Res =
			let	val x = (hd(candidato St P)) handle Empty => raise Ciclo
			in	tsort (P --- x) (St -- x) (x::Res) end
	fun elementos lt =
		List.foldr (fn((x, y), l) =>
			let	val l1 = case List.find (op= rs x) l of
							NONE => x::l | _ => l
				val l2 = case List.find (op= rs y) l1 of
							NONE => y::l1 | _ => l1
			in	l2 end) [] lt
	in	tsort P (elementos P) [] end

fun buscaArrRecords lt =
	let	fun buscaRecs [] res = res
		| buscaRecs({name, ty=RecordTy lf}::t) res =
			let	fun genrecs [] _ = []
				| genrecs({name, escape, typ=NameTy s}::tail) n =
					(name, ref(TTipo s), n)::genrecs tail (n+1)
				| genrecs _ _ = raise Fail "error interno 666+3"
			in	buscaRecs t ((name, TRecord(genrecs lf 0, ref()))::res) end
		| buscaRecs({name, ty=ArrayTy ty}::t) res = buscaRecs t ((name, TArray(ref(TTipo ty), ref()))::res)
		| buscaRecs(_::t) res = buscaRecs t res
	in	buscaRecs lt [] end
fun genPares lt =
	let
		val lrecs = buscaArrRecords lt
		fun genP [] res = res
		| genP ({name, ty=NameTy s'}::t) res = (print("NameTy "^s'^"\n"); genP t ((s', name)::res)   )
		| genP ({name, ty=ArrayTy s'}::t) res = genP t ((s', name)::res)
		| genP ({name, ty=RecordTy lf}::t) res = genP t res
	in	genP lt [] end
fun procesa [] pares env _ = env: (string, Tipo) Tabla
| procesa (sorted as (h::t)) (pares:{name:symbol, ty:ty} list) env recs =
	let
		fun filt h {name, ty = NameTy t} = h=t
		| filt h {name, ty = ArrayTy t} = h=t
		| filt h {name, ty = RecordTy lt} = List.exists (((NameTy h) ls op=) o #typ) lt
		val (ps, ps') = List.partition (filt h) pares
		val ttopt = case tabBusca(h, env) of
					SOME t => SOME t
					| _ =>
						case List.find((h ls op=) o #1) recs of
						SOME (n, tr) =>
								(tabRInserta(h, tr, env);
								SOME tr) (* OJOOOOOOOOOOOOOOOO *)
						| _ => raise Fail (h^" **no existe!!!")
		val env' = case ttopt of
					SOME tt =>
						List.foldr
							(fn({name, ty=NameTy ty}, env') => tabRInserta(name, tt, env')
							| ({name, ty=ArrayTy s}, env') =>
								let val (k, v) =
										case List.find((name ls op=) o #1) recs of
										SOME x => x | _ => raise Fail "error 666+45"
								in	tabRInserta(k, v , env') end
							| ({name, ty=RecordTy s}, env') =>
								let val (k, v) =
										case List.find((name ls op=) o #1) recs of
										SOME x => x | _ => raise Fail "error 666+46"
								in	tabRInserta(k, v , env') end)
					 		env ps
					| _ => env
	in procesa t ps' env' recs end
fun fijaNONE [] env = env
| fijaNONE((name, TArray(ar as (ref(TTipo s)), u))::t) env =
	(case tabBusca(s, env) of
	NONE => raise Fail "error interno 666+1"
	| SOME ras => (ar := ras; fijaNONE t env))
| fijaNONE((name, TRecord(lf, u))::t) env =
	let	fun busNONE(s, ar as (ref(TTipo t)), _) =
			(ar := tabSaca(t, env) handle _ => raise noExiste)
		| busNONE _ = ()
		val _ = List.app busNONE lf
	in	fijaNONE t env end
| fijaNONE(_::t) env = fijaNONE t env
fun agregarecs env [] = env
| agregarecs env ((k, v)::t) =
	agregarecs (tabRInserta(k, v, env)) t

fun fijaTipos batch env =
	let	val pares = genPares batch
		val recs = buscaArrRecords batch
		val orden = topsort pares
		val env' = procesa orden batch env recs
		val env'' = agregarecs env' recs
		val env''' = fijaNONE (tabAList env'') env''
val _ = tigermuestratipos.printTTipos(tabAList env'')
	in	env''' end
end
