structure tigersimpleregalloc :> tigersimpleregalloc =
struct
	(*structure frame = tigerframe
	open tigerassem
	
	fun simpleregalloc (frm:frame.frame) (body:instr list) =
	let
		(* COMPLETAR: Temporarios que ya tienen color asignado (p.ej, el temporario que representa a rax) *)
		val precolored = ["a", "b", "c", "d", "e", "f"]
		(* COMPLETAR: Temporarios que se pueden usar (p.ej, el temporario que representa a rax. Diferencia con precolored: el temporario que representa a rbp no se puede usar) *)
		val asignables = ["b", "c", "d", "e", "f"]
		(* COMPLETAR: movaMem crea una instrucciÃ³n que mueve un temporario a memoria. movaTemp, de memoria a un temporario.*)
		fun movaMem(temp, mempos) =
			let
				val desp = if mempos<0 then " - " ^ Int.toString(~mempos) else if mempos>0 then " + " ^ Int.toString(mempos) else ""
			in
				OPER {assem="mov `s0 M(a" ^ desp ^ ")", src=[temp], dst=[], jump=NONE}
			end
		fun movaTemp(mempos, temp) =
			let
				val desp = if mempos<0 then " - " ^ Int.toString(~mempos) else if mempos>0 then " + " ^ Int.toString(mempos) else ""
			in
				OPER {assem="mov M(a" ^ desp ^ ") `d0", src=[], dst=[temp], jump=NONE}
			end
		val temps =
			let
				val tempList = 
					let
						fun f (OPER r, tmplist) = List.concat [#dst r, #src r, tmplist]
						| f (LABEL _, tmplist) = tmplist
						| f (MOVE r, tmplist) = (#dst r)::(#src r)::tmplist
					in
						List.foldr f [] body
					end
				val s = Splayset.addList(Splayset.empty String.compare, tempList)
				val precoloredSet = Splayset.addList(Splayset.empty String.compare, precolored)
			in
				Splayset.listItems(Splayset.difference(s, precoloredSet))
			end

		val accesses = map (fn T => let val frame.InFrame n = frame.allocLocal frm true in (T, n) end) temps
		fun getFramePos T =
			let
				fun gfp T [] = raise Fail("Temporario no encontrado: "^T)
				| gfp T ((a,b)::xs) = if a=T then b else gfp T xs
			in
				gfp T accesses
			end

		fun rewriteInstr (OPER {assem, dst, src, jump}) =
			let
				val eset = Splayset.empty String.compare
				val precoloredSet = Splayset.addList(eset, precolored)
				val asignablesSet = Splayset.addList(eset, asignables)
				val dstset = Splayset.addList(eset, dst)
				val srcset = Splayset.addList(eset, src)
				val colores = Splayset.listItems(Splayset.difference(asignablesSet, Splayset.union(dstset, srcset)))
				val uncolored = Splayset.listItems(Splayset.difference(Splayset.union(dstset, srcset), precoloredSet))

				val N = length(uncolored)
				val tempcols = ListPair.zip(uncolored, List.take(colores, N))
				fun getTempCol T =
				let
					fun gtc T [] = if Splayset.member(precoloredSet, T) then T else raise Fail("Temporario no encontrado: "^T)
					| gtc T ((a,b)::xs) = if a=T then b else gtc T xs
				in
					gtc T tempcols
				end

				val (prevMovs, posMovs) =
				let
					fun mkgetMov T = movaTemp(getFramePos T, getTempCol T)
					fun mksetMov T = movaMem(getTempCol T, getFramePos T)
					fun filterPC T = not(Splayset.member(precoloredSet, T))
				in
					(map mkgetMov (List.filter filterPC src), map mksetMov (List.filter filterPC dst))
				end
				val newdst = map getTempCol dst
				val newsrc = map getTempCol src
				val newinstr = OPER {assem=assem, dst=newdst, src=newsrc, jump=jump}
			in
				List.concat [prevMovs, [newinstr], posMovs]
			end
		  | rewriteInstr (LABEL l) = [LABEL l]
		  | rewriteInstr (MOVE {assem, dst, src}) =
			let
				val precoloredSet = Splayset.addList(Splayset.empty String.compare, precolored)
			in
				if Splayset.member(precoloredSet, dst) andalso Splayset.member(precoloredSet, src) then [OPER {assem=assem, dst=[dst], src=[src], jump=NONE}]
				else if Splayset.member(precoloredSet, dst) then [movaTemp(getFramePos src, dst)]
				else if Splayset.member(precoloredSet, src) then [movaMem(src, getFramePos dst)]
				else
					let
						val color = hd(asignables)
					in
						[movaTemp(getFramePos src, color), movaMem(color, getFramePos dst)]
					end
			end
	in
		List.concat (map rewriteInstr body)
	end*)
end
