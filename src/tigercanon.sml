structure tigercanon :> tigercanon = 
struct

open tigertab
open tigertree

fun linearize(stm0: stm) : stm list =
	let
		infix %
		fun (EXP(CONST _)) % x = x
		| x % (EXP(CONST _)) = x
		| x % y = SEQ(x,y)

		fun commute(EXP(CONST _), _) = true
		| commute(_, NAME _) = true
		| commute(_, CONST _) = true
		| commute(EXP(CALL(NAME "_checkIndexArray", _)), _) = true
		| commute(EXP(CALL(NAME "_checkNil", _)), _) = true
		| commute(EXP x, y) =
			let	fun inmut(NAME _) = true
				| inmut(CONST _) = true
				| inmut(TEMP FP) = true
				| inmut(BINOP(_, x, y)) = inmut(x) andalso inmut(y)
				| inmut _ = false
			in	inmut x orelse inmut y end
		| commute _ = false

		val nop = EXP(CONST 0)

		fun reorder ((e as CALL _ )::rest) =
			let	val t = tigertemp.newtemp()
			in	reorder(ESEQ(MOVE(TEMP t, e),
						TEMP t) :: rest) end
		| reorder (a::rest) =
			let val (stms,e) = do_exp a
				val (stms',el) = reorder rest
			in	if commute(stms',e)
				then (stms % stms',e::el)
				else
					let	val t = tigertemp.newtemp()
					in	(stms % MOVE(TEMP t, e) % stms',
							TEMP t :: el) end
			end
		| reorder nil = (nop,nil)

		and reorder_exp(el,build) =
			let	val (stms,el') = reorder el
			in	(stms, build el') end

		and reorder_stm(el,build) =
			let	val (stms,el') = reorder (el)
			in	stms % build(el') end

		and do_stm(SEQ(a,b)) = do_stm a % do_stm b
		| do_stm(JUMP(e,labs)) = 
			reorder_stm([e],fn l => JUMP(hd l,labs))
		| do_stm(CJUMP(p,a,b,t,f)) = 
			reorder_stm([a,b], fn l=> CJUMP(p,hd l,hd(tl l),t,f))
		| do_stm(MOVE(TEMP t,CALL(e,el))) = 
			reorder_stm(e::el,fn l => MOVE(TEMP t,CALL(hd l,tl l)))
		| do_stm(MOVE(TEMP t,b)) = 
			reorder_stm([b],fn l=>MOVE(TEMP t,hd l))
		| do_stm(MOVE(MEM e,b)) = 
			reorder_stm([e,b],fn l=>MOVE(MEM(hd l),hd(tl l)))
		| do_stm(MOVE(ESEQ(s,e),b)) = do_stm(SEQ(s,MOVE(e,b)))
		| do_stm(EXP(CALL(e,el))) = 
			reorder_stm(e::el,fn l => EXP(CALL(hd l,tl l)))
		| do_stm(EXP e) = reorder_stm([e],fn l=>EXP(hd l))
		| do_stm s = reorder_stm([],fn _=>s)

		and do_exp(BINOP(p,a,b)) = 
			reorder_exp([a,b], fn l=>BINOP(p,hd l,hd(tl l)))
		| do_exp(MEM(a)) = reorder_exp([a], fn l=>MEM(hd l))
		| do_exp(ESEQ(s,e)) = 
			let	val stms = do_stm s
				val (stms',e) = do_exp e
			in	(stms%stms',e) end
		| do_exp(CALL(e,el)) = 
			reorder_exp(e::el, fn l => CALL(hd l,tl l))
		| do_exp e = reorder_exp([],fn _=>e)

  (* linear gets rid of the top-level SEQ's, producing a list *)
		fun linear(SEQ(a,b),l) = linear(a,linear(b,l))
		| linear(s,l) = s::l

	in (* body of linearize *)
		linear(do_stm stm0, nil)
	end

type block = stm list

  (* Take list of statements and make basic blocks satisfying conditions
       3 and 4 above, in addition to the extra condition that 
      every block ends with a JUMP or CJUMP *)

fun basicBlocks stms = 
	let	val done = tigertemp.newlabel()
		fun blocks((head as LABEL _) :: tail, blist) =
			let	fun next((s as (JUMP _))::rest, thisblock) =
					endblock(rest, s::thisblock)
				| next((s as (CJUMP _))::rest, thisblock) =
					endblock(rest,s::thisblock)
				| next(stms as (LABEL lab :: _), thisblock) =
					next(JUMP(NAME lab,[lab])
					:: stms, thisblock)
				| next(s::rest, thisblock) = next(rest, s::thisblock)
				| next(nil, thisblock) = 
					next([JUMP(NAME done, [done])],
						thisblock)

				and endblock(stms, thisblock) = 
					blocks(stms, rev thisblock :: blist)
			in next(tail, [head]) end
		| blocks(nil, blist) = rev blist
		| blocks(stms, blist) =
			blocks(LABEL(tigertemp.newlabel())::stms, blist)
	in (blocks(stms,nil), done) end

fun enterblock(b as (LABEL s :: _), table) = tabRInserta(s, b, table)
| enterblock(_, table) = table

fun splitlast([x]) = (nil,x)
| splitlast(h::t) = let val (t',last) = splitlast t in (h::t', last) end
| splitlast _ = raise Fail "no ocurre!"

fun trace(table,b as (LABEL lab :: _),rest) = 
	let	val table = tabRInserta(lab, nil, table)
	in	case splitlast b of
		(most,JUMP(NAME lab, _)) =>
			(case tabBusca(lab, table) of
			SOME(b' as _::_) => most @ trace(table, b', rest)
			| _ => b @ getnext(table,rest))
		| (most,CJUMP(opr,x,y,t,f)) =>
			(case (tabBusca(t,table), tabBusca(f,table)) of
			(_, SOME(b' as _::_)) => b @ trace(table, b', rest)
			| (SOME(b' as _::_), _) => 
				most @ [CJUMP(notRel opr,x,y,f,t)]
				@ trace(table, b', rest)
			| _ =>	let	val f' = tigertemp.newlabel()
					in most @ [CJUMP(opr,x,y,t,f'), 
						LABEL f',
						JUMP(NAME f,[f])]
						@ getnext(table,rest)
					end)
		| (most, JUMP _) => b @ getnext(table,rest)
		| _ => raise Fail "debiera ser imposible!(1)"
	end
| trace _ = raise Fail "debiera ser imposible!(2)"

and getnext(table,(b as (LABEL lab::_))::rest) = 
	(case tabBusca(lab, table) of
	SOME(_::_) => trace(table,b,rest)
	| _ => getnext(table,rest))
| getnext(table,nil) = nil
| getnext _ = raise Fail "no puede pasar!"

fun traceSchedule(blocks,done) = 
       getnext(foldr enterblock (tabNueva()) blocks, blocks)
         @ [LABEL done]

end
