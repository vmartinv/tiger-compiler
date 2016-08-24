(*
structure tigerpp :> tigerpp = 
struct
*)

open tigerabs
open PP

fun ppexpr pps e0 = 
	let
		fun ppf{name,escape,typ} =
			(add_string pps ("{name="^name^",");
			add_break pps (0, 1);
			add_string pps "escape=";
			add_string pps (Bool.toString(!escape));
			add_string pps "typ=";
			ppt typ;
			add_string pps "}"; add_break pps (0, 1))
		and ppd(FunctionDec flist) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "FunctionDec([";
			List.app
				(fn({name,params,result,body}, _)=>
					(add_string pps ("{name="^name^",");
					add_break pps (0, 0);
					add_string pps "params=[";
					List.app ppf params;
					add_string pps "],"; add_break pps (0, 1);
					add_string pps ("result="^
						(case result of SOME s => s | _ => "NONE"));
					add_break pps (0, 0);
					add_string pps "body="; ppe body;
					add_string pps "}"; add_break pps (0, 0)))
				flist;
			add_string pps "])"; add_break pps (0, 0);
			end_block pps)
		| ppd(VarDec({name, escape, typ, init}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps ("VarDec{name="^name^",");
			add_break pps (0, 1);
			add_string pps "escape=";
			add_string pps (Bool.toString(!escape));
			add_string pps ","; add_break pps (0, 1);
			add_string pps ("typ="^
				(case typ of SOME s => s | _ => "NONE"));
			add_string pps ","; add_break pps (0, 1);
			add_string pps "init="; ppe init;
			add_string pps "}"; add_break pps (0, 0);
			end_block pps)
		| ppd(TypeDec tlist) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "TypeDec([";
			List.app
				(fn({name: symbol, ty: ty}, _)=>
					(add_string pps ("{name="^name^","); add_break pps (0, 1);
					add_string pps "ty=";
					ppt ty; add_string pps "}";
					add_break pps (0, 1)))
				tlist;
			add_string pps "])"; add_break pps (0, 0);
			end_block pps)
		and ppt(NameTy s) =
			(begin_block pps INCONSISTENT 0;
			add_string pps ("NameTy("^s^")"); add_break pps (0, 0);
			end_block pps)
		| ppt(RecordTy fieldlist) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "RecordTy([";
			List.app ppf fieldlist;
			add_string pps "])";
			end_block pps)
		| ppt(ArrayTy s) =
			(begin_block pps INCONSISTENT 0;
			add_string pps ("ArrayTy("^s^")"); add_break pps (0, 1);
			end_block pps)
		and ppv(SimpleVar s) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "SimpleVar("; add_break pps (0, 1);
			add_string pps (s^")"); add_break pps (0, 0);
			end_block pps)
		| ppv(FieldVar(v, s)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "FieldVar(";
			ppv v; add_break pps (0, 1);
			add_string pps (","^s^")");
			end_block pps)
		| ppv(SubscriptVar(v, e)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "SubscriptVar("; add_break pps (0, 1);
			ppv v; add_string pps ","; add_break pps (0, 1);
			ppe e; add_string pps ")"; add_break pps (0, 0);
			end_block pps)
		and ppe(UnitExp _) = add_string pps "UnitExp"
		| ppe(NilExp _) = add_string pps "NilExp"
		| ppe(IntExp(n, _)) = add_string pps (Int.toString n)
		| ppe(StringExp(s, _)) = add_string pps ("\""^s^"\"")
		| ppe(BreakExp _) = add_string pps "BreakExp"
		| ppe(VarExp(v, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "VarExp("; add_break pps (0, 1);
			ppv v; add_break pps (0, 0);
			add_string pps ")"; add_break pps (0, 0);
			end_block pps)
		| ppe(OpExp({left, oper, right}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "OpExp{"; add_break pps (0, 0);
					add_string pps "left=";
					ppe left; add_string pps ","; add_break pps (0, 1);
			add_string pps "oper=";
			add_string pps
				(case oper of
				PlusOp => "PlusOp" | MinusOp => "MinusOp"
				| TimesOp => "TimesOp" | DivideOp => "DivideOp"
				| EqOp => "EqOp" | NeqOp => "NeqOp"
				| LtOp => "LtOp" | LeOp => "LeOp"
				| GtOp => "GtOp" | GeOp => "GeOp");
			add_string pps ","; add_break pps (0, 1);
			add_string pps "right="; ppe right; add_string pps "}";
			end_block pps)
		| ppe(WhileExp({test, body}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "WhileExp{test=";
			ppe test; add_string pps ","; add_break pps (0, 0);
			add_string pps "body=";
			add_string pps "body="; ppe body; add_string pps "}";
			end_block pps)
		| ppe(ForExp({var, escape, lo, hi, body}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "ForExp{var=";
			add_string pps var; add_string pps ","; add_break pps (0, 0);
			add_string pps "escape=";
			add_string pps (Bool.toString(!escape));
			add_string pps ","; add_break pps (0, 0);
			add_string pps "lo="; ppe lo; add_string pps ",";
			add_break pps (0, 0);
			add_string pps "hi="; ppe hi; add_string pps ",";
			add_break pps (0, 0);
			add_string pps "body="; ppe body; add_string pps "}";
			add_break pps (0, 0);
			end_block pps)
		| ppe(AssignExp({var, exp}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "AssignExp{"; add_break pps (0, 1);
			add_string pps "var=";
			ppv var; add_string pps ","; add_break pps (0, 1);
			add_string pps "exp="; ppe exp; add_string pps "}";
			end_block pps)
		| ppe(IfExp({test, then', else'}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "IfExp{"; add_break pps (0, 0); 
			add_string pps "test=";
			ppe test; add_string pps ","; add_break pps (0, 0);
			add_string pps "then'=";
			ppe then'; add_string pps ","; add_break pps (0, 0);
			add_string pps "else'=";
			case else' of SOME e => ppe e | NONE => add_string pps "NONE";
			add_break pps (0, 0);
			add_string pps "}";
			end_block pps)
		| ppe(CallExp({func, args}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "CallExp{"; add_break pps (0, 0);
			add_string pps ("func="^func^","); add_break pps (0, 0);
			add_string pps "args=[";
			List.app (fn e => (ppe e; add_break pps (0, 0))) args;
			add_string pps "]}";
			add_break pps (0, 0);
			end_block pps)
		| ppe(SeqExp(explist, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "SeqExp([";
			List.app (fn e => (ppe e; add_break pps (0, 0))) explist;
			add_string pps "])"; add_break pps(0, 0);
			end_block pps)
		| ppe(RecordExp({fields, typ}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "RecordExp{"; add_break pps (0, 1);
			add_string pps "fields=["; add_break pps (0, 1);
			List.app
				(fn(s,e)=> (add_string pps ("("^s^",");
					ppe e; add_string pps ")"; add_break pps (0, 0)))
				fields;
			add_string pps "],"; add_break pps (0, 1);
			add_string pps "typ=";
			add_string pps typ;
			add_string pps "}";
			end_block pps)
		| ppe(ArrayExp({typ, size, init}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "ArrayExp{test=";
			add_string pps (typ^","); add_break pps (0, 1);
			add_string pps "size=";
			ppe size; add_string pps ","; add_break pps (0, 1);
			add_string pps "init=";
			ppe init; add_string pps "}";
			end_block pps)
		| ppe(LetExp({decs, body}, _)) =
			(begin_block pps INCONSISTENT 0;
			add_string pps "LetExp{decs=[";
				List.app ppd decs;
			add_string pps "],"; add_break pps (0, 1);
			add_string pps "body=";
			ppe body; add_string pps "}";
			end_block pps)
	in
		begin_block pps INCONSISTENT 0; 
		ppe e0;
		end_block pps
	end
val ppstrm =
	PP.mk_ppstream {
			consumer=fn s=>TextIO.output(TextIO.stdOut, s), 
			linewidth = 79,
			flush=fn()=>TextIO.flushOut TextIO.stdOut
	}

fun exprAst e =
	(ppexpr ppstrm e;
	flush_ppstream ppstrm;
	TextIO.output(TextIO.stdOut, "\n"))
