local open Obj Lexing in


open tigergrm

val num_linea = tigernlin.num_linea
val ncom = ref 0
val atoi = valOf o Int.fromString
fun inc r = r:=(!r+1)
fun dec r = (r:=(!r-1); !r)
fun aHex n =
	"\\x"^(if n<=15 then "0" else "")^
	(Int.fmt StringCvt.HEX n)
fun ctrl0 "\\n" = "\\x0a"
| ctrl0 "\\t" = "\\x09"
| ctrl0 _ = raise Fail "error interno 1 en lex!"
fun ctrl1 s =
	let	val c = ord(hd(tl(tl(explode s))))
	in	aHex(c-ord(#"@")) end
fun ctrl2 s =
	let	val n = ord(valOf(Char.fromString s))
	in	aHex n end
exception noEsta
val pal_claves = Polyhash.mkPolyTable(20, noEsta)
val _ = List.app (fn(c, v) => Polyhash.insert pal_claves (c, v))
	[("type",		TYPE),
	("array",		ARRAY),
	("of",			OF),
	("var",			VAR),
	("function",	FUNCTION),
	("let",			LET),
	("in",			IN),
	("end",			END),
	("if",			IF),
	("then",		THEN),
	("else",		ELSE),
	("while",		WHILE),
	("do",			DO),
	("for",			FOR),
	("to",			TO),
	("break",		BREAK),
	("nil",			NIL)]
fun clav_id(s) = (Polyhash.find pal_claves s) handle noEsta => ID s

fun action_50 lexbuf = (
 raise Fail("lex:["^getLexeme(lexbuf)^"]!") )
and action_49 lexbuf = (
 ID(getLexeme lexbuf) )
and action_48 lexbuf = (
 clav_id(getLexeme lexbuf) )
and action_47 lexbuf = (
 NRO(atoi(getLexeme lexbuf)) )
and action_46 lexbuf = (
 LITERAL(literal lexbuf) )
and action_45 lexbuf = (
 DIV )
and action_44 lexbuf = (
 POR )
and action_43 lexbuf = (
 MENOS )
and action_42 lexbuf = (
 MAS )
and action_41 lexbuf = (
 DIST )
and action_40 lexbuf = (
 MAYIG )
and action_39 lexbuf = (
 MAYOR )
and action_38 lexbuf = (
 MENIG )
and action_37 lexbuf = (
 MENOR )
and action_36 lexbuf = (
 PIPE )
and action_35 lexbuf = (
 AMPER )
and action_34 lexbuf = (
 LD )
and action_33 lexbuf = (
 LI )
and action_32 lexbuf = (
 CD )
and action_31 lexbuf = (
 CI )
and action_30 lexbuf = (
 PD )
and action_29 lexbuf = (
 PI )
and action_28 lexbuf = (
 IGUAL )
and action_27 lexbuf = (
 PCOMA )
and action_26 lexbuf = (
 COMA )
and action_25 lexbuf = (
 DOSPIG )
and action_24 lexbuf = (
 DOSP )
and action_23 lexbuf = (
 PTO )
and action_22 lexbuf = (
 Tok lexbuf )
and action_21 lexbuf = (
 inc num_linea; Tok lexbuf )
and action_20 lexbuf = (
 EOF )
and action_19 lexbuf = (
 inc ncom; Com lexbuf )
and action_18 lexbuf = (

							let	val s = getLexeme lexbuf
							in
								if s > "\^_" then s^literal(lexbuf)
								else raise Fail "caracter ilegal!"
							end
						)
and action_17 lexbuf = (
 literal1 lexbuf )
and action_16 lexbuf = (
 ctrl2(getLexeme lexbuf)^literal(lexbuf) )
and action_15 lexbuf = (
 ctrl1(getLexeme lexbuf)^literal(lexbuf) )
and action_14 lexbuf = (
 ctrl0(getLexeme lexbuf)^literal(lexbuf) )
and action_13 lexbuf = (
 getLexeme(lexbuf)^literal(lexbuf) )
and action_12 lexbuf = (
 "" )
and action_11 lexbuf = (
 raise Fail "NL en string!" )
and action_10 lexbuf = (
 raise Fail "string sin terminar! " )
and action_9 lexbuf = (
 raise Fail "\\\\ malformada!" )
and action_8 lexbuf = (
 literal lexbuf )
and action_7 lexbuf = (
 literal1 lexbuf )
and action_6 lexbuf = (
 inc num_linea; literal1 lexbuf )
and action_5 lexbuf = (
 raise Fail "string incompleta!" )
and action_4 lexbuf = (
 Com lexbuf )
and action_3 lexbuf = (
 inc num_linea; Com lexbuf )
and action_2 lexbuf = (
 raise Fail "coment incompleto!" )
and action_1 lexbuf = (
 inc ncom; Com lexbuf )
and action_0 lexbuf = (
 (if dec ncom=0 then Tok else Com) lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => state_62 lexbuf
 |  #"*" => state_61 lexbuf
 |  #"\n" => action_3 lexbuf
 |  #"\^@" => action_2 lexbuf
 |  _ => action_4 lexbuf
 end)
and state_1 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_54 lexbuf
 |  #"\r" => state_54 lexbuf
 |  #"\f" => state_54 lexbuf
 |  #" " => state_54 lexbuf
 |  #"\\" => action_8 lexbuf
 |  #"\n" => action_6 lexbuf
 |  #"\^@" => action_5 lexbuf
 |  _ => action_9 lexbuf
 end)
and state_2 lexbuf = (
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\\" => state_44 lexbuf
 |  #"\"" => action_12 lexbuf
 |  #"\n" => action_11 lexbuf
 |  #"\^@" => action_10 lexbuf
 |  _ => action_18 lexbuf
 end)
and state_3 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"a" andalso currChar <= #"z" then  state_27 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_24 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_18 lexbuf
 else case currChar of
    #"\t" => state_6 lexbuf
 |  #"\r" => state_6 lexbuf
 |  #"\f" => state_6 lexbuf
 |  #" " => state_6 lexbuf
 |  #"}" => action_34 lexbuf
 |  #"|" => action_36 lexbuf
 |  #"{" => action_33 lexbuf
 |  #"]" => action_32 lexbuf
 |  #"[" => action_31 lexbuf
 |  #">" => state_23 lexbuf
 |  #"=" => action_28 lexbuf
 |  #"<" => state_21 lexbuf
 |  #";" => action_27 lexbuf
 |  #":" => state_19 lexbuf
 |  #"/" => state_17 lexbuf
 |  #"." => action_23 lexbuf
 |  #"-" => action_43 lexbuf
 |  #"," => action_26 lexbuf
 |  #"+" => action_42 lexbuf
 |  #"*" => action_44 lexbuf
 |  #")" => action_30 lexbuf
 |  #"(" => action_29 lexbuf
 |  #"&" => action_35 lexbuf
 |  #"\"" => action_46 lexbuf
 |  #"\n" => action_21 lexbuf
 |  #"\^@" => action_20 lexbuf
 |  _ => action_50 lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_39 lexbuf
 |  #"\r" => state_39 lexbuf
 |  #"\f" => state_39 lexbuf
 |  #" " => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_45);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_19 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_18 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_47);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_37 lexbuf
 else backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_24);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_25 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_37);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_41 lexbuf
 |  #"=" => action_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_23 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_39);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_40 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_49);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_31 lexbuf
 else case currChar of
    #"_" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_27 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_48);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_32 lexbuf
 else case currChar of
    #"_" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_49);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_31 lexbuf
 else case currChar of
    #"_" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_32 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_48);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_32 lexbuf
 else case currChar of
    #"_" => state_31 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_47);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_37 lexbuf
 else backtrack lexbuf
 end)
and state_39 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_22);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_39 lexbuf
 |  #"\r" => state_39 lexbuf
 |  #"\f" => state_39 lexbuf
 |  #" " => state_39 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_44 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_17);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_46 lexbuf
 else case currChar of
    #"n" => action_14 lexbuf
 |  #"t" => action_14 lexbuf
 |  #"\"" => action_13 lexbuf
 |  #"\\" => action_13 lexbuf
 |  #"^" => state_47 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_46 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_50 lexbuf
 else backtrack lexbuf
 end)
and state_47 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"@" andalso currChar <= #"_" then  action_15 lexbuf
 else backtrack lexbuf
 end)
and state_50 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  action_16 lexbuf
 else backtrack lexbuf
 end)
and state_54 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_57 lexbuf
 |  #"\r" => state_57 lexbuf
 |  #"\f" => state_57 lexbuf
 |  #" " => state_57 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_57 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_7);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\t" => state_57 lexbuf
 |  #"\r" => state_57 lexbuf
 |  #"\f" => state_57 lexbuf
 |  #" " => state_57 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_61 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"/" => action_0 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_62 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_4);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"*" => action_1 lexbuf
 |  _ => backtrack lexbuf
 end)
and Tok lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_3 lexbuf)

and literal lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_2 lexbuf)

and literal1 lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_1 lexbuf)

and Com lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_50, action_49, action_48, action_47, action_46, action_45, action_44, action_43, action_42, action_41, action_40, action_39, action_38, action_37, action_36, action_35, action_34, action_33, action_32, action_31, action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19];
val _ = fn _ => [action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10];
val _ = fn _ => [action_9, action_8, action_7, action_6, action_5];
val _ = fn _ => [action_4, action_3, action_2, action_1, action_0];

end
