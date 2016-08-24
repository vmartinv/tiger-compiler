{
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
}

let SPC = [` ``\t``\r``\^L`]
let MN = [`a`-`z`]
let L = [`A`-`Z``a`-`z`]
let LDU = [`A`-`Z``a`-`z``0`-`9``_`]
let D = [`0`-`9`]

rule Tok = parse "/*"	{ inc ncom; Com lexbuf }
	| eof				{ EOF }
	| `\n`				{ inc num_linea; Tok lexbuf }
	| SPC+				{ Tok lexbuf }
	| `.`				{ PTO }
	| `:`				{ DOSP }
	| ":="				{ DOSPIG }
	| `,`				{ COMA }
	| `;`				{ PCOMA }
	| `=`				{ IGUAL }
	| `(`				{ PI }
	| `)`				{ PD }
	| `[`				{ CI }
	| `]`				{ CD }
	| `{`				{ LI }
	| `}`				{ LD }
	| `&`				{ AMPER }
	| `|`				{ PIPE }
	| `<`				{ MENOR }
	| "<="				{ MENIG }
	| `>`				{ MAYOR }
	| ">="				{ MAYIG }
	| "<>"				{ DIST }
	| `+`				{ MAS }
	| `-`				{ MENOS }
	| `*`				{ POR }
	| `/`				{ DIV }
	| `"`				{ LITERAL(literal lexbuf) }
	| D+				{ NRO(atoi(getLexeme lexbuf)) }
	| MN+				{ clav_id(getLexeme lexbuf) }
	| L LDU*			{ ID(getLexeme lexbuf) }
	| _					{ raise Fail("lex:["^getLexeme(lexbuf)^"]!") }
and literal = parse eof	{ raise Fail "string sin terminar! " }
	| `\n`				{ raise Fail "NL en string!" }
	| `"`				{ "" }
	| "\\"[`"``\\`]		{ getLexeme(lexbuf)^literal(lexbuf) }
	| "\\"[`n``t`]		{ ctrl0(getLexeme lexbuf)^literal(lexbuf) }
	| "\\^"[`@`-`_`]	{ ctrl1(getLexeme lexbuf)^literal(lexbuf) }
	| "\\"D D D			{ ctrl2(getLexeme lexbuf)^literal(lexbuf) }
	| "\\"				{ literal1 lexbuf }
	| _					{
							let	val s = getLexeme lexbuf
							in
								if s > "\^_" then s^literal(lexbuf)
								else raise Fail "caracter ilegal!"
							end
						}
and literal1 =
	parse eof			{ raise Fail "string incompleta!" }
	| `\n`				{ inc num_linea; literal1 lexbuf }
	| SPC+				{ literal1 lexbuf }
	| "\\"				{ literal lexbuf }
	| _ 				{ raise Fail "\\\\ malformada!" }
and Com = parse "*/"	{ (if dec ncom=0 then Tok else Com) lexbuf }
	| "/*"				{ inc ncom; Com lexbuf }
	| eof				{ raise Fail "coment incompleto!" }
	| `\n`				{ inc num_linea; Com lexbuf }
	| _					{ Com lexbuf }
;
