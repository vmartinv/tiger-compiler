structure tigerimport :> tigerimport =
struct
open tigerabs
open tigerlex
open tigergrm
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun expandImports prog =
	let fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
		fun trexp(LetExp({decs, body}, nl)) = LetExp({decs=List.foldl (op@) [] (map trdec decs), body=body}, nl)
        | trexp x = x
		and trvar x = x
		and trdec(ImportDec({name}, nl)) =
            let
                val fname = name^".tigd"
                val entrada = open_in fname
                                handle _ => error(fname^" no existe!", nl)
                val lexbuf = lexstream entrada
            in
                decs Tok lexbuf handle _ => errParsing lexbuf
            end
        | trdec x = [x]
	in trexp prog end

end
