structure tigerimport :> tigerimport =
struct
open tigerabs
open tigerlex
open tigergrm
open tigernlin
open tigerutils
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing fname lbuf = (print("Error parseando archivo "^fname^"!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun expandImports prog =
	let fun trexp(LetExp({decs, body}, nl)) = LetExp({decs=List.foldr (op@) [] (map trdec decs), body=body}, nl)
        | trexp x = x
		and trdec(ImportDec({name}, nl)) =
            let
                val fname = name^".tigd"
                val entrada = open_in fname
                                handle _ => error(fname^" no existe!", nl)
                val lexbuf = lexstream entrada
            in
                num_linea := 1; file_name := name^":"; modu Tok lexbuf handle _ => errParsing fname lexbuf
            end
        | trdec x = [x]
	in trexp prog end

end
