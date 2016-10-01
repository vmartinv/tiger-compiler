structure tigerinclude :> tigerinclude =
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

fun expandIncludes dir prog =
	let fun trdec s (IncludeDec({name}, nl)) =
            let
                val fname = Path.concat(dir, name)^".tigd"
                val _ = if Binaryset.member(s, fname) then error("Ciclo en los includes", nl) else ()
                val s' = Binaryset.add(s, fname)
                val entrada = open_in fname handle _ => error(fname^" no existe!", nl)
                val lexbuf = lexstream entrada
                val decs = (num_linea := 1; file_name := name^":"; modu Tok lexbuf handle _ => errParsing fname lexbuf)
            in
                trdecs s' decs
            end
        | trdec _ x = [x]
        and trdecs s decs = flatten (map (trdec s) decs)
        and trexp s (LetExp({decs, body}, nl)) = LetExp({decs=trdecs s decs, body=body}, nl)
        | trexp _ x = x 
	in
        trexp (Binaryset.empty String.compare) prog
    end

end
