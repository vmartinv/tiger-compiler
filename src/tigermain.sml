open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigerinclude
open tigerutils
open tigercanon
open tigercodegen
open tigerassem
open BasicIO Nonstdio

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
    ^(makestring(!num_linea))^
    ")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun compile arbol escapes ir canon code flow inter source =
    let fun pass f x = (f x; x)
        infix >>=
        fun m >>= f = f m

        (*Printers*)
        val prntArbol = pass (fn x=> if arbol then tigerpp.exprAst x else ())
        val prntIr = pass (fn x => if ir then print(tigertrans.Ir(x)) else ())
        val prntCanon = pass (fn x => if canon then print("------Canon------\n"^tigercanon.Canon(x)) else ())
        val prntCode = pass (fn (b, f) => if code then print(";;--FRAME--"^(tigerframe.name f)^":\n"^tigerassem.printCode b^";;-END-FRAME-:\n") else ())
        fun prntFlow fr instr g = if flow then print(";;--FLOW--"^(tigerframe.name fr)^":\n"^(tigerflow.printGraph (instr, g))^";;-END-FLOW-:\n") else ()
        fun prntInter fr instr g live_out = if inter then print(";;--INTER--"^(tigerframe.name fr)^":\n"^(tigerliveness.printInter (instr, g, live_out))^";;-END-INTER-:\n") else ()
        fun prntOk _ = print "yes!!\n"
        
        (*Etapas de la compilacion*)
        fun lee_archivo file = ((open_in file)
                    handle _ => raise Fail (file^" no existe!"))
        val lexer = lexstream
        fun parser l = prog Tok l handle _ => errParsing l
        val expIncludes = expandIncludes (Path.dir source)
        val escap = pass findEscape
        fun seman x = (transProg x; tigertrans.getResult())
 
        fun instructionSel (body, frame) = 
			let val instrs = tigercodegen.codegens frame body
			in (instrs, frame)
			end
		fun livenessAnalysis (instrs, frame) =
			let val (flowgraph, nodes) = tigerflow.instrs2graph instrs
				val _ = prntFlow frame instrs flowgraph
				val (igraph, live_out) = tigerliveness.interferenceGraph flowgraph
				val _ = prntInter frame instrs igraph live_out
			in (igraph, flowgraph, frame) (*COMPLETAR*)
			end

		(*Pipeline ejecutado por cada fragmento*)
		fun perFragment fragment = 
			fragment >>= instructionSel >>= prntCode >>=
				livenessAnalysis
    in
		(*Pipeline del compilador*)
        source >>= lee_archivo >>= 
           lexer >>= parser >>= (*de ASCII al arbol tigerabs.exp*)
           expIncludes >>=  (*etapa agregada para que funcionen los includes*)
           escap >>= prntArbol >>= 
           seman >>= prntIr >>= (*chequeo de tipos y generacion de fragmentos*)
           canonize >>= prntCanon >>=
           (fn (stringList, frags) => (stringList, map perFragment frags)) >>= 
           prntOk (*si llega hasta aca esta todo ok*)
    end

fun main(args) =
    let fun arg(l, s) =
            (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
        val usage = "Usage:\n\ttiger [-arbol] [-escapes] [-ir] [-canon] [-code] [-flow] [-inter] FILE.tig\n"
        val (arbol, l1)     = arg(args, "-arbol")
        val (escapes, l2)   = arg(l1, "-escapes") 
        val (ir, l3)        = arg(l2, "-ir") 
        val (canon, l4)     = arg(l3, "-canon") 
        val (code, l5)      = arg(l4, "-code") 
        val (flow, l6)      = arg(l5, "-flow") 
        val (inter, l7)     = arg(l6, "-inter") 
        
        val file = case List.filter (endswith ".tig") l7 of
                [file] => file
                | _ => (print usage; raise Fail "No hay archivos de entrada!")
    in
         compile arbol escapes ir canon code flow inter file
    end handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
