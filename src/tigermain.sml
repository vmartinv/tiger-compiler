open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigerinclude
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun compile arbol escapes ir canon code flow inter manyfiles source =
    let val dir = Path.dir source
        val entrada = ((open_in source)
					handle _ => raise Fail (source^" no existe!"))
        fun pass f x = (f x; x)
        val _ = (tigertrans.clearResult(); tigernlin.num_linea := 1; tigernlin.file_name := (if manyfiles then source else ""))

        (*Printers*)
        val prntArbol = pass (fn x=> if arbol then tigerpp.exprAst x else ())
		val prntIr = pass (fn x => if ir then print(tigertrans.Ir(x)) else ())

        (*Etapas de la compilacion*)
        val lexer = lexstream
        fun parser l = prog Tok l handle _ => errParsing l
        val expIncudes = expandIncludes dir
        val escap = pass findEscape
        fun seman x = (transProg x; tigertrans.getResult())
    in
        (prntIr o seman o prntArbol o escap o expIncudes o parser o lexer) entrada;
		print ((if manyfiles then source^": " else "")^"yes!!\n")
    end

fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
        
        fun isTig s = (".tig" = String.extract (s ,(size(s)-4), NONE))
        val files = List.filter isTig l7
	in
		List.app (compile arbol escapes ir canon code flow inter (length files>1)) files
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
