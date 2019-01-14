structure tigerregalloc :> tigerregalloc =
struct

open tigerutils
open tigercolor
open tigerframe
open tigerassem

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

fun spill spillList frame instrs = (let
    (* structure where the new instructions are being saved *)
    val ilist = ref ([]:(instr list))
    val tlist = ref ([]:(tigertemp.temp list))
    fun emit x = ilist := x::(!ilist)
    fun newt() = let val t = tigertemp.newtemp() in tlist := t::(!tlist) ; t end

    (* access of each temporary to spill *)
    val spillAlloc = List.map ( fn(x) => (x,tigerframe.allocLocal frame true) ) spillList
    fun peek t = Option.map (fn(x,a)=>tigerframe.offset a) ( List.find ( fn(x,a) => x=t ) spillAlloc )
    
    (* function that generate instructions to load and store tempraries *)
    fun load t k = emit(OPER{assem = "movq "^(toString k)^"(%'s0), %'d0", src=[tigerframe.fp],dst=[t],jump=NONE})
    fun store t k = emit(OPER{assem = "movq %'s1, "^(toString k)^"(%'s0)", src=[tigerframe.fp,t],dst=[],jump=NONE})

    (* function that actually transforms the code *)
    fun oneinstr (OPER{src,dst,assem,jump}) = let
        fun auxf x = Option.map (fn k=>(k,newt())) (peek x)
        val auxsrc = List.map auxf src
        val auxdst = List.map auxf dst
        fun newf aux old = List.map (fn(x,y) => Option.getOpt(Option.map #2 x,y)) (ListPair.zip( aux, old ))
        val newsrc = newf auxsrc src
        val newdst = newf auxdst dst
        fun exec f l = List.app ( fn(SOME (k,t)) => f t k | NONE => () ) l
        in exec load auxsrc ; emit(OPER{assem=assem,src=newsrc,dst=newdst,jump=jump}) ; exec store auxdst  end
      | oneinstr (inst as MOV{src,dst,...}) = (* this special case helps reduce the number of temporaries needed*)
            ( case (peek src,peek dst) of
                   (NONE,NONE) => emit(inst)
                 | (SOME k,NONE) => load dst k
                 | (NONE,SOME k) => store src k
                 | (SOME k1, SOME k2) => let val t = newt() in load t k1 ; store t k2 end )
      | oneinstr (aLABEL x) = emit(aLABEL x)
    in
        List.app oneinstr instrs;
        (rev(!ilist),!tlist)
end)

(* Rewrite the instructions if there are spilled nodes *)
fun RewriteProgram (spilledNodes : tigertemp.temp list, body : tigerassem.instr list, fr : tigerframe.frame) =
    let
(*
        val _ = List.app (fn t => print ("spilling node: "^t^"\n")) spilledNodes (* DEBUG *)
*)
        val (lInstr', tlist) = spill spilledNodes fr body
    in
        lInstr'
    end

(* Register Allocation *)
fun alloc (body : tigerassem.instr list, fr : tigerframe.frame) =
    let
        val (allocation, spilledNodes) = tigercolor.color(body, fr)
    in
        if (null spilledNodes)
        then
            (body, allocation)
        else
            let val newbody = RewriteProgram(spilledNodes, body, fr)
            in  alloc(newbody, fr)
            end
    end
end
