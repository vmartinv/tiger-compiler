structure tigerregalloc :> tigerregalloc =
struct

open tigerutils
open tigercolor
open tigerframe

type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict

(*
fun alloc (body, fr) = (* COMPLETAR *)
    ([], Splaymap.mkDict(String.compare))
*)


(* Rewrite the instructions if there are spilled nodes *)
fun RewriteProgram (spilledNodes : tigertemp.temp list, body : tigerassem.instr list, fr : tigerframe.frame) = (* COMPLETAR *)
	body


(* Register Allocation *)
fun alloc (body : tigerassem.instr list, fr : tigerframe.frame) =
    let
        val (allocation, spilledNodes) = tigercolor.color(body, fr)
    in
        if (null spilledNodes)
        then
            (body, allocation)
        else
            let val newbody =  RewriteProgram(spilledNodes, body, fr)
            in  alloc(newbody, fr)
            end
    end
    
end
