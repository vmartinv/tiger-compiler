 signature tigerframe =
sig

type frame
type register = string
val rax : tigertemp.temp
val rdx : tigertemp.temp
val rv : tigertemp.temp (* return value *)
val ov : tigertemp.temp (* overflow value *)
val fp : tigertemp.temp (* frame pointer *)
val sp : tigertemp.temp (* stack pointer *)

type access (* describes formals and locals that may be in frame or registers *)

val fpPrevLev : int
val newFrame : {name: tigertemp.label, formals: bool list} -> frame (* name -> function name, formals -> function's formal parameters (t = escape) *)
val name : frame -> tigertemp.label
val string : tigertemp.label * string -> string
val formals : frame -> access list
val allocLocal : frame -> bool -> access (* used to allocate a new local variable in the frame (t) or register (f) *)
(*
val maxRegFrame : frame -> int
*)
val wSz : int
val log2WSz : int
val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
val specialregs : tigertemp.temp list
val coloredregisters : tigertemp.temp list
val argregs : register list
val exp : access -> tigertree.exp -> tigertree.exp
val offset : access -> int
val externalCall : string * tigertree.exp list -> tigertree.exp
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list
(*
val procEntryExit3 : frame * tigerassem.instr list -> {prolog:string, body: tigerassem.instr list, epilog: string}
*)
datatype frag = PROC of {body: tigertree.stm, frame: frame}
    | STRING of tigertemp.label * string

end
