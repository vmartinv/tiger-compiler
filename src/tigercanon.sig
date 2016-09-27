signature tigercanon = 
sig
(* From an arbitrary Tree statement, produce a list of cleaned trees
  satisfying the following properties:
  1.  No SEQ's or ESEQ's
  2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
*)
val linearize : tigertree.stm -> tigertree.stm list

(* From a list of cleaned trees, produce a list of
	 basic blocks satisfying the following properties:
	      1. and 2. as above;
	      3.  Every block begins with a LABEL;
              4.  A LABEL appears only at the beginning of a block;
              5.  Any JUMP or CJUMP is the last stm in a block;
              6.  Every block ends with a JUMP or CJUMP;
   Also produce the "label" to which control will be passed
   upon exit.
*)
val basicBlocks :
	tigertree.stm list -> (tigertree.stm list list * tigertree.label)

(* From a list of basic blocks satisfying properties 1-6,
   along with an "exit" label, produce a list of stms such that:
	      1. and 2. as above;
          7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
   The blocks are reordered to satisfy property 7; also
   in this reordering as many JUMP(T.NAME(lab)) statements
   as possible are eliminated by falling through into T.LABEL(lab).
*)
val traceSchedule :
	tigertree.stm list list * tigertree.label -> tigertree.stm list
end

