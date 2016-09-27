structure tigertree =
struct
	datatype exp = CONST of int
		     | NAME of tigertemp.label
		     | TEMP of tigertemp.temp
		     | BINOP of binop*exp*exp
		     | MEM of exp
		     | CALL of exp*exp list
		     | ESEQ of stm*exp
	and stm = MOVE of exp*exp
		| EXP of exp
		| JUMP of exp*tigertemp.label list
		| CJUMP of relop*exp*exp*tigertemp.label*tigertemp.label
		| SEQ of stm*stm
		| LABEL of tigertemp.label
	and binop = PLUS | MINUS | MUL | DIV | AND | OR
		  | LSHIFT | RSHIFT | ARSHIFT | XOR
	and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE
		  | UGT | UGE

	fun notRel EQ = NE
	  | notRel NE = EQ
	  | notRel LT = GE
	  | notRel GE = LT
	  | notRel GT = LE
	  | notRel LE = GT
	  | notRel ULT = UGE
	  | notRel UGE = ULT
	  | notRel ULE = UGT
	  | notRel UGT = ULE
end
