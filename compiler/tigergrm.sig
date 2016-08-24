local
in
datatype token =
    AMPER
  | ARRAY
  | BREAK
  | CD
  | CI
  | COMA
  | DIST
  | DIV
  | DO
  | DOSP
  | DOSPIG
  | ELSE
  | END
  | EOF
  | FOR
  | FUNCTION
  | ID of string
  | IF
  | IGUAL
  | IN
  | LD
  | LET
  | LI
  | LITERAL of string
  | MAS
  | MAYIG
  | MAYOR
  | MENIG
  | MENOR
  | MENOS
  | NIL
  | NRO of int
  | OF
  | PCOMA
  | PD
  | PI
  | PIPE
  | POR
  | PTO
  | THEN
  | TO
  | TYPE
  | VAR
  | WHILE
end;

val prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> tigerabs.exp;
