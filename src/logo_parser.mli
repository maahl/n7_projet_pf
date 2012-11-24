type token =
  | MOVE
  | JUMP
  | ROTATE
  | CALL
  | DEF
  | BEGIN
  | END
  | IF
  | THEN
  | ELSE
  | REPEAT
  | VIRG
  | COLOR
  | EQU
  | INFEQ
  | SUPEQ
  | INF
  | SUP
  | AND
  | OR
  | NOT
  | PLUS
  | MOINS
  | MULT
  | DIV
  | PARO
  | PARF
  | COSINUS
  | SINUS
  | TANGENTE
  | FIN
  | NUM of (float)
  | IDENT of (string)

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logo_types.mot list
