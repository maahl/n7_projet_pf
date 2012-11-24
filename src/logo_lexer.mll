
{

open Logo_parser;;

let line_g = ref (ref 0);;

exception ErreurLex of string;;

}

rule token =
 parse
  [' ''\t''\r']+ { token lexbuf }
| '\n'      { incr !line_g; token lexbuf }
| "(*"      { comment lexbuf }
| "CALL"    { CALL }
| "DEF"     { DEF }
| "BEGIN"   { BEGIN }
| "END"     { END }
| "IF"      { IF }
| "THEN"    { THEN }
| "ELSE"    { ELSE }
| "REPEAT"  { REPEAT }
| "MOVE"    { MOVE }
| "JUMP"    { JUMP }
| "ROTATE"  { ROTATE }
| "COLOR"   { COLOR }
| "cos"     { COSINUS }
| "sin"     { SINUS }
| "tan"     { TANGENTE }
| "+"       { PLUS }
| "-"       { MOINS }
| "*"       { MULT }
| "/"       { DIV }
| "("       { PARO }
| ")"       { PARF }
| "="       { EQU }
| "<="      { INFEQ }
| ">="      { SUPEQ }
| "<"       { INF }
| ">"       { SUP }
| "&&"      { AND }
| "||"      { OR  }
| "not"     { NOT }
| ","       { VIRG }
| ['0'-'9']+('.'['0'-'9']*)?  { NUM (float_of_string (Lexing.lexeme lexbuf)) }
| '.'['0'-'9']+               { NUM (float_of_string (Lexing.lexeme lexbuf)) }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*  { IDENT (Lexing.lexeme lexbuf) }
| _         { raise (ErreurLex "lexeme inconnu !") }

and comment =
 parse
  eof  { raise (ErreurLex "commentaire non fermé !") }
| "*)" { token lexbuf }
| '\n' { incr !line_g; comment lexbuf }
| _    { comment lexbuf }

and trash =
 parse
  eof  { FIN }
| _    { trash lexbuf }



