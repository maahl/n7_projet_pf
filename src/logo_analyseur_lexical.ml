open Logo_types;;
open Logo_lexer;;
open Logo_parser;;

let analyseur_lexical fich = 
	let rec parse lexbuf =
	let tok = Logo_parser.parse Logo_lexer.token lexbuf in
     		try tok@(parse lexbuf)
     		with _ -> tok
	in
	let lexbuf = Lexing.from_channel (open_in fich) in
	parse lexbuf;;

