open Logo_types;;
open Logo_analyseur_lexical;;
open Logo_analyseur_syntaxique;;
open Logo_analyseur_semantique;;

let lmot = analyseur_lexical "../EXEMPLES/menger2d.logo";;
let prog = analyseur_syntaxique lmot;;
