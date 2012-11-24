open Logo_types;;
open Logo_lexer;;

(* Convertit un fichier dont le chemin d'accès est en paramètre	*)
(*  en une liste de Logo_types.mot				*)
val analyseur_lexical : string -> Logo_types.mot list ;;
