open Logo_types;;

(* Execute successivement une liste de commandes :	*)
(* - une commande Moveto dessine un segment de la 	*)
(* position courante à la position passée en paramètre	*)
(* - une commande Jumpto déplace la position courante 	*)
(* - une commande Change_color change la couleur des	*)
(* futurs segments					*)
val dessine : cmd list -> unit;;
