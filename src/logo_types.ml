(* type des expressions arithmétiques *)
type expr =
  Plus of expr * expr
| Moins of expr * expr
| Div of expr * expr
| Mult of expr* expr
| Const of float
| Var of string
| Cosinus of expr
| Sinus of expr
| Tangente of expr;;
(* type des expressions booléennes*)
type test =
  Equal of expr * expr
| InfEq of expr * expr
| And   of test * test
| Or    of test * test
| Not   of test;;
(* type des mots clés du langage *)
type mot =
  IF
| THEN
| ELSE
| BEGIN
| END
| REPEAT
| DEF
| CALL
| ROTATE
| MOVE
| JUMP
| COLOR
| FIN
| EXPR of expr
| TEST of test
| IDENT of string;;
(* type des commandes à générer pour afficher les figures *)
type cmd =
        Change_color of Graphics.color        
        |Moveto of float*float
        |Jumpto of float*float
;;
(* type des instructions : A DEFINIR *)
type instruction;;
(* type des définitions de procedure : A DEFINIR *)
type definition;;
(* type des programmes : A DEFINIR *)
type programme;;

(* type des environnements : A DEFINIR *)
type environnement;;
(* type des états du système : A DEFINIR *)
type etat;;


