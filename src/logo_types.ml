(* type des expressions arithmetiques *)
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
(* type des expressions booleennes*)
type test =
  Equal of expr * expr
| InfEq of expr * expr
| And   of test * test
| Or    of test * test
| Not   of test;;
(* type des mots cles du langage *)
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
(* type des commandes a generer pour afficher les figures *)
type cmd =
        Change_color of Graphics.color        
        |Moveto of float*float
        |Jumpto of float*float
;;

(* type des definitions de procedure : TODO *)
type definition;;

(* type parametre *)
type parametre;;
(* type instruction elementaire sous forme d'arbre *)
type instruction = 
  | Move of expr
  | Jump of expr
  | Rotate of expr
  | Color of Graphics.color
  | If of (test * instruction list * instruction list)
  | Repeat of (expr * instruction list);;
(* type procedure : liste des params * liste des sous-programmes *)
type procedure = parametre list * instruction list;;
(* Type programme : liste des procedures * liste des instructions de premier
 * niveau (d'indentation) *)
type programme = procedure list * instruction list;;

(* type des environnements : TODO *)
type environnement;;
(* type des etats du systeme : TODO *)
type etat;;


