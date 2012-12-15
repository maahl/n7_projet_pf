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

(* type parametre *)
type parametre = string;;
(* type instruction elementaire sous forme d'arbre *)
type instruction =
  | Move of expr
  | Jump of expr
  | Rotate of expr
  | Color of expr * expr * expr
  | If of (test * instruction list * instruction list)
  | Repeat of (expr * instruction list)
  | Call of (string * expr list);;

(* type procedure : nom de la procedure * liste des params * liste des sous-programmes *)
type definition = string * parametre list * instruction list;;
(* Type programme : liste des procedures * liste des instructions de premier niveau (d'indentation) *)
type programme = definition list * instruction list;;

(* type des environnements : une liste de variables et une liste de procedures *)
type environnement = (parametre * expr) list * definition list;;
(* type des etats du systeme : la position du curseur et son angle de visee en degres *)
type etat = (float*float) * float;;
