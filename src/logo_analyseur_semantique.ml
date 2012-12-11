let evalue_expression env expr =
  match expr with
  | Const c -> c
  | Plus(a, b) -> (evalue_expression env a) +. (evalue_expression env b)
  | Moins(a, b) -> (evalue_expression env a) -. (evalue_expression env b)
  | Div(a, b) -> (evalue_expression env a) /. (evalue_expression env b)
  | Mult(a, b) -> (evalue_expression env a) *. (evalue_expression env b)
  | Cosinus(t) -> cos (evalue_expression env t)
  | Sinus(t) -> sin (evalue_expression env t)
  | Tangente(t) -> tan (evalue_expression env t)
  | Var(v) -> failwith "TODO";;

let evalue_condition env test = 
  match test with
  | Equal(a,b) -> ((evalue_expression env a) = (evalue_expression env b))
  | InfEq(a,b) -> ((evalue_expression env a) <= (evalue_expression env b))
  | And(a,b) -> ((evalue_expression env a) && (evalue_expression env b))
  | Or(a,b) -> ((evalue_expression env a) || (evalue_expression env b))
  | Not(a) -> not (evalue_expression env a);;

let execute_instruction environnement instruction etat =
	failwith "A Faire";;

let execute_programme programme =
	failwith "A Faire";;
