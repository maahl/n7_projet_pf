open Logo_types;;

let rec evalue_expression env expr =
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

let rec evalue_condition env test = 
  match test with
  | Equal(a,b) -> ((evalue_expression env a) = (evalue_expression env b))
  | InfEq(a,b) -> ((evalue_expression env a) <= (evalue_expression env b))
  | And(a,b) -> ((evalue_condition env a) && (evalue_condition env b))
  | Or(a,b) -> ((evalue_condition env a) || (evalue_condition env b))
  | Not(a) -> not (evalue_condition env a);;

let execute_instruction env instruction etat =
	failwith "A Faire";;

let etat_initial = ((0., 0.), 0., Graphics.black);;
let env_initial = [];;

let execute_programme (defs, instructions) =
  let rec eval_defs defs env =
    failwith "TODO: eval_defs"
  in
  let rec eval_instructions instructions env etat =
    match instructions with
    | [] -> []
    | Move(e)::instructions'   -> let (cmd, _, nouvel_etat) = (execute_instruction env (Move(e)) etat) in
                                    cmd::(eval_instructions instructions' env nouvel_etat)
    | Jump(e)::instructions'   -> let (cmd, _, nouvel_etat) = (execute_instruction env (Jump(e)) etat) in
                                    cmd::(eval_instructions instructions' env nouvel_etat)
    | Rotate(e)::instructions' -> let (_, _, nouvel_etat) = (execute_instruction env (Rotate(e)) etat) in
                                    (eval_instructions instructions' env nouvel_etat)
    | Color(c)::instructions'  -> let (cmd, _, nouvel_etat) = (execute_instruction env (Color(c)) etat) in
                                    cmd::(eval_instructions instructions' env nouvel_etat)
    | If(test, instructions_if, instructions_else)::instructions' 
                               -> if evalue_condition env test then 
                                    eval_instructions (instructions_if@instructions') env etat
                                  else
                                    eval_instructions (instructions_else@instructions') env etat
    | Repeat(expr, instructions_rpt)::instructions'
                               -> let x = evalue_expression env expr in 
                                    if x > 0. then
                                      eval_instructions ((Repeat(Const(x-.1.), instructions_rpt))::(instructions_rpt@instructions')) env etat
                                    else
                                      eval_instructions instructions' env etat
    | Call::instructions' -> failwith "TODO: eval_instructions->Call"



  in let env = eval_defs defs env_initial in eval_instructions instructions env etat_initial;;
