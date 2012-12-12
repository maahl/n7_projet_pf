open Logo_types;;

let pi = 3.141592654;;
let deg2rad angle = pi *. angle /. 180.;;

let rec evalue_expression env expr =
  match expr with
  | Const c -> c
  | Plus(a, b) -> (evalue_expression env a) +. (evalue_expression env b)
  | Moins(a, b) -> (evalue_expression env a) -. (evalue_expression env b)
  | Div(a, b) -> (evalue_expression env a) /. (evalue_expression env b)
  | Mult(a, b) -> (evalue_expression env a) *. (evalue_expression env b)
  | Cosinus(t) -> cos (deg2rad(evalue_expression env t))
  | Sinus(t) -> sin (deg2rad(evalue_expression env t))
  | Tangente(t) -> tan (deg2rad(evalue_expression env t))
  | Var(v) -> failwith "TODO";;

let rec evalue_condition env test = 
  match test with
  | Equal(a,b) -> ((evalue_expression env a) = (evalue_expression env b))
  | InfEq(a,b) -> ((evalue_expression env a) <= (evalue_expression env b))
  | And(a,b) -> ((evalue_condition env a) && (evalue_condition env b))
  | Or(a,b) -> ((evalue_condition env a) || (evalue_condition env b))
  | Not(a) -> not (evalue_condition env a);;

let execute_instruction env instruction ((x,y), angle) =
  match instruction with
  | Move(e) -> let distance = evalue_expression env e in
               let (new_x, new_y) = ((cos (deg2rad angle))*.distance +. x, (sin (deg2rad angle))*.distance +. y) in 
                 ((new_x, new_y), angle)
  | Jump(e) -> let distance = evalue_expression env e in
               let (new_x, new_y) = ((cos (deg2rad angle))*.distance +. x, (sin (deg2rad angle))*.distance +. y) in
                 ((new_x, new_y), angle)
  | Rotate(t) -> let rotation = evalue_expression env t in
                   ((x, y), angle+.rotation)
  | _ -> failwith "execute_instruction: this was not supposed to happen :(";;

let etat_initial = ((0., 0.), 0.);;
let env_initial = [];;

let execute_programme (defs, instructions) =
  let rec eval_defs defs env =
    failwith "TODO: eval_defs"
  in
  let rec eval_instructions instructions env etat =
    match instructions with
    | [] -> []
    | Move(e)::instructions'   -> let ((x, y), angle) = execute_instruction env (Move(e)) etat in
                                    Moveto(x, y)::(eval_instructions instructions' env ((x, y), angle))
    | Jump(e)::instructions'   -> let ((x, y), angle) = execute_instruction env (Jump(e)) etat in
                                    Jumpto(x, y)::(eval_instructions instructions' env ((x, y), angle))
    | Rotate(t)::instructions' -> let nouvel_etat = (execute_instruction env (Rotate(t)) etat) in
                                    eval_instructions instructions' env nouvel_etat
    | Color(r, g, b)::instructions'  -> Change_color(Graphics.rgb (int_of_float (evalue_expression env r)) 
                                                                  (int_of_float (evalue_expression env g)) 
                                                                  (int_of_float (evalue_expression env b))
                                                    )::(eval_instructions instructions' env etat)
    | If(test, instructions_if, instructions_else)::instructions' 
                               -> if evalue_condition env test then 
                                    eval_instructions (instructions_if@instructions') env etat
                                  else
                                    eval_instructions (instructions_else@instructions') env etat
    | Repeat(expr, instructions_rpt)::instructions'
                               -> let x = evalue_expression env expr in 
                                    if x > 0. then
                                      eval_instructions ((Repeat(Const(x-.1.), 
                                                                 instructions_rpt)
                                                         )::(instructions_rpt@instructions')) env etat
                                    else
                                      eval_instructions instructions' env etat
    | Call::instructions' -> failwith "TODO: eval_instructions->Call"

  (* in let env = eval_defs defs env_initial *) 
  in eval_instructions instructions env_initial etat_initial;;
