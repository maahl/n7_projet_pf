open Logo_types;;

let pi = 3.141592654;;
let deg2rad angle = pi *. angle /. 180.;;
let etat_initial = ((0., 0.), 0.);;

(*
 * fonction get_procedure
 * But : retourner la liste de params et l'arbre d'instructions d'une procedure a partir de son nom
 * Entree : le nom de la procedure et les defs ed l'env courant
 * Precondition : la fonction recherchee est bien dans l'env courant
 * Sortie : la procedure correspondant au nom fourni
 * Postcondition : -
 *)
let rec get_procedure nom_proc defs =
  match defs with
  | (nom, params, instructions)::defs' -> if nom_proc = nom then (nom, params, instructions) else get_procedure nom_proc defs'
  | _ -> failwith "rtfm noob (get_procedure)";;

(*
 * fonction get_valeur_param
 * But : trouver la valeur d'une Var
 * Entree : le nom de la Var en question et la liste de parametres de l'env courant
 * Precondition : la Var est bien declaree dans l'env
 * Sortie : la valeur associee a la Var
 * Postcondition : -
 *)
let rec get_valeur_param var params =
  match params with
  | (param, valeur)::params' -> if var = param then valeur else get_valeur_param var params'
  | _ -> failwith "rtfm noob (get_valeur_param)";;

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
  | Var(v) -> let (params, _) = env in evalue_expression env (get_valeur_param v params);;

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
  | _ -> failwith "rtfm noob (execute_instruction)";;

(*
 * fonction enrichir_env
 * But : associer les params a leurs valeurs dans l'env
 * Entree : les params de l'env courant, la liste de params, et la liste de valeurs
 * Precondition : -
 * Sortie : l'env enrichi
 * Postcondition : -
 *)
let rec enrichir_env params_env params_proc valeurs_params =
  (List.map2 (fun p v -> (p,v)) params_proc (List.map (fun x -> Const(evalue_expression (params_env, []) x)) valeurs_params))@params_env;;
  (* match (params_proc, valeurs_params) with
  | ([], []) -> params_env
  | (p::params_proc', v::valeurs_params') -> enrichir_env ((p,v)::params_env) params_proc' valeurs_params'
  | _ -> failwith "rtfm noob (enrichir_env)";; *)

let execute_programme (defs, instructions) =
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
    | Call(nom_proc, valeurs_params)::instructions' -> let (params, defs) = env 
                                                       in let (_, params_proc, instructions_proc) = get_procedure nom_proc defs
                                                       in let new_env = (enrichir_env params params_proc valeurs_params, defs)
                                                       in (eval_instructions instructions_proc new_env etat)@(eval_instructions instructions' env etat)

  (* la liste de procedures joue le role d'env initial *)
  in eval_instructions instructions ([], defs) etat_initial;;
