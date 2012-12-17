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

(*
 * fonction evalue_expression
 * But : evaluer la valeur de l'expression passee en param
 * Entree : l'expression a evaluer
 * Precondition : -
 * Sortie : la valeur flottante de l'expression
 * Postcondition : -
 *)
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

(*
 * fonction evalue_condition
 * But : Evaluer la valeur booleenne d'une condition
 * Entree : la condition
 * Precondition : -
 * Sortie : la valeur de la condition
 * Postcondition : -
 *)
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

(*
 * fonction etat_fin_bloc
 * But : Faire remonter l'etat de fin d'un call pour reprendre l'exec depuis le bon endroit
 * Entree : l'etat de fin du bloc et l'etat courant
 * Precondition : -
 * Sortie : l'etat approprie
 * Postcondition : -
 *)
let etat_fin_bloc etat_courant etat_suivant =
  match etat_suivant with
  | None -> etat_courant
  | Some(e) -> e;;

(*
 * fonction execute_programme
 * But : transformer un arbre d'instructions en une liste de commandes interpretables par la fonction de dessin
 * Entree : l'arbre d'execution du programme
 * Precondition : -
 * Sortie : l'etat a la fin du bloc et la liste de cmd correspondant a l'arbre d'instructions
 * Postcondition : -
 *)
let execute_programme (defs, instructions) =
  let rec eval_instructions instructions env etat =
    match instructions with
    | [] -> (None, [])
    (* Move: on recupere le nouvel etat du curseur apres l'instruction Move, et on ajoute la cmd correspondante *)
    | Move(e)::instructions' ->
        let ((x, y), angle) = execute_instruction env (Move(e)) etat in
        let (etat_suivant, cmd_list) = eval_instructions instructions' env ((x, y), angle) in
        ( Some(etat_fin_bloc ((x, y), angle) etat_suivant), Moveto(x, y)::cmd_list )
    (* Jump: idem *)
    | Jump(e)::instructions' -> 
        let ((x, y), angle) = execute_instruction env (Jump(e)) etat in
        let (etat_suivant, cmd_list) = eval_instructions instructions' env ((x, y), angle) in
        ( Some(etat_fin_bloc ((x, y), angle) etat_suivant), Jumpto(x, y)::cmd_list )
    (* Rotate: idem *)
    | Rotate(t)::instructions' -> 
        let etat_courant = (execute_instruction env (Rotate(t)) etat) in
        let (etat_suivant, cmd_list) = eval_instructions instructions' env etat_courant in 
        ( Some(etat_fin_bloc etat etat_suivant), cmd_list)
    (* Color: on ajoute la cmd correspondante *)
    | Color(r, g, b)::instructions'  -> 
        let (etat_suivant, cmd_list) = eval_instructions instructions' env etat in
        (Some(etat_fin_bloc etat etat_suivant), Change_color(Graphics.rgb (int_of_float (evalue_expression env r)) 
                                                                          (int_of_float (evalue_expression env g)) 
                                                                          (int_of_float (evalue_expression env b))
                                                            )::cmd_list)
    (* If: si la condition est vraie, on execute le premier bloc d'instructions, le deuxieme sinon *)
    | If(test, instructions_if, instructions_else)::instructions' -> 
        if evalue_condition env test then 
          eval_instructions (instructions_if@instructions') env etat
        else
          eval_instructions (instructions_else@instructions') env etat
    (* Repeat: on execute le bloc d'instructions tant que l'expression est > 0 *)
    | Repeat(expr, instructions_rpt)::instructions' -> 
        let x = evalue_expression env expr in 
        if x > 0. then
          eval_instructions ( (Repeat(Const(x-.1.), instructions_rpt))::(instructions_rpt@instructions') ) env etat
        else
          eval_instructions instructions' env etat
    (* Call: on enrichit l'environnement et on execute le bloc de la fonction *)
    | Call(nom_proc, valeurs_params)::instructions' -> 
        let (params, defs) = env in
        let (_, params_proc, instructions_proc) = get_procedure nom_proc defs in
        let new_env = (enrichir_env params params_proc valeurs_params, defs) in
        let (nouvel_etat_call, cmd_list_call) = (eval_instructions instructions_proc new_env etat) in
        let (nouvel_etat, cmd_list) = (eval_instructions instructions' env (etat_fin_bloc etat nouvel_etat_call)) in
        (nouvel_etat, cmd_list_call@cmd_list)

  (* la liste de procedures joue le role d'env initial *)
  in let (_, cmd_list) = eval_instructions instructions ([], defs) etat_initial in cmd_list;;
