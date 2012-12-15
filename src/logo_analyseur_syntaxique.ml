open Logo_types;;

(*
 * fonction get_params
 * But : recuperer les valeurs des parametres passes lors d'un call. Logiquement, devrait etre une fonction auxiliaire de interprete_instructions, mais je la mets la pour l'avoir une seule fois dans l'env
 * Entree : la liste de mots
 * Precondition : prog correct
 * Sortie : la nouvelle liste de mots et la liste des params
 * Postcondition : -
 *)
let rec get_params lmot =
  match lmot with
  (* tant qu'on trouve des parametres, on les recupere *)
  | EXPR(valeur_param)::lmot' -> let (new_lmot, valeurs_params) = get_params lmot'
                                  in (new_lmot, valeur_param::valeurs_params)
  (* fin des parametres *)
  | _ -> (lmot, []);;

(*
 * fonction interprete_instructions
 * But : interpreter une liste de mots jusqu'au END de meme niveau que la premiere instruction
 * Entree : la liste de mots
 * Precondition : la liste contient au moins un END
 * Sortie : la nouvelle liste de mots a interpreter et l'arbre d'instructions correspondant a la liste de mots jusqu'au END
 * Postcondition : -
 *)
let rec interprete_instructions lmot = 
  match lmot with
  (* END: on remonte d'un niveau dans l'arbre *)
  | END::lmot' -> (lmot', []) 
  (* BEGIN: on interprete la suite *)
  | BEGIN::lmot' -> interprete_instructions lmot'
  (* MOVE: on ajoute l'instruction Move et on interprete la suite *)
  | MOVE::EXPR(e)::lmot' -> let (new_lmot, instructions) = interprete_instructions lmot' 
                            in (new_lmot, Move(e)::instructions)
  (* JUMP: on ajoute l'instruction Jump et on interprete la suite *)
  | JUMP::EXPR(e)::lmot' -> let (new_lmot, instructions) = interprete_instructions lmot' 
                           in (new_lmot, Jump(e)::instructions)
  (* ROTATE: on ajoute l'instruction Rotate et on interprete la suite *)
  | ROTATE::EXPR(e)::lmot' -> let (new_lmot, instructions) = interprete_instructions lmot'
                              in (new_lmot, Rotate(e)::instructions)
  (* COLOR: on ajoute l'instruction Color et on interprete la suite *)
  | COLOR::EXPR(r)::EXPR(g)::EXPR(b)::lmot' -> let (new_lmot, instructions) = interprete_instructions lmot' in
                                                 (new_lmot, Color(r, g, b)::instructions)
  (* IF: on ajoute l'instruction If constituee du test et des 2 sous-blocs d'instructions *)
  | IF::TEST(t)::THEN::BEGIN::lmot' -> let (new_lmot_if, instructions_if) = interprete_instructions lmot'
                                       in let (new_lmot_else, instructions_else) = interprete_instructions new_lmot_if
                                       in let (new_lmot, instructions) = interprete_instructions new_lmot_else
                                       in (new_lmot, If(t, instructions_if, instructions_else)::instructions)
  (* ELSE: on interprete simplement le bloc, il sera traite dans le cas if *)
  | ELSE::BEGIN::lmot' -> interprete_instructions lmot'
  (* REPEAT: on ajoute l'instruction Repeat constituee du nombre de repetitions et du sous bloc d'instructions *)
  | REPEAT::EXPR(e)::BEGIN::lmot' -> let (new_lmot_rpt, instructions_rpt) = interprete_instructions lmot'
                                     in let (new_lmot, instructions) = interprete_instructions new_lmot_rpt
                                     in (new_lmot, Repeat(e, instructions_rpt)::instructions)
  (* CALL: on recupere les valeurs des parametres passes a la fonction et on ajoute l'instruction correspondante *)
  | CALL::IDENT(nom_proc)::lmot' -> let (new_lmot_def, valeurs_params) = get_params lmot'
                                    in let (new_lmot, instructions) = interprete_instructions new_lmot_def
                                    in (new_lmot, Call(nom_proc, valeurs_params)::instructions)
  | _ -> failwith "rtfm noob (interprete_instructions)";;

(*
 * fonction interprete_procedure
 * But : interpreter une liste de mots correspondant a une procedure
 * Entree : la liste de mots commencant par la definition de la procedure
 * Precondition : programme LOGO correct
 * Sortie : la nouvelle liste de mots a interpreter, la liste de parametres et l'arbre d'instructions de la procedure
 * Postcondition : - 
 *)
let rec interprete_procedure lmot = 
  match lmot with
  (* on remonte d'un niveau dans l'arbre d'instructions *)
  | END::lmot' -> (lmot', [], [])
  (* on recupere la liste des parametres *)
  | IDENT(param)::lmot' -> let (new_lmot, params, instructions) = interprete_procedure lmot'
                           in (new_lmot, param::params, instructions)
  (* on recupere l'arbre d'instructions de la fonction *)
  | BEGIN::lmot' -> let (new_lmot, instructions) = interprete_instructions lmot'
                    in (new_lmot, [], instructions)
  | _ -> failwith "rtfm noob (interprete_procedure)";;

  (*
   * fonction analyseur syntaxique
   * But : transformer la liste des mots du programme LOGO en un programme interpretable par l'analyseur semantique
   * Entree : la liste des mots du prog LOGO
   * Precondition : Pas d'erreurs dans le programme LOGO (syntaxe, sens, etc)
   * Sortie : liste des procedures et liste des sous-programmes du prog principal, interpretable par l'analyseur semantique
   * Postcondition : le programme en sortie correspond au programme en entree
   *)
let rec analyseur_syntaxique lmot =
  match lmot with
  (* definition du prog principal; constitue d'une liste de sous-programmes *)
  | BEGIN::lmot' -> let (_, instructions) = interprete_instructions lmot' 
                    in ([], instructions)
  (* Definition d'une procedure : on recupere la procedure et la nouvelle liste de mots *)
  | DEF::IDENT(nom_proc)::lmot' -> let (new_lmot, params, instructions_proc) = interprete_procedure lmot' 
                  in let (procs, instructions) = analyseur_syntaxique new_lmot 
                  in ((nom_proc, params, instructions_proc)::procs, instructions)
  | _ -> failwith "rtfm noob! (analyseur_syntaxique)";;
