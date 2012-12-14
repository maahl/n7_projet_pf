open Logo_types;;

(* Interpretation d'une procedure : 
  * retourne la nouvelle liste de mots 
  * TODO *)
let interprete_procedure lmot = (lmot, ([],[]));;
(*
 * fonction interprete_ss_prgms
 * But : interpreter une liste de mots jusqu'au END de meme niveau que la
 * premiere instruction
 * Entree : la liste de mots
 * Precondition : la liste contient au moins un END
 * Sortie : l'arbre d'instructions correspondant a la liste de mots jusqu'au END
 * Postcondition : toutes les instructions jusqu'au END de meme niveau que la
 * premiere instruction sont renvoyees dans la liste
 *)
let rec interprete_ss_prgms lmot = 
  match lmot with
  (* END: on remonte d'un niveau dans l'arbre *)
  | END::lmot' -> (lmot', []) 
  (* BEGIN: on descend d'un niveau dans l'arbre *)
  | BEGIN::lmot' -> interprete_ss_prgms lmot'
  (* MOVE: on ajoute l'instruction Move et on interprete la suite *)
  | MOVE::EXPR(e)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot' 
                            in (new_lmot, Move(e)::ss_prgms)
  (* JUMP: on ajoute l'instruction Jump et on interprete la suite *)
  | JUMP::EXPR(e)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot' 
                           in (new_lmot, Jump(e)::ss_prgms)
  (* ROTATE: on ajoute l'instruction Rotate et on interprete la suite *)
  | ROTATE::EXPR(e)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot'
                              in (new_lmot, Rotate(e)::ss_prgms)
  (* COLOR: on ajoute l'instruction Color et on interprete la suite *)
  | COLOR::EXPR(r)::EXPR(g)::EXPR(b)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot' in
                                                 (new_lmot, Color(r, g, b)::ss_prgms)
  (* IF: on ajoute l'instruction If constituee du test et des 2 sous-blocs d'instructions *)
  | IF::TEST(t)::THEN::BEGIN::lmot' -> let (new_lmot_if, ss_prgms_if) = interprete_ss_prgms lmot'
                                       in let (new_lmot_else, ss_prgms_else) = interprete_ss_prgms new_lmot_if
                                       in let (new_lmot, ss_prgms) = interprete_ss_prgms new_lmot_else
                                       in (new_lmot, If(t, ss_prgms_if, ss_prgms_else)::ss_prgms)
  (* REPEAT: on ajoute l'instruction Repeat constituee du nombre de repetitions et du sous bloc d'instructions *)
  | REPEAT::EXPR(e)::BEGIN::lmot' -> let (new_lmot_rpt, ss_prgms_rpt) = interprete_ss_prgms lmot'
                                     in let (new_lmot, ss_prgms) = interprete_ss_prgms new_lmot_rpt
                                     in (new_lmot, Repeat(e, ss_prgms_rpt)::ss_prgms)
  (* CALL: TODO! *)
  | CALL::lmot' -> failwith "TODO!"
  | _ -> failwith "rtfm noob";;

  (*
   * fonction analyseur syntaxique
   * But : transformer la liste des mots du programme LOGO en un programme
   * interpretable par l'analyseur semantique
   * Entree : la liste des mots du prog LOGO
   * Precondition : Pas d'erreurs dans le programme LOGO (syntaxe, sens, etc)
   * Sortie : liste des procedures et liste des sous-programmes du prog
   * principal, interpretable par l'analyseur semantique
   * Postcondition : le programme en sortie correspond au programme en entree
   *)
let rec analyseur_syntaxique lmot =
  match lmot with
  (* definition du prog principal; constitue d'une liste de sous-programmes *)
  | BEGIN::lmot' -> let (_, ss_prgms) = interprete_ss_prgms lmot' 
                    in ([], ss_prgms)
  (* Definition d'une procedure : on recupere la procedure et la nouvelle liste
   * de mots *)
  | DEF::lmot' -> let (new_lmot, proc) = interprete_procedure lmot' 
                  in let (procs, ss_prgms) = analyseur_syntaxique new_lmot 
                  in (proc::procs, ss_prgms)
  | _ -> failwith "rtfm noob!";;
