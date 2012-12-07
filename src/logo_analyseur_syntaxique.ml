open Logo_types;;

(* Interprétation d'une procédure : 
  * retourne la procedure la nouvelle liste de mots 
  * TODO *)
let interprete_procedure lmot = (lmot, ([],[]));;
(* Interprétation d'une liste de sous_programmes :
  * retourne la liste de sous-programmes et la nouvelle liste de mots*)
(*
 * fonction interprete_ss_prgms
 * But : interpréter une liste de mots jusqu'au END de même niveau que la
 * première instruction
 * Entree : la liste de mots
 * Precondition : la liste contient au moins un END
 * Sortie : la liste de sous-programmes correspondant à la liste de mots jusqu'au END
 * Postcondition : tous les sous programmes jusqu'au END de meme niveau que la
 * premiere instruction sont renvoyes dans la liste
 *)
let rec interprete_ss_prgms lmot = 
  match lmot with
  (* | [] -> [] *)
  | END::lmot' -> (lmot', []) 
  | BEGIN::lmot' -> interprete_ss_prgms lmot'
  | MOVE::EXPR(e)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot' 
                            in (new_lmot, Move(e)::ss_prgms)
  | JUMP::EXPR(e)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot' 
                           in (new_lmot, Jump(e)::ss_prgms)
  | ROTATE::EXPR(e)::lmot' -> let (new_lmot, ss_prgms) = interprete_ss_prgms lmot'
                              in (new_lmot, Rotate(e)::ss_prgms)
  | COLOR::EXPR(r)::EXPR(g)::EXPR(b)::lmot' -> failwith "TODO!"
  | IF::TEST(t)::THEN::BEGIN::lmot' -> let (new_lmot_if, ss_prgms_if) = interprete_ss_prgms lmot'
                                       in let (new_lmot_else, ss_prgms_else) = interprete_ss_prgms new_lmot_if
                                       in let (new_lmot, ss_prgms) = interprete_ss_prgms new_lmot_else
                                       in (new_lmot, If(t, ss_prgms_if, ss_prgms_else)::ss_prgms)
  | REPEAT::EXPR(e)::BEGIN::lmot' -> let (new_lmot_rpt, ss_prgms_rpt) = interprete_ss_prgms lmot'
                                     in let (new_lmot, ss_prgms) = interprete_ss_prgms new_lmot_rpt
                                     in (new_lmot, Repeat(e, ss_prgms_rpt)::ss_prgms)
  | CALL::lmot' -> failwith "TODO!"
  | _ -> failwith "rtfm noob";;

  (*
   * fonction analyseur syntaxique
   * But : transformer la liste des mots du programme LOGO en un programme
   * interpretable par l'analyseur semantique
   * Entree : la liste des mots du prog LOGO
   * Precondition : Pas d'erreurs dans le programme LOGO (syntaxe, sens, etc)
   * Sortie : liste des procédures et liste des sous-programmes du prog
   * principal, interpretable par l'analyseur semantique
   * Postcondition : le programme en sortie correspond au programme en entree
   *)
let rec analyseur_syntaxique lmot =
  match lmot with
  | [] -> ([], [])
  (* Définition d'une procédure : on récupère la procedure et la nouvelle liste
   * de mots *)
  | DEF::lmot' -> let (new_lmot, proc) = interprete_procedure lmot' 
                  in let (procs, ss_prgms) = analyseur_syntaxique new_lmot 
                  in (proc::procs, ss_prgms)
  (* definition du prog principal; constitué d'une liste de sous-programmes *)
  | BEGIN::lmot' -> let (_, ss_prgms) = interprete_ss_prgms lmot' 
                    in ([], ss_prgms)
  | _ -> failwith "rtfm noob!";;
