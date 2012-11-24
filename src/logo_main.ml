open Graphics;;
open Logo_dessine;;
open Logo_analyseur_lexical;;
open Logo_analyseur_syntaxique;;
open Logo_analyseur_semantique;;

let logo fich =
  open_graph " 600x600";
  dessine (execute_programme (analyseur_syntaxique (analyseur_lexical(fich)))); 
  print_endline "Tapez <retour> pour terminer !!";
  let _ = read_line () in
  close_graph ()
;;

if not !Sys.interactive
then 
      logo
      (if (Array.length Sys.argv) != 2
       then begin
             prerr_string "Argument incorrect !\nUsage : logo <fichier>\n";
             flush stderr;
             exit 1
            end
       else Sys.argv.(1)
      );;



