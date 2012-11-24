open Logo_types;;


(* évaluation d'une expression 'expr' dans 'environnement' : A DEFINIR	*)
val evalue_expression : environnement -> expr -> float;;

(* évaluation d'une condition 'test' dans 'environnement' : A DEFINIR	*)
val evalue_condition : environnement -> test -> bool;;

(* exécution de 'instruction' dans 'environnement' depuis la position	*)
(* 'etat'. Renvoie le nouvel état atteint : A DEFINIR			*)
val execute_instruction : environnement -> instruction -> etat -> etat;;

(* exécution de 'programme' à partir de l'état initial (0.0, 0.0, 0.0)	*)
(* A DEFINIR								*)
val execute_programme : programme -> cmd list;;

