open Logo_types;;


(* �valuation d'une expression 'expr' dans 'environnement' *)
val evalue_expression : environnement -> expr -> float;;

(* �valuation d'une condition 'test' dans 'environnement' *)
val evalue_condition : environnement -> test -> bool;;

(* ex�cution de 'instruction' dans 'environnement' depuis la position	*)
(* 'etat'. Renvoie le nouvel �tat atteint : TODO			*)
val execute_instruction : environnement -> instruction -> etat -> etat;;

(* ex�cution de 'programme' � partir de l'�tat initial (0.0, 0.0, 0.0)	*)
(* TODO 								*)
val execute_programme : programme -> cmd list
