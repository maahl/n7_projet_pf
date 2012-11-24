open Logo_types;;
open Graphics;;


let my_int_of_float r =
 let ir = int_of_float r in
 if r -. (float_of_int ir) > 0.5 then ir+1 else ir;;

let fmoveto x y =
 moveto (my_int_of_float x) (my_int_of_float y);;

let flineto x y =
 lineto (my_int_of_float x) (my_int_of_float y);;


let rec dessine lcmd =
  let traite_cmd cmd =
          match cmd with
            Change_color(c) -> set_color c
           |Jumpto(x,y) -> fmoveto x y
           |Moveto(x,y) -> flineto x y
  in 
List.fold_left (fun tq t ->  (traite_cmd t ; tq)) () lcmd;;
