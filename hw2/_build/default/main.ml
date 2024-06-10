open Utils 
open Cover 
open Learn 


let sets1 : sets = (6, 7, 
[
  [1;0;0;1;0;0;1]; 
  [1;0;0;1;0;0;0];
  [0;0;0;1;1;0;1]; 
  [0;0;1;0;1;1;0]; 
  [0;1;1;0;0;1;1];
  [0;1;0;0;0;0;1];
])

let sets2 : sets = (5, 5,
[
  [1; 0; 1; 0; 1];
  [1; 1; 0; 0; 1];
  [0; 0; 0; 1; 0];
  [0; 1; 0; 1; 0];
  [0; 1; 1; 0; 0];
])

let sets3 : sets = (5, 5,
[
  [1; 0; 1; 0; 1];
  [1; 1; 1; 1; 1];
  [0; 0; 0; 1; 0];
  [0; 1; 0; 1; 0];
  [0; 1; 1; 0; 0];
])

let sets4 : sets = (5, 5,
[
  [1; 0; 0; 0; 0];
  [0; 1; 0; 0; 0];
  [0; 0; 1; 0; 0];
  [0; 0; 0; 1; 0];
  [0; 0; 0; 0; 1];
])

let sets5 : sets = (6, 5,
[
  [1; 0; 0; 0; 0];
  [0; 1; 0; 0; 0];
  [0; 0; 1; 0; 0];
  [0; 0; 0; 1; 0];
  [0; 0; 0; 0; 1];
  [1; 0; 0; 0; 0];
])

let sets6 : sets = (6, 5,
[
  [1; 0; 0; 0; 0];
  [0; 1; 0; 0; 0];
  [0; 0; 1; 0; 0];
  [0; 0; 0; 1; 0];
  [0; 0; 0; 0; 1];
  [0; 1; 1; 1; 1];
])

(* test *)
let _ = print_endline ("sets1 : " ^ string_of_list string_of_int (cover sets1))
let _ = print_endline ("sets2 : " ^ string_of_list string_of_int (cover sets2))
let _ = print_endline ("sets3 : " ^ string_of_list string_of_int (cover sets3))
let _ = print_endline ("sets4 : " ^ string_of_list string_of_int (cover sets4))
let _ = print_endline ("sets5 : " ^ string_of_list string_of_int (cover sets5))
let _ = print_endline ("sets6 : " ^ string_of_list string_of_int (cover sets6))

(* test *)
let _ = 
  List.iter ( fun spec -> 
    match synthesize spec with 
    | None -> print_endline "No solution exists"
    | Some dnf -> 
      print_endline ("solution found with m = " ^ string_of_int (List.length dnf)); 
      print_endline (string_of_dnf dnf)
  ) [spec1; spec2; spec3; spec4; spec5;]


