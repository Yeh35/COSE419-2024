open Sat 

let _ = print_endline "run main"

let f1 = 
  Or (
    And (Var "Q1", Var "Q2"), 
    And (Var "R1", Var "R2")
  )

let f2 = False 

let f3 = Not True 

let f4 = 
  And (
    Imply (Var "P", Var "Q"), 
    And (Var "P", Not (Var "Q"))
  )

let f5 = Or (And (Var "P", Var "Q"), Var "R")

let f6 = True 

let f7 = (Imply (Var "P", Var "Q"))

let f8 = Or (
  And (Var "Q1", Var "Q2"),
  And (Var "R1", Var "R2")
)

let f9 = And (
  Imply (Var "P", Var "Q"),
  And (Var "P", Not (Var "Q"))
)

let f10 = Or (And (Var "P", Var "Q"), Var "R")

let test_cnf () = 
  List.iter (fun f ->
    print_endline (Sat.string_of_formula f ^ " => " ^ Sat.string_of_cnf (Sat.convert f) )  
) [f1; f2; f3; f4; f5; f6; f7]
  

let subst1 = Sat.subst [[(true, "P")]] true "P"
let subst2 = Sat.subst [[(false, "P")]] true "P"
let subst3 = Sat.subst [[(true, "P")]] false "P"
let subst4 = Sat.subst [[(false, "P"); (true, "Q")]] true "P"
let subst5 = Sat.subst [[(false, "P"); (true, "Q")]] false "P"
let subst6 = Sat.subst [[(false, "P"); (true, "Q")]; [(true, "R"); (false, "Q"); (true, "S")]] false "Q"
let subst7 = Sat.subst [[(false, "P"); (true, "Q")]; [(true, "R"); (false, "Q"); (true, "S")]] true "P"

let test_subst () = 
  List.iter (fun subst ->
    print_endline (Sat.string_of_cnf subst )  
) [subst1; subst2; subst3; subst4; subst5; subst6; subst7]

let bcp1 = [[(true, "P")]]
let bcp2 = [[(false, "P")]]
let bcp3 = [
  [(true, "P")];
  [(false, "P"); (true, "Q")];
  [(true, "R"); (false, "Q"); (true, "S")]
  ]
let bcp4 =  [[(true, "P((P and Q) or R)")];
  [(false, "P((P and Q) or R)"); (true, "P(P and Q)"); (true, "R")];
  [(false, "P(P and Q)"); (true, "P((P and Q) or R)")];
  [(false, "R"); (true, "P((P and Q) or R)")];
  [(false, "P(P and Q)"); (true, "P")]; [(false, "P(P and Q)"); (true, "Q")]
]
let bcp5 =  [[(true, "P((P -> Q) and (P and (not Q)))")];
  [(false, "P((P -> Q) and (P and (not Q)))"); (true, "P(P -> Q)")];
  [(false, "P((P -> Q) and (P and (not Q)))"); (true, "P(P and (not Q))")];
  [(false, "P(P -> Q)"); (false, "P(P and (not Q))");
  (true, "P((P -> Q) and (P and (not Q)))")];
  [(false, "P(P -> Q)"); (false, "P"); (true, "Q")];
  [(true, "P"); (true, "P(P -> Q)")]; [(false, "Q"); (true, "P(P -> Q)")];
  [(false, "P(P and (not Q))"); (true, "P")];
  [(false, "P(P and (not Q))"); (true, "P(not Q)")];
  [(false, "P"); (false, "P(not Q)"); (true, "P(P and (not Q))")];
  [(false, "P(not Q)"); (false, "Q")]; [(true, "P(not Q)"); (true, "Q")]
]
let bcp6 = [
  [(false, "P")]; 
  [(true, "P"); (false, "Q")]
]

let test_bcp () =
  List.iter (fun cnf ->
    print_endline (Sat.string_of_cnf cnf ^ " => " ^ Sat.string_of_cnf (Sat.bcp cnf))  
) [bcp1; bcp2; bcp3; bcp4; bcp5; bcp6]
  (* ) [bcp3] *)
  
let test_get_all_literal () =
  List.iter (fun c ->
    let cnf = (Sat.bcp c) in
    let _ = print_endline (Sat.string_of_cnf cnf ^ " => ") in
    let pures = List.map (fun l -> Sat.string_of_literal l) (Sat.get_pure_literals cnf) in
    print_endline (List.fold_left (fun l v -> l ^ v ^ ", ") "" pures)
) [bcp1; bcp2; bcp3; bcp4; bcp5; bcp6]

let test_ple () =
  List.iter (fun c ->
    let cnf = (Sat.bcp c) in
    let _ = print_endline (Sat.string_of_cnf cnf ^ " => ") in
    print_endline (Sat.string_of_cnf (Sat.ple cnf))
) [bcp1; bcp2; bcp3; bcp4; bcp5; bcp6]

let test () = 
  List.iter (fun f ->
    print_endline (Sat.string_of_formula f ^ " : " ^ 
      if Sat.solve f then "SAT" else "UNSAT")  
  ) [f1; f2; f3; f4; f5; f6 ; f7; f8; f9; f10]

(* let _ = test_cnf(); *)
let _ = test();