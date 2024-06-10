open Utils 
(*** Do not open anything else ***)

type var = string  

(*** Propositional Formulas ***)
type formula = 
  | False 
  | True 
  | Var of var 
  | Not of formula 
  | And of formula * formula
  | Or of formula * formula 
  | Imply of formula * formula
  | Iff of formula * formula

let rec string_of_formula f = 
  match f with 
  | True -> "true"
  | False -> "false"
  | Var x -> x 
  | Not f -> "(not " ^ string_of_formula f ^ ")"
  | And (f1, f2) -> "(" ^ string_of_formula f1 ^ " and " ^ string_of_formula f2 ^ ")"
  | Or (f1, f2) -> "(" ^ string_of_formula f1 ^ " or " ^ string_of_formula f2 ^ ")"
  | Imply (f1, f2) -> "(" ^ string_of_formula f1 ^ " -> " ^ string_of_formula f2 ^ ")"
  | Iff (f1, f2) -> "(" ^ string_of_formula f1 ^ " <-> " ^ string_of_formula f2 ^ ")"
  
(*** CNF ***)
type literal = bool * var (* false means negated *)
type clause = literal list 
type cnf = clause list 

let string_of_literal (b, x) = if b then x else "!" ^ x 
let string_of_clause c = string_of_list string_of_literal c ~first:"(" ~last:")" ~sep:"\\/"
let string_of_cnf a = string_of_list string_of_clause a ~first:"(" ~last:")" ~sep:"/\\"
  
(*** DPLL ***)
exception Not_implemented 

(* Problem 1: CNF conversion *)
let rec convert_to_nnf : formula -> formula
= fun f ->
  match f with
  | Not (True) -> False
  | Not (False) -> True
  | Not (Not f) -> convert_to_nnf f
  | Not (And (f1, f2)) -> Or (convert_to_nnf (Not f1), convert_to_nnf (Not f2))
  | Not (Or (f1, f2)) -> And (convert_to_nnf (Not f1), convert_to_nnf (Not f2))
  | Imply (f1, f2) -> Or (Not (convert_to_nnf f1), convert_to_nnf f2)
  | Iff (f1, f2) -> And (convert_to_nnf (Imply (convert_to_nnf f1, convert_to_nnf f2)), convert_to_nnf (Imply (convert_to_nnf f2, convert_to_nnf f1)))
  | True -> True
  | False -> False
  | Var x -> Var x
  | Not f -> Not (convert_to_nnf f)
  | And (f1, f2) -> And (convert_to_nnf f1, convert_to_nnf f2)
  | Or (f1, f2) -> Or (convert_to_nnf f1, convert_to_nnf f2)

let rec convert_to_cnf : formula -> formula
= fun f -> match f with
| Or (And (f1, f2), f3) -> And (convert_to_cnf (Or (f1, f3)), convert_to_cnf (Or (f2, f3)))
| Or (f1, And (f2, f3)) -> And (convert_to_cnf (Or (f1, f2)), convert_to_cnf (Or (f1, f3)))
| And (f1, f2) -> And (convert_to_cnf f1, convert_to_cnf f2)
| Or (f1, f2) -> Or (convert_to_cnf f1, convert_to_cnf f2)
| _ -> f

let rec _trans_to_cnf : formula -> cnf
= fun f -> match f with
| True -> []
| False -> [[]]
| Not (Var x) -> [[(false, x)]]
| Var x -> [[(true, x)]]
| And (f1, f2) -> _trans_to_cnf f1 @ _trans_to_cnf f2
| Or (f1, f2) -> _trans_to_cnf f1 @ _trans_to_cnf f2
| _ -> let _ = print_endline ("error: " ^ string_of_formula f) in []

(* let convert : formula -> cnf
= fun f -> let nnf = convert_to_nnf f in
    let cnf_f = convert_to_cnf nnf in
    _trans_to_cnf cnf_f *)


let rep : formula -> formula
= fun f -> match f with
| True | False | Var _ -> f
| _ -> Var ("P" ^ string_of_formula f)

let en : formula -> formula
= fun f -> match f with
| True | False | Var _ -> f
| And (f1, f2) -> let p = rep(f) in 
    And( And( Or(Not p, rep f1), Or(Not p, rep f2) ), Or ( Or( Not (rep f1), Not (rep f2) ), p ))
| Not f -> let p = rep(Not f) in
    And( Or( Not p, Not (rep f)), Or( p, rep f))
| Or (f1, f2) -> let p = rep(f) in
    And( And( Or (Or(Not p, rep f1), rep(f2)), Or (Not (rep f1), p)), Or(Not (rep f2), p))
| Imply (f1, f2) -> let p = rep(f) in
    And (And( Or ( Or(Not p, Not (rep f1)), rep f2 ), Or( rep f1, p ) ), Or (Not (rep f2), p))
| Iff (f1, f2) -> let p = rep(f) in
    And( 
      And( 
        And( Or(Or( Not p, Not (rep f1)), rep f2), Or(Or(Not p, rep f1), Not (rep f2))), 
          Or(Or(p, Not (rep f1)), Not (rep f2))),
          Or(Or(p, rep f1), rep f2)
        )


let convert : formula -> cnf
= fun f -> let f_cnf = en f in _trans_to_cnf f_cnf
  
(*  let rep : formula -> literal
= fun f -> match f with
| Not (Var x) -> (false, x)
| Var x -> (true, x)
| True | False -> raise Not_implemented
| _ -> (true, "P" ^ string_of_formula f)

let not_rep : formula -> literal
= fun f -> let l = rep f in
  match l with
  | (true, x) -> (false, x)
  | (false, x) -> (true, x)

let not_literal : literal -> literal
= fun (b, x) -> (not b, x)

let convert : formula -> cnf
= fun f -> match convert_to_nnf f with
  | True -> []
  | False -> [[]]
  | And (f1, f2) -> let (p_b, p_x) = rep(f) in 
    [
      [(not p_b, p_x); rep (f1)];
      [(not p_b, p_x); rep (f2)];
      [not_rep f1; not_rep f2; (p_b, p_x)];
    ]
  | Not f -> let (p_b, p_x) = rep(Not f) in
    [
      [(not p_b, p_x); not_rep f];
      [(p_b, p_x); rep f];
    ]
  | Or (f1, f2) -> let (p_b, p_x) = rep(f) in
    [
      [(not p_b, p_x); rep f1; rep f2];
      [not_rep f1; (p_b, p_x)];
      [not_rep f2; (p_b, p_x)];
    ]
  | Imply (f1, f2) -> let (p_b, p_x) = rep(f) in
    [
      [(not p_b, p_x); not_rep f1; rep f2];
      [rep f1; (p_b, p_x)];
      [not_rep f2; (p_b, p_x)];
    ]
  | Iff (f1, f2) -> let (p_b, p_x) = rep(f) in
    [
      [(not p_b, p_x); not_rep f1; rep f2];
      [(not p_b, p_x); rep f1; not_rep f2];
      [(p_b, p_x); not_rep f1; not_rep f2];
      [(p_b, p_x); rep f1; rep f2];
    ]
  | _ -> raise (Failure "Not a NNF") *)

  
(* Problem 2: substitution a[v/x] (replacing x by v in a) *)
let subst_literal : literal -> bool -> var -> cnf
= fun (b, x) v y -> match x == y, b == v with
| true, true -> []
| true, false -> [[]]
| false, _ -> [[(b, x)]]

let rec subst_clause : clause -> bool -> var -> cnf
= fun c v x -> match c with
| [] -> []
| hd::[] -> subst_literal hd v x
| hd::tl -> match subst_literal hd v x with 
  | [] -> []
  | [[]] -> subst_clause tl v x
  | c_li -> match subst_clause tl v x with
    | [] -> []
    | [[]] -> c_li
    | c2_li -> c_li @ c2_li

let rec subst : cnf -> bool -> var -> cnf 
=fun a v x -> match a with
| [] -> []
| hd::tl -> subst_clause hd v x @ subst tl v x

(* Problem 3: boolean constraint propagation *)

let rec bcp : cnf -> cnf
= fun c -> match List.find_opt (fun x -> List.length x = 1) c with
| None -> c
| Some unit -> 
  let (unit_b, unit_x) = List.hd unit in
  let removed_unit_c = List.filter (fun x -> not (List.mem (unit_b, unit_x) x)) c in
  match List.find_opt (fun e -> 
      List.exists (fun (l_b, l_x) -> l_b = (not unit_b) && l_x = unit_x) e
    ) c with
  | None -> bcp (removed_unit_c)
  | Some c_contain_not_unit -> 
    let c_contain_not_unit_other = List.filter (fun (b_x, x) -> not (b_x = (not unit_b) && x = unit_x)) c_contain_not_unit in
    let removed_c = List.filter (fun x -> not (List.mem (not unit_b, unit_x) x)) removed_unit_c in
    bcp (c_contain_not_unit_other::removed_c)

(* Problem 4: pure literal elimination *)
let remove_duplicates (xs: var list): var list =
  let seen = List.fold_left (fun seen x -> if List.mem x seen then seen else x::seen) [] xs in
  seen

let get_all_literal : cnf -> literal list
= fun c -> List.flatten c

let get_pure_literals : cnf -> literal list
= fun c ->
  let all_literals = get_all_literal c in
  List.filter (fun x -> 
    let (b, x) = x in
    let not_x = (not b, x) in
    not (List.exists (fun y -> y = not_x) all_literals)
  ) all_literals

let rec ple : cnf -> cnf 
= fun c -> 
  match get_pure_literals c with
  | [] -> c
  | pure_literals -> 
    let pure_vars = List.map (fun (_, x) -> x) pure_literals in
    let removed_c = List.filter (fun x -> not (List.mem (List.hd x) pure_literals)) c in
    let removed_c = List.map (fun x -> List.filter (fun (_, x) -> not (List.mem x pure_vars)) x) removed_c in
    ple removed_c

let choose : cnf -> var 
=fun a -> snd (List.hd (List.hd a))

let rec dpll : cnf -> bool 
=fun a ->  
  let a = ple (bcp a) in 
    if a = [] then true  (* /\ [] = true *)
    else if List.mem [] a then false (* \/ [] = false *)
    else 
      let x = choose a in 
        dpll (subst a false x) || dpll (subst a true x) 

let solve : formula -> bool 
=fun f -> dpll (convert f)