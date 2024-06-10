open Smt 
open Utils

type sets = int * int * (int list list)
type solution = int list 

type formula = 
  | X of int 
  | T of int * int 
  | Bool of bool 
  | And of formula list 
  | Or of formula list 
  | Not of formula 
  | Imply of formula * formula 
  | Iff of formula * formula 
  | Neq of int * int 


let forall f l = And (List.map f l)
let exists f l = Or (List.map f l)

let get_value: (int list list) * int * int -> int
  = fun (t, i, j) -> List.nth (List.nth t i) j

let get_T = fun (t, i, j) -> if (List.nth (List.nth t i) j) == 1 then T (i, j) else Not (T (i, j))

let encode : sets -> formula 
=fun (n, m, t) -> 
  let c0 = forall ( fun i -> 
    forall ( fun j -> 
      if get_value(t, i, j) == 1 then T (i, j) else Not (T (i, j))
    ) (range m)
  ) (range n) in
  let c1 = 
    forall ( fun j ->
      exists ( fun i ->
        And [ X i; Bool (get_value(t, i, j) == 1)] (* T(i, j);  *)
      ) (range n)
    ) (range m) in 
  let c2 = 
    forall ( fun i_1 -> 
      forall ( fun i_2 -> 
        Imply ( 
          And [Neq (i_1, i_2); X i_1; X i_2;], 
          forall ( fun j -> 
            Or [
              Bool (get_value (t, i_1, j) != 1);
              Bool (get_value (t, i_2, j) != 1);
            ]
          ) (range m)
        )
      ) (range n)
    ) (range n) in
  And [c0; c1; c2]
  
let rec trans: formula -> Fmla.t
= fun f ->
  match f with
  (* | S i -> Fmla.create_exists (Expr.create_var (Expr.sort_of_int()) ~name: "S") (Expr.of_int i) *)
  | X i -> Expr.create_var (Expr.sort_of_bool()) ~name: ("X" ^ string_of_int i)
  | T (i, j) -> Expr.create_var (Expr.sort_of_bool()) ~name: ("T" ^ string_of_int i ^ string_of_int j)
  | Bool b -> Expr.of_bool b
  | And fs -> Fmla.create_and (List.map trans fs)
  | Or fs -> Fmla.create_or (List.map trans fs)
  | Not f -> Fmla.create_not (trans f)
  | Imply (f1, f2) -> Fmla.create_imply (trans f1) (trans f2)
  | Iff (f1, f2) -> Fmla.create_iff (trans f1) (trans f2)
  | Neq (i, j) -> Expr.create_neq (Expr.of_int i) (Expr.of_int j)

let model2solution : int -> Model.t -> solution
= fun n model -> 
    List.fold_left (fun l v ->
      match Model.eval (trans (X v)) ~model:model with
      | Some expr -> (
          (* print_endline ("X" ^ string_of_int v ^ ": " ^ Expr.to_string expr); *)
          match bool_of_string (Expr.to_string expr) with
            | true -> l @ [v + 1] (* +1을 하는 이유는 출력의 리스트 인덱스는 1부터 시작하기 때문 *)
            | false -> l
        )
      | None -> raise (Failure "model2solution")
    ) [] (range n)

let solve : int -> formula -> solution
= fun n f -> 
  match Solver.check_satisfiability [trans f] with
  | _, Some model -> 
    (* print_endline (Model.to_string model);  *)
    model2solution n model
  | _, None -> []

let cover : sets -> int list  
  =fun (n, m, t) -> solve n (encode (n, m, t))
