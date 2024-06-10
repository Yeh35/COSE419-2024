open Smt 
open Utils 

type specification = int * int * example list * example list 
and example = int list 

let spec1 = (10, 2, 
[
  [0; 1]; 
  [1; 0]
], 
[
  [1; 1]; 
  [0; 0]
])

let spec2 = (10, 2, 
[
  [1; 1]
], 
[
  [0; 1]; 
  [1; 0]; 
  [0; 0]
])

let spec3 = (10, 3, 
[
  [0;0;0]; 
  [0;1;0];
  [1;0;0];
  [1;1;0];
], 
[
  [0;0;1]; 
  [0;1;1];
  [1;0;1];
  [1;1;1];
])

let spec4 = (10, 4, 
[
  [0;0;0;1]; 
  [0;0;1;1];
  [0;1;0;1];
  [1;0;0;1];
  [1;0;1;1];
  [1;1;0;1];
  [1;1;1;0]; 
], 
[
  [0;0;0;0];
  [0;0;1;0];
  [0;1;0;0];
  [0;1;1;0];
  [0;1;1;1];
  [1;0;0;0];
  [1;0;1;0];
  [1;1;0;0];
])

let spec5 = (20, 20, 
[
  [1;1;0;0;1;0;0;1;0;0;0;0;1;1;1;1;1;1;0;1];
  [1;0;1;0;1;0;1;0;0;0;1;0;0;0;1;0;0;0;0;1];
  [0;1;1;0;1;0;0;0;1;1;0;0;0;0;1;0;0;0;1;1];
  [0;1;0;0;1;1;0;0;0;1;0;0;1;1;0;0;0;1;1;0];
  [0;1;1;0;0;0;1;0;1;0;0;0;1;0;1;1;1;0;0;0];
  [0;0;0;0;1;1;0;1;1;1;0;0;0;0;0;1;1;1;0;0];
  [1;1;0;1;0;0;0;1;0;0;1;0;1;0;0;1;0;0;0;0];
  [0;0;1;0;0;1;0;0;1;1;1;0;0;0;0;0;1;0;0;0];
  [1;0;0;0;1;0;1;0;0;1;1;0;0;1;1;1;1;1;0;0];
  [1;1;0;0;0;1;1;1;0;1;0;0;0;0;0;0;0;0;1;0];
  [0;0;0;0;1;0;1;1;1;0;1;1;1;1;1;0;1;0;1;0];
  [0;1;1;0;0;0;1;1;1;0;1;1;0;0;0;1;0;0;1;1];
  [1;0;0;1;1;0;1;1;0;0;1;0;0;0;1;0;0;1;0;1];
  [0;0;0;1;0;1;0;0;1;0;1;0;0;0;0;0;1;0;0;0];
  [0;1;1;1;1;0;0;1;1;0;0;0;1;1;1;0;0;0;1;1];
  [0;1;0;0;0;0;0;0;0;1;0;0;1;1;0;1;1;1;0;1];
], 
[
  [1;0;1;0;1;1;0;1;1;1;1;1;1;0;0;0;0;1;0;1];
  [0;1;0;0;0;1;0;1;1;0;0;0;1;0;1;0;0;0;1;0];
  [1;0;1;1;1;0;1;1;0;1;0;0;1;0;1;0;1;0;0;1];
  [1;0;1;0;1;0;1;0;1;1;1;1;1;1;0;1;1;1;0;0];
  [0;1;0;1;0;1;1;0;0;0;1;0;0;0;0;0;0;0;1;0];
  [0;1;1;1;0;0;1;1;1;1;0;1;0;0;1;1;1;1;0;0];
  [1;1;1;1;0;0;0;1;1;1;0;1;1;0;0;0;1;0;1;1];
  [1;0;0;1;1;1;0;0;0;1;0;1;1;0;0;0;0;0;1;1];
  [1;1;0;0;1;1;1;0;0;0;1;0;1;1;0;1;0;0;1;1];
  [0;1;1;0;1;0;0;1;0;1;0;1;1;0;1;0;1;0;0;1];
  [1;1;1;0;0;0;0;1;0;0;1;1;0;1;1;0;0;1;0;0];
  [0;0;0;1;0;0;0;1;0;1;0;0;0;1;1;0;0;1;0;0];
  [0;0;1;1;0;0;1;1;1;1;1;1;1;0;1;1;1;1;0;0];
  [1;1;0;0;1;0;0;1;0;0;1;1;1;0;0;1;1;1;0;1];
  [1;1;0;0;1;1;1;0;0;0;1;0;0;1;0;0;1;0;0;1];
  [1;0;1;1;0;0;1;1;1;1;1;0;1;1;1;1;1;0;0;1];
])

type dnf = conj list 
and conj = lit list 
and lit = X of int | NotX of int 

type formula = 
  | P of int * int
  | Q of int * int
  | Z of int * int
  | And of formula list 
  | Or of formula list 
  | Not of formula 


let string_of_lit lit = 
  match lit with 
  | X i -> "x" ^ string_of_int i 
  | NotX i -> "!x" ^ string_of_int i 
let string_of_conj conj = string_of_list string_of_lit conj ~sep:" /\\ " ~first:"(" ~last:")"
let string_of_dnf dnf = string_of_list string_of_conj dnf ~sep:" \\/ " ~first:"" ~last:""

let forall f l = And (List.map f l)
let exists f l = Or (List.map f l)

let get_value: (int list list) * int * int -> int
  = fun (t, k, j) -> List.nth (List.nth t k) j

let positive_encode :  int * int * example list -> formula
= fun (t_n, e_lenth, e_list) -> 
  let c1 = forall ( fun k -> 
    exists ( fun i -> 
      Z (i, k)
    ) (range t_n)
  ) (range (List.length e_list)) in
  let c2 = forall (fun k -> 
    forall( fun i -> 
      forall ( fun j -> 
        Or [
          Not (Z (i, k));
          if get_value (e_list, k, j) == 1 then Not (Q (i, j)) else Not (P (i, j));
        ]
      ) (range e_lenth)
    ) (range t_n)
  ) (range (List.length e_list)) in
  And [c1; c2]

let negative_encode :  int * int * example list -> formula
= fun (t_n, e_lenth, e_list) -> 
  let c1 = forall ( fun k -> 
    forall (fun i -> 
      exists ( fun j -> 
        if get_value (e_list, k, j) == 1 then Q (i, j) else P (i, j);
      ) (range e_lenth)
    ) (range t_n)
  ) (range (List.length e_list)) in
  And [c1]

let rec trans: formula -> Fmla.t
  = fun f ->
    match f with
    | P (i, j) -> Expr.create_var (Expr.sort_of_bool()) ~name: ("P" ^ string_of_int i ^ string_of_int j)
    | Q (i, j) -> Expr.create_var (Expr.sort_of_bool()) ~name: ("Q" ^ string_of_int i ^ string_of_int j)
    | Z (i, k) -> Expr.create_var (Expr.sort_of_bool()) ~name: ("Z" ^ string_of_int i ^ string_of_int k)
    | And fs -> Fmla.create_and (List.map trans fs)
    | Or fs -> Fmla.create_or (List.map trans fs)
    | Not f -> Fmla.create_not (trans f)


let model2solution : int -> int -> Model.t -> dnf
= fun t_n e_lenth model -> 
  List.fold_left (fun d i -> 
    (
      List.fold_left (fun c j -> 
        match Model.eval (trans (P (i, j))) ~model:model with
        | Some expr -> (
          match bool_of_string (Expr.to_string expr) with
          | true -> (X (j + 1)) :: c (* +1을 하는 이유는 출력의 리스트 인덱스는 1부터 시작하기 때문 *)
          | false -> 
            match Model.eval (trans (Q (i, j))) ~model:model with
            | Some expr -> (
              match bool_of_string (Expr.to_string expr) with
                | true -> (NotX (j + 1)) :: c (* +1을 하는 이유는 출력의 리스트 인덱스는 1부터 시작하기 때문 *)
                | false -> c
            )
            | None -> raise (Failure "model2solution")
        )
        | None -> raise (Failure "model2solution")
      ) [] (range e_lenth) 
    ) :: d
  ) [] (range t_n)

let rec _synthesize_for : int -> specification -> dnf option
= fun t_n_max  (t_n, e_lenth, positive_list, negative_list) -> 
  if t_n > t_n_max 
    then None
  else
    let f_p = positive_encode(t_n, e_lenth, positive_list) in
    let f_n = negative_encode(t_n, e_lenth, negative_list) in
    let f = And [f_p; f_n] in
    match Solver.check_satisfiability [trans f] with
    | _, Some model -> Some (model2solution t_n e_lenth model)
    | _, None -> _synthesize_for t_n_max (t_n + 1, e_lenth, positive_list, negative_list)

let synthesize : specification -> dnf option
=fun (t_n_max, e_lenth, positive_list, negative_list) -> 
  _synthesize_for t_n_max (1, e_lenth, positive_list, negative_list)
