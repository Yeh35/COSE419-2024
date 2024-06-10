open Smt 
(* open Utils lec5 - 12page  *)

(* constraint *)
type const = 
  | X of int * int (* Xij *)
  | True 
  | False 
  | And of const list 
  | Or of const list 
  | Imply of const * const 
  | Not of const 

let bigand f l = And (List.map f l)
let bigor f l = Or (List.map f l)

let c1 = 
  bigand (fun i -> 
    bigor (fun j -> 
      X (i, j)
    ) (range 3)
  ) (range 3)

let c2 = 
  bigand (fun j -> 
    bigor (fun i -> 
      X (i, j)
    ) (range 3)
  ) (range 3)

let c3 = 
  bigand (fun i -> 
    bigand (fun j -> 
      Imply (
        X (i, j),
        bigand (fun k ->
          if j <> k then Not(X (i, k))
          else True
        ) (range 3)
      )
    ) (range 3)
  ) (range 3)


let c4 = And [
  Imply (X (0, 0), Not (X (2, 1)));
  Imply (X (0, 1), Not (Or [X (2, 0); X(2, 2)]));
  Imply (X (0, 2), Not (X (2, 1)));
]
let c5 = Not (X (0, 0))

let c6 = And [
  Imply ( X (2, 0), Not (X (1, 1)));
  Imply ( X (2, 1), Not (X (1, 2)));
]

let encode : unit -> const
=fun () -> And[c1; c2; c3; c4; c5; c6]

let x = 
  List.map (fun i -> 
    List.map (fun j ->
      Expr.create_var (Expr.sort_of_bool()) 
        ~name:("x" ^ string_of_int i ^ string_of_int j)
    ) (range 3)
  ) (range 3)

let get_x i j = List.nth (List.nth x i) j

let rec trans : const -> Fmla.t 
= fun c -> 
  match c with
  | X (i, j) -> get_x i j
  | True -> Expr.true_()
  | False -> Expr.false_()
  | And cs -> Fmla.create_and (List.map trans cs)
  | Or cs -> Fmla.create_or (List.map trans cs)
  | Imply (c1, c2) -> Fmla.create_imply (trans c1) (trans c2)
  | Not c -> Fmla.create_not (trans c)
  
let run () = 
  let c = encode () in 
  let f = trans c in 
  let _ = prerr_endline ("\n" ^ Fmla.to_string f) in 
  let v, model_opt = Solver.check_satisfiability [f] in 
  let _ = prerr_endline (Solver.string_of_satisfiability v) in 
    match model_opt with 
    | Some model -> prerr_endline (Model.to_string model)
    | None -> ()