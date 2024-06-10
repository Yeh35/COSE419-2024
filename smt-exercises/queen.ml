open Smt 
open Utils 

type solution = int list 

let display_board : solution -> unit 
=fun cols -> 
  List.iter (fun row -> 
    List.iter (fun col -> 
      if col = List.nth cols row then print_string "1"
      else print_string "0" 
    ) (range 8); 
    print_endline ""
  ) (range 8) 

type exp = 
  | Q of int 
  | Int of int 
  | Sub of exp * exp 

type const = 
  | And of const list 
  | Or of const list 
  | Imply of const * const 
  | Le of exp * exp 
  | Neq of exp * exp 
  | Eq of exp * exp

let forall f l = And (List.map f l)
let exists f l = Or (List.map f l)


let encode : unit -> const 
=fun _ -> 
  let c1 =
    forall (fun i -> 
      And [
        Le (Int 0, (Q i));
        Le ((Q i), Int 7)
      ] 
    ) (range 8) in
  let c2 = 
    forall (fun i -> 
      forall (fun j ->
        Imply (
          Neq (Int i, Int j),
          Neq (Q i, Q j)
        )
      ) (range 8)
    ) (range 8) in
  let c3 = 
    forall (fun i -> 
      forall (fun j -> 
        Imply (
          Neq (Int i, Int j),
          And [
            Neq (Sub (Int i, Int j), Sub (Q i, Q j));
            Neq (Sub (Int i, Int j), Sub (Q j, Q i))
          ]
        )
      ) (range i)
    ) (range 8) in
    And [c1; c2; c3]
    
let q = 
  List.map (fun i -> 
    Expr.create_var (Expr.sort_of_int()) ~name: ("Q" ^ string_of_int i)
  ) (range 8)


let rec trans_exp : exp -> Expr.t
= fun e -> 
  match e with
  | Q i -> List.nth q i
  | Int n -> Expr.of_int n
  | Sub (e1, e2) ->
    Expr.create_sub (trans_exp e1) (trans_exp e2)


let rec trans : const -> Fmla.t 
=fun c -> 
  match c with
  | And cs -> Fmla.create_and (List.map trans cs)
  | Or cs -> Fmla.create_or (List.map trans cs)
  | Imply (c1, c2) -> Fmla.create_imply (trans c1) (trans c2)
  | Le (e1, e2) -> Expr.create_le (trans_exp e1) (trans_exp e2)
  | Neq (e1, e2) -> Expr.create_neq (trans_exp e1) (trans_exp e2)
  | Eq (e1, e2) -> Expr.create_eq (trans_exp e1) (trans_exp e2)

let model2solution : Model.t -> solution 
=fun model -> 
  List.map (fun i ->
    match Model.eval (List.nth q i) ~model:model with
    | Some expr -> int_of_string (Expr.to_string expr)
    | None -> raise (Failure "model2solution")
  ) (range 8)


let diff : solution list -> const
= fun sols -> 
  forall (fun s -> 
    exists (fun i -> 
      Neq (Int (List.nth s i), Q i)
    ) (range 8)
  ) sols

(* 모든 경우의 수를 찾는 방법 *)
let find_all : unit -> solution list 
= fun () -> 
  let rec loop sols = 
    let c1 = encode () in 
    let c2 = diff sols in
    let f = trans (And [c1; c2]) in
      match Solver.check_satisfiability [f] with
      | _, Some model -> display_board (model2solution model);
        print_endline "";
        loop ((model2solution model)::sols)
      | _, None -> sols in
    loop []

let run () = 
  let c = encode () in 
  let f = trans c in 
  let _ = prerr_endline ("\n" ^ Fmla.to_string f) in 
  let v, model_opt = Solver.check_satisfiability [f] in 
  let _ = prerr_endline (Solver.string_of_satisfiability v) in 
    match model_opt with 
    | Some model -> display_board (model2solution model)
    | None -> ()
