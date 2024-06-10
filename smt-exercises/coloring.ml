opne Smt
opne Utils

type node = int 
type edge = node * node 
type graph = node list * edge list 
type color = int 

let instance1 : graph * color list = (
  ([1; 2; 3], [(1, 2); (2, 3)]), 
  [1; 2]
)


type const = 
  | True
  | X of int * int
  | And of const list
  | Or of const list
  | Not of const


let bigand f l = And(List.map f l)
let bigor f l = Or(List.map f l)



let encode : graph * color list -> const 
= fun (nodes, edges) colors -> 
  let c1 = 
    bigand (fun n -> 
      bigor (fun c -> 
        X(n, c)
      ) colors
    ) nodes in
  let c2 = 
    bigand (fun (i, j) ->
      bigand (fun c ->
        Not (And [X(i, c); X(j, c)])
      ) colors
    ) edges in
  let c3 = 
    bigand (fun n -> 
      bigand (fun c1 -> 
        bigand (fun c2 -> 
            if c1 <> c2 then Not (And [X(n, c1); X(n, c2)])
            else True
          ) colors
      ) colors
    ) nodes in 
    And [c1; c2; c3]

let create_x nodes colors = 
  List.map (fun n -> 
    List.map (fun c -> 
      Expr.create_var (Expr.sort_of_bool())
        ~name: ("x" ^ string_of_int n ^ string_of_int c)
    ) colors
  ) nodes

let get_x x n c = List.nth (List.nth x (n - 1)) (c - 1)

let trans x : const -> Fmla.t
  = fun c ->
    match c with
    | True -> Expr.true_()
    | And cs -> Fmla.create_and (List.map (trans x) cs)
    | Or cs -> Fmla.create_or (List.map (trans x) cs)
    | Not c -> Fmla.create_not (trans x c)
    | X (n, c) -> get_x x n c

let model2sol x nodes colors : Model.t -> (node * color) list 
= fun model -> 
  list_fold (fun n ->
    list_fold ( fun c sol ->
      match  Model.eval (get_x x n c) ~model:model with
      | Some expr -> 
          let b = bool_of_string (Expr.to_string expr) in
            if b then (n, c)::sol
            else sol
      | None -> raise (Failure "model2sol: eval failed")
    ) colors
  ) nodes []


let coloring : graph * color list -> bool * (node * color) list option 
=fun ((nodes, edges), colors) -> 
  let c = encode (nodes, edges) colors in
    let x = create_x nodes colors in
    let f = trans x c in
    let _, model_opt = Solver.check_satisfiability [f] in
    match model_opt with
    | None -> false, None
    | Some model -> true, Some (model2sol x nodes colors model)

let run () = 
  match coloring instance1 with 
  | false, _ -> print_endline "No solution"
  | true, Some sol -> 
    List.iter (fun (n, c) -> 
      print_endline ("node " ^ string_of_int n ^ " |-> color " ^ string_of_int c)
    ) sol 
  | _ -> raise (Failure "XXX")