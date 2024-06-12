open Cfg 
open Syntax 
  
type dbm_var_map = (string, int) Hashtbl.t
type dbm_matrix = int array array
type dbm = dbm_var_map * dbm_matrix
type worklist = NodeSet.t

(* output node_id  : dbs *)
type abs_table = (int, dbm option) Hashtbl.t

(* int에서 infinity를 표현하기 위해서 어쩔 수 없음 *)
let pos_inf = max_int;;
let neg_inf = min_int;;

let add_i : int -> int -> int
= fun x y -> 
  if x = pos_inf && y = neg_inf then 0
  else if x = neg_inf && y = pos_inf then 0
  else if x = pos_inf || y = pos_inf then pos_inf
  else if x = neg_inf || y = neg_inf then neg_inf
  else x + y

let sub_i : int -> int -> int
= fun x y -> 
  if x = pos_inf && y = pos_inf then 0
  else if x = neg_inf && y = neg_inf then 0
  else if x = pos_inf || y = neg_inf then pos_inf
  else if x = neg_inf || y = pos_inf then neg_inf
  else x - y

let mult_i : int -> int -> int
= fun x y -> 
  if x = 0 || y = 0 then 0
  else if x = pos_inf || y < 0 then neg_inf
  else if x = neg_inf || y < 0 then pos_inf
  else if x < 0 || y = pos_inf then neg_inf
  else if x < 0 || y = neg_inf then pos_inf
  else x * y

let get_abs_table_key : Node.t -> int
= fun n -> Node.get_nodeid n

let mult_vars : string -> string -> string
= fun var1 var2 -> if var1 < var2 then var1 ^ "*" ^ var2 else var2 ^ "*" ^ var1

(* 출력용 *)
let print_dbm : dbm -> unit
= fun (var_map, matrix) ->
  let vars = Hashtbl.to_seq_keys var_map |> List.of_seq |> (List.sort compare) in
  let n = Array.length matrix in
  Printf.printf "      ";
  for i = 0 to n - 1 do
    Printf.printf "%-5s " (List.nth vars i)
  done;
  print_newline ();
  for i = 0 to n - 1 do
    Printf.printf "%-5s " (List.nth vars i); 
    for j = 0 to n - 1 do
      let v = matrix.(i).(j) in
      if v = pos_inf then
        Printf.printf "%-5s " "inf"
      else if v = neg_inf then
        Printf.printf "%-5s " "-inf"
      else
        Printf.printf "%-5d " v
    done;
    print_newline ()
  done;
  print_endline "-------------------------"

let print_dbm_op : dbm option -> unit
= fun dbm_op -> match dbm_op with
  | None -> print_endline "⊥"
  | Some dbm -> print_dbm dbm

let print_abs_table : abs_table -> unit
= fun abs_table ->
  print_endline "Abs Table:";
  Hashtbl.to_seq_keys abs_table |> List.of_seq |> (List.sort compare) |> List.iter (fun id -> 
    Printf.printf "(%d)\n" id;
    match (Hashtbl.find_opt abs_table id) with
    | None -> print_endline "⊥"
    | Some dbm_op -> match dbm_op with
      | None -> print_endline "⊥"
      | Some dbm -> print_dbm dbm
  );
  print_endline "-------------------------------"

let string_of_range: (int * int) -> string
= fun (l, u) -> "[" ^ (string_of_int l) ^ ", " ^ (string_of_int u) ^ "]"

let print_range: (int * int) -> unit
= fun (l, u) -> print_endline (string_of_range (l, u))


let print_debug_point : int -> Node.t -> abs_table -> unit
= fun node_id current_n abs_table ->
  if Node.get_nodeid current_n = node_id then
  print_endline "";
  print_endline ("Debug Point:" ^ (string_of_int node_id));
  print_endline "";
  print_abs_table abs_table

let print_debug_point_dbm : int -> Node.t -> dbm -> unit
  = fun node_id current_n dbm ->
    if Node.get_nodeid current_n = node_id then
      let _ = print_endline ("Debug Point DBM :" ^ (string_of_int node_id)) in
      let _ = print_endline "" in
      print_dbm dbm
    else
      ()

let print_debug_point_dbm_op : int -> Node.t -> dbm option -> unit
= fun node_id current_n dbm_op ->
  if Node.get_nodeid current_n = node_id then
    let _ = print_endline ("Debug Point DBM :" ^ (string_of_int node_id)) in
    let _ = print_endline "" in
    match dbm_op with
    | None -> print_endline "⊥"
    | Some dbm -> print_dbm dbm
  else
    ()

let print_debug_point_string : int -> Node.t -> string -> unit
= fun node_id current_n str ->
  if Node.get_nodeid current_n = node_id then
    print_endline str

(* 곱하기를 추가하기 위한 확장 *)
let extended_mult_vars : string list -> string list
= fun vars ->
  let n = List.length vars in
  let new_vars = ref [] in
  for i = 0 to n - 1 do
    for j = i to n - 1 do
      new_vars := (mult_vars (List.nth vars i) (List.nth vars j)) :: !new_vars
    done
  done;
  !new_vars

(* dbm을 초기화 *)
let dbm_init : string list -> dbm
= fun vars -> 
  (* let extended_vars = ["0"] @ vars @ (extended_mult_vars vars) |> (List.sort compare) in *)
  let extended_vars = ["0"] @ vars |> (List.sort compare) in
  let n = List.length extended_vars in 
  
  let dbm_var_map = Hashtbl.create n in 
  let _ = List.iteri (fun i var -> Hashtbl.add dbm_var_map var i) extended_vars in

  let matrix = Array.make_matrix n n pos_inf in
  for i = 0 to (n - 1) do 
    for j = 0 to (n - 1) do 
      if i = j then 
        matrix.(i).(j) <- 0
      else 
        matrix.(i).(j) <- pos_inf
    done
  done;
  (dbm_var_map, matrix)

let copy_dbm : dbm -> dbm
= fun (var_map, matrix) -> 
  let n = Array.length matrix in 
  let copied_matrix = Array.make_matrix n n pos_inf in
  for i = 0 to n - 1 do 
    for j = 0 to n - 1 do 
      copied_matrix.(i).(j) <- matrix.(i).(j)
    done
  done;
  (var_map, copied_matrix)

let abs_table_init : Cfg.t -> var list -> abs_table
= fun cfg vars -> 
  let init_dbm = dbm_init vars in
  let abs_table = Hashtbl.create (List.length (Cfg.nodesof cfg)) in
  let _ = Hashtbl.add abs_table 0 (Some init_dbm) in
  abs_table

let get_dbm_from_abs_table : abs_table -> Node.t -> dbm option
= fun abs_table n ->
  let at_key = get_abs_table_key n in
  match (Hashtbl.find_opt abs_table at_key) with
  | None -> None
  | Some dbm_op -> dbm_op

let get_init_dbm : abs_table -> dbm
= fun abs_table -> 
  match (Hashtbl.find_opt abs_table 0) with
  | None -> raise (Failure "get_init_dbm : dbm is None")
  | Some dbm_op -> match dbm_op with
    | None -> raise (Failure "get_init_dbm : dbm is None")
    | Some dbm -> dbm


let get_index_from_var : dbm -> string -> int
= fun (var_map, _matrix) var -> Hashtbl.find var_map var

let get_zero_index : dbm -> int
= fun dbm -> get_index_from_var dbm "0"

let get_vars: dbm -> string list
= fun (var_map, _matrix) -> Hashtbl.to_seq_keys var_map |> List.of_seq |> (List.sort compare)

(* 범위만 구해줬을뿐 추가로 연산자 처리를 해줘야한다  *)
let get_range_from_var : dbm -> string -> (int * int)
= fun dbm var -> 
  let (_, matrix) = dbm in
  let z_i = get_zero_index dbm in 
  let v_i = get_index_from_var dbm var in
  (-matrix.(v_i).(z_i), matrix.(z_i).(v_i))

(* var1 - var2 *)
let get_range_from_vars : dbm -> string -> string -> (int * int)
  = fun dbm var1 var2 -> 
    let (_, matrix) = dbm in
    let v1_i = get_index_from_var dbm var1 in 
    let v2_i = get_index_from_var dbm var2 in
    (-matrix.(v1_i).(v2_i), matrix.(v2_i).(v1_i))

let set_range_to_var : dbm -> string -> (int * int) -> unit
= fun (var_map, matrix) var (l, u) -> 
  let z_i = get_zero_index (var_map, matrix) in 
  let v_i = get_index_from_var (var_map, matrix) var in
  matrix.(z_i).(v_i) <- u;
  matrix.(v_i).(z_i) <- -l

let set_range_to_vars : dbm -> string -> string -> (int * int) -> unit
= fun (var_map, matrix) var1 var2 (l, u) -> 
  let v1_i = get_index_from_var (var_map, matrix) var1 in
  let v2_i = get_index_from_var (var_map, matrix) var2 in
  matrix.(v1_i).(v2_i) <- -l;
  matrix.(v2_i).(v1_i) <- u

let rec aexp2range : aexp -> dbm -> (int * int)
= fun aexp dbm -> match aexp with
| Const i -> (i, i)
| Var id -> get_range_from_var dbm id
| Add (aexp1, aexp2) ->
  let (l1, u1) = aexp2range aexp1 dbm in 
  let (l2, u2) = aexp2range aexp2 dbm in 
  (add_i l1 l2, add_i u1 u2)
| Sub (aexp1, aexp2) -> 
  let (l1, u1) = aexp2range aexp1 dbm in 
  let (l2, u2) = aexp2range aexp2 dbm in 
  (sub_i l1 u2, sub_i u1 l2)
| Mult (Var id1, Var id2) ->
  let mult_var = mult_vars id1 id2 in
  get_range_from_var dbm mult_var
| Mult (aexp1, aexp2) ->
  let (l1, u1) = aexp2range aexp1 dbm in 
  let (l2, u2) = aexp2range aexp2 dbm in 
  let l = min (min (mult_i l1 l2) (mult_i l1 u2)) (min (mult_i u1 l2) (mult_i u1 u2)) in 
  let u = max (max (mult_i l1 l2) (mult_i l1 u2)) (max (mult_i u1 l2) (mult_i u1 u2)) in 
  (l, u)

(* Forget operator를 적용 *)
let forget_operator : dbm -> string -> dbm
= fun (var_map, matrix) var -> 
  let n = Array.length matrix - 1 in 
  let i = Hashtbl.find var_map var in 
  for j = 0 to n do 
    matrix.(i).(j) <- pos_inf;
    matrix.(j).(i) <- pos_inf
  done;
  (var_map, matrix)

(* Floyd-Warshall algorithm을 이용 *)
(* let closure_dbm : dbm -> dbm
= fun (var_map, matrix) ->
  let n = Array.length matrix in
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if matrix.(i).(k) <> pos_inf && matrix.(k).(j) <> pos_inf then
          let new_val = matrix.(i).(k) + matrix.(k).(j) in
          if new_val < matrix.(i).(j) then
            matrix.(i).(j) <- new_val
      done
    done
  done;
  (var_map, matrix) *)

(* Floyd-Warshall algorithm을 이용 *)
let closure_dbm_op : dbm -> dbm option
= fun (var_map, matrix) ->
  let n = Array.length matrix in
  let dist = Array.map Array.copy matrix in
  
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if dist.(i).(k) <> max_int && dist.(k).(j) <> max_int then
          let new_dist = dist.(i).(k) + dist.(k).(j) in
          if new_dist < dist.(i).(j) then
            dist.(i).(j) <- new_dist
      done
    done
  done;
  
  (* Check for negative weight cycles *)
  let negative_cycle = ref false in
  for i = 0 to n - 1 do
    if dist.(i).(i) < 0 then negative_cycle := true
  done;
  
  if !negative_cycle then None
  else Some (var_map, dist)

let closure_dbm : dbm -> dbm
= fun dbm -> 
  match closure_dbm_op dbm with
  | None -> dbm
  | Some dbm -> dbm

let is_contain_var_at_aexp : aexp -> bool
= fun aexp ->
  let rec aux : aexp -> bool
  = fun aexp -> match aexp with
    | Const _ -> false
    | Var _ -> true
    | Add (aexp1, aexp2) -> (aux aexp1) || (aux aexp2)
    | Sub (aexp1, aexp2) -> (aux aexp1) || (aux aexp2)
    | Mult (aexp1, aexp2) -> (aux aexp1) || (aux aexp2)
  in
  aux aexp

let rec is_contain_specific_var_at_aexp : aexp -> var -> bool
= fun aexp var -> match aexp with
  | Const _ -> false
  | Var id -> id = var
  | Add (aexp1, aexp2) -> (is_contain_specific_var_at_aexp aexp1 var) || (is_contain_specific_var_at_aexp aexp2 var)
  | Sub (aexp1, aexp2) -> (is_contain_specific_var_at_aexp aexp1 var) || (is_contain_specific_var_at_aexp aexp2 var)
  | Mult (aexp1, aexp2) -> (is_contain_specific_var_at_aexp aexp1 var) || (is_contain_specific_var_at_aexp aexp2 var)


let aexp2range4assign : aexp -> dbm -> (int * int)
= fun aexp dbm ->
  match aexp with
  | Const i -> (i, i)
  | Var id -> get_range_from_var dbm id
  | Add (Var _id, aexp) 
  | Add (aexp, Var _id) ->
    aexp2range aexp dbm
  | Sub (Var _id, aexp)
  | Sub (aexp, Var _id) ->
    aexp2range aexp dbm
  | _ -> aexp2range aexp dbm

(* Assign operator를 적용 *)
let assign_operator : dbm -> string -> aexp -> dbm
  = fun dbm var aexp -> 
    if is_contain_var_at_aexp aexp then
      (* 변수가 포함되어 있는 경우 해당 값을 모두 적용 *)
      (* x = x + 1 : x - x' <= c -> x - x' <= c + 1 *)
      let (l, u) = aexp2range4assign aexp dbm in
      (* set_range_to_var dbm var (l, u); *)
      print_range (l, u);
      let vars' =  get_vars dbm in
      List.iter (fun var' -> 
        let (l', u') = get_range_from_vars dbm var var' in
        print_string (var ^ "-" ^ var' ^ " : ");
        print_string (string_of_range (l', u'));
        print_string " -> ";
        if var = var' then
          print_endline ""
        (* else if var' = "0" then
          let _ = set_range_to_vars dbm var var' (l, u) in 
          print_endline (string_of_range (l, u)); *)
        else 
          let r = (add_i l' l, add_i u' u) in
          let _ = set_range_to_vars dbm var var' r in
          print_endline (string_of_range r);
      ) vars';
      print_dbm dbm;
      dbm
      
    else
      (* 1. Remove information about X *)
      let (var_map, matrix) = forget_operator dbm var in

      (* 2. Add constraint *)
      let v_i = get_index_from_var (var_map, matrix) var in
      let z_i = get_zero_index (var_map, matrix) in
      let (l, u) = aexp2range aexp (var_map, matrix) in
      matrix.(z_i).(v_i) <- l;
      matrix.(v_i).(z_i) <- -u;
      (* 3. Normalize the resulting state *)
      closure_dbm (var_map, matrix)

(* Assume operator를 적용 *)
let rec assume_operator : dbm -> bexp -> dbm
= fun dbm bexp -> match bexp with
  | True -> dbm
  | False -> dbm
  | Le (Var id1, Var id2) ->
    let (l1, u1) = get_range_from_var dbm id1 in
    let (_l2, u2) = get_range_from_var dbm id2 in
    set_range_to_var dbm id1 (l1, min u1 u2);
    dbm
  | Le (Var id, aexp) ->
    let (v_l, v_u) = get_range_from_var dbm id in
    let (_l, u) = aexp2range aexp dbm in
    set_range_to_var dbm id (v_l, min v_u u);
    dbm
  | Le (aexp, Var id) ->
    let (v_l, v_u) = get_range_from_var dbm id in
    let (_l, u) = aexp2range aexp dbm in
    set_range_to_var dbm id (max v_l u, v_u);
    (* set_range_to_var dbm id (v_l, max v_u u); *)
    dbm
  | Not (Le (Var id, aexp)) ->
    assume_operator dbm (Le (Add (aexp, Const 1), Var id))
  | Not (Le (aexp, Var id)) ->
    assume_operator dbm (Le (Var id, Add (aexp, Const 1)))
  | Not (And (bexp1, bexp2)) ->
      let dbm2 = assume_operator dbm (Not bexp1) in
      assume_operator dbm2 (Not bexp2)
  | And (bexp1, bexp2) -> 
    let updated_dbm = assume_operator dbm bexp1 in
    assume_operator updated_dbm bexp2
  | _ -> raise (Failure ("Not implemented : " ^ string_of_bexp bexp))

(* Node를 dbm에 적용 *)
let compute_node : Node.t -> dbm -> dbm
= fun node dbm ->
  match (Node.get_instr node) with
  | I_assign (s, aexp) -> 
    (* 2. Add constraint *)
    assign_operator dbm s aexp
   
  | I_assume bexp -> 
    (* print_endline (string_of_bexp bexp); *)
    let updated_dbm = assume_operator dbm bexp in
    (* 3. Normalize the resulting state *)
    closure_dbm updated_dbm
  | I_assert _bexp -> dbm
  | I_skip -> dbm

let join_dbm : dbm option -> dbm option -> dbm
= fun old_dbm_op new_dbm_op -> 
  match (old_dbm_op, new_dbm_op) with
  | (None, None) -> raise (Failure "join_dbm : both dbm are None")
  | (None, Some new_dbm) -> new_dbm
  | (Some old_dbm, None) -> old_dbm
  | (Some old_dbm, Some new_dbm) -> 
    let copied_dbm = copy_dbm new_dbm in
    let vars = get_vars old_dbm in
    List.iter (fun var -> 
      let (l1, u1) = get_range_from_var old_dbm var in
      let (l2, u2) = get_range_from_var new_dbm var in
      set_range_to_var copied_dbm var (min l1 l2, max u1 u2)
    ) vars;
    copied_dbm

let meet_dbm : dbm -> dbm -> dbm
= fun old_dbm new_dbm ->
  let vars = get_vars old_dbm in
    List.iter (fun var -> 
      let (l1, u1) = get_range_from_var old_dbm var in
      let (l2, u2) = get_range_from_var new_dbm var in
      if (l1 <= l2) && (l2 <= u1) then
        set_range_to_var new_dbm var (l2, u1)
      else if (l2 <= l1) && (l1 <= u2) then
        set_range_to_var new_dbm var (l1, u2)
      else
        set_range_to_var new_dbm var (0, 0)
    ) vars;
    new_dbm

let partial_order_dbm : dbm -> dbm -> bool
= fun dbm1 dbm2 ->
  let vars = get_vars dbm1 in
  let is_le = ref true in
  List.iter (fun var -> 
    let (l1, u1) = get_range_from_var dbm1 var in
    let (l2, u2) = get_range_from_var dbm2 var in
    if not ((l2 <= l1) && (u1 <= u2)) then
      is_le := false
  ) vars;
  !is_le

let widening_dbm : dbm option -> dbm option -> dbm
= fun old_dbm_op new_dbm_op ->
  match (old_dbm_op, new_dbm_op) with
  | (None, None) -> raise (Failure "widening_dbm : both dbm are None")
  | (None, Some new_dbm) -> new_dbm
  | (Some old_dbm, None) -> old_dbm
  | (Some old_dbm, Some new_dbm) -> 
    let vars = get_vars old_dbm in
    List.iter (fun var -> 
      let (l1, u1) = get_range_from_var old_dbm var in
      let (l2, u2) = get_range_from_var new_dbm var in
      let wided_l = if l1 > l2 then neg_inf else l1 in
      let wided_u = if u1 < u2 then pos_inf else u1 in
      set_range_to_var new_dbm var (wided_l, wided_u)
    ) vars;
    new_dbm

let narrowing_dbm : dbm option -> dbm option -> dbm 
= fun old_dbm_op new_dbm_op ->
  match (old_dbm_op, new_dbm_op) with
  | (None, None) -> raise (Failure "narrowing_dbm : both dbm are None")
  | (None, Some _new_dbm) -> raise (Failure "narrowing_dbm : bottom dbm is None")
  | (Some _old_dbm, None) -> raise (Failure "narrowing_dbm : bottom dbm is None")
  | (Some old_dbm, Some new_dbm) -> 
    let vars = get_vars old_dbm in
    List.iter (fun var -> 
      let (l1, u1) = get_range_from_var old_dbm var in
      let (l2, u2) = get_range_from_var new_dbm var in
      let narrowed_l = if l1 = neg_inf then l2 else l1 in
      let narrowed_u = if u1 = pos_inf then u2 else u1 in
      (* set_range_to_var new_dbm var (min narrowed_l narrowed_u, max narrowed_l narrowed_u) *)
      set_range_to_var new_dbm var (min narrowed_l narrowed_u, max narrowed_l narrowed_u)
    ) vars;
    new_dbm

let is_entry_node : Node.t -> Cfg.t -> bool
= fun node cfg -> NodeSet.mem node (Cfg.entries cfg)

let is_node_skip_assert : Node.t -> bool
= fun node -> match (Node.get_instr node) with
  | I_assert _bexp -> true
  | I_skip -> true
  | _ -> false

let is_fixed_point : dbm option -> dbm option -> bool
  = fun dbm1 dbm2 -> match (dbm1, dbm2) with
    | (None, None) -> false
    | (None, Some _) -> false
    | (Some _, None) -> false
    | (Some dbm1, Some dbm2) -> partial_order_dbm dbm1 dbm2
  

let inputof_node : Node.t -> Cfg.t -> abs_table -> dbm
= fun n cfg abs_table -> 
  let pred_ns = Cfg.preds n cfg |> NodeSet.to_list in
 
  let pred_dbms = List.map (fun pred_n -> get_dbm_from_abs_table abs_table pred_n) pred_ns in

  let old_dbm_op = if is_entry_node n cfg then Some (get_init_dbm abs_table) else None in
  let joined_dbm_op = List.fold_left (fun joined_dbm_op pred_dbm_op ->   
    Some (join_dbm joined_dbm_op pred_dbm_op)
  ) old_dbm_op pred_dbms in

  match joined_dbm_op with
  | None -> raise (Failure "inputof_node : joined_dbm_op is None")
  | Some joined_dbm -> joined_dbm

let analyze_node : Node.t -> dbm -> dbm 
= fun n dbm ->
  let copied_dbm = copy_dbm dbm in
  if is_node_skip_assert n then
    copied_dbm
  else
    let computed_dbm = compute_node n copied_dbm in
    (* closure_dbm computed_dbm *)
    computed_dbm

(* 노드 중복 제거 *)
let remove_duplicates lst =
  let set = List.fold_right NodeSet.add lst NodeSet.empty in
  NodeSet.elements set

let compute_fixed_point_widening : var list -> Cfg.t -> abs_table
= fun vars cfg ->
  let entries = NodeSet.to_list (Cfg.entries cfg) in
  let abs_table = abs_table_init cfg vars in
  let worklist = ref [] in
  worklist := entries;
  while List.length !worklist > 0 do
    let current_n = List.hd !worklist in
    worklist := List.tl !worklist;

    print_endline " ";
    print_endline ("run : " ^ (string_of_int (Node.get_nodeid current_n)));

    let in_dbm = inputof_node current_n cfg abs_table in

    let _ = print_endline "in_dbm : " in
    let _ = print_dbm in_dbm in

    let out_dbm = analyze_node current_n in_dbm in

    let _ = print_endline "out_dbm : " in
    let _ = print_dbm out_dbm in
    
    let current_dbm = get_dbm_from_abs_table abs_table current_n in
    let _ = print_endline "current_dbm : " in
    let _ = print_dbm_op current_dbm in

    if not (is_fixed_point (Some out_dbm) current_dbm) then
      let succ_ns = NodeSet.to_list (Cfg.succs current_n cfg) in
      let old_dbm = get_dbm_from_abs_table abs_table current_n in
      let at_key = get_abs_table_key current_n in
      if Cfg.is_loophead current_n cfg then
        let widened_dbm = widening_dbm old_dbm (Some out_dbm) in
        
        let _ = print_endline "widened_dbm : " in
        let _ = print_dbm widened_dbm in

        Hashtbl.add abs_table at_key (Some widened_dbm);
        worklist := succ_ns @ !worklist
      else
        let joined_dbm = join_dbm old_dbm (Some out_dbm) in

        let _ = print_endline "joined_dbm : " in
        let _ = print_dbm joined_dbm in

        Hashtbl.add abs_table at_key (Some joined_dbm);
        worklist := succ_ns @ !worklist
  done;
  abs_table

let compute_fixed_point_narrowing : Cfg.t -> abs_table -> abs_table
= fun cfg abs_table ->
  let worklist = ref [] in
  let entries = NodeSet.to_list (Cfg.entries cfg) in
  worklist := entries @ Cfg.nodesof cfg;

  while List.length !worklist > 0 do
    let current_n = List.hd !worklist in
    worklist := remove_duplicates (List.tl !worklist);
      print_endline " ";
      print_endline ("run : " ^ (string_of_int (Node.get_nodeid current_n)));
      
      let in_dbm = inputof_node current_n cfg abs_table in
      let _ = print_endline "in_dbm : " in
      let _ = print_dbm in_dbm in

      let out_dbm = analyze_node current_n in_dbm in
      let _ = print_endline "out_dbm : " in
      let _ = print_dbm out_dbm in

      let at_key = get_abs_table_key current_n in
      let old_dbm = Option.get (get_dbm_from_abs_table abs_table current_n) in

      let _ = print_endline "old_dbm : " in
      let _ = print_dbm old_dbm in
      
      if not (partial_order_dbm old_dbm out_dbm) then
        let narrowing_dbm = narrowing_dbm (Some old_dbm) (Some out_dbm) in

        let _ = print_endline "narrowing_dbm : " in
        let _ = print_dbm narrowing_dbm in

        Hashtbl.add abs_table at_key (Some narrowing_dbm);
        let succ_ns = NodeSet.to_list (Cfg.succs current_n cfg) in
        worklist := succ_ns @ !worklist
  done;
  abs_table

let rec compute_assert : dbm -> bexp -> bool
= fun dbm bexp -> 
  match bexp with
  | True -> true
  | False -> false
  | Equal (aexp1, aexp2) -> 
    let (l, u) = aexp2range aexp1 dbm in
    let (v_l, v_u) = aexp2range aexp2 dbm in
    l = v_l && u = v_u
  | Le  (aexp1, aexp2) ->
    let (l, u) = aexp2range aexp1 dbm in
    let (v_l, v_u) = aexp2range aexp2 dbm in
    l <= v_l && u <= v_u
  | Not bexp -> not (compute_assert dbm bexp)
  | And (bexp1, bexp2) -> (compute_assert dbm bexp1) && (compute_assert dbm bexp2)
  

let compute_assert_count : Cfg.t -> abs_table -> int
= fun cfg abs_table ->
  let count = ref 0 in
  let nodes = Cfg.nodesof cfg in
  List.iter (fun n -> 
    let dbm_op = get_dbm_from_abs_table abs_table n in
    match dbm_op with
    | None -> ()
    | Some dbm -> 
      let instr = Node.get_instr n in
      match instr with
      | I_assert bexp -> 
        print_dbm dbm;
        if compute_assert dbm bexp then
          count := !count + 1
      | _ -> ()
  ) nodes;
  !count

let analyze : Syntax.pgm -> int 
=fun (vars, cmd) -> 
  let cfg = cmd2cfg cmd in 
  let _ = print_endline "compute_fixed_point_widening : " in
  let abs_table = compute_fixed_point_widening vars cfg in
  let _ = print_endline "" in
  let _ = print_endline "" in
  let _ = print_endline "compute_fixed_point_narrowing : " in
  let abs_table = compute_fixed_point_narrowing cfg abs_table in
  let _ = print_endline "" in
  let _ = print_endline "" in
  let _ = print_endline "Result:" in
  let _ = print_abs_table abs_table in
  let _ = print_endline "" in
  let _ = print_endline "compute_assert_count:" in
  compute_assert_count cfg abs_table

