open Syntax
open Graph
(* open Utils *)
open Smt


type basic_path = Node.t list
type basic_paths = basic_path list

let rec typ_to_string: typ -> string
  = fun typ -> match typ with
    | T_int -> "INT"
    | T_bool -> "Bool"
    | T_arr t -> "Array [" ^ (typ_to_string t) ^ "]"

let rec typ_trans: typ -> Sort.t
= fun typ -> match typ with
  | T_int -> Expr.sort_of_int()
  | T_bool -> Expr.sort_of_bool()
  | T_arr t -> Expr.sort_of_arr (typ_trans t)

(* basic path 뽑아내기 *)
let get_basic_paths : Graph.Cfg.t -> basic_paths
= fun cfg -> 
  let entry = Cfg.get_entry cfg in

let rec search_succs_path : Node.t -> basic_path -> basic_paths -> basic_paths
  = fun n path paths -> 
    let succs_list = NodeSet.to_list (Cfg.succs n cfg) in 
    match Node.get_instr n with
    | I_function_entry
    | I_assign _
    | I_assume _
    | I_skip 
    | I_break
    | I_return _ -> 
      let next_path = path @ [n] in 
      search_succs_path (List.nth succs_list 0) next_path paths
    | I_if_entry -> 
      let next_path = path @ [n] in 
      (search_succs_path (List.nth succs_list 0) next_path paths) @ (search_succs_path (List.nth succs_list 1) next_path paths)
    | I_if_exit -> 
      let next_path = path @ [n] in 
      search_succs_path (List.nth succs_list 0) next_path paths
    | I_loop_entry -> 
      (* let next_paths = paths @ [path] in *)
      let next_paths = paths @ [path @ [n]] in
      (match List.find_opt (fun n' -> Cfg.is_loophead n' cfg) path with
      | Some _ -> next_paths
      | None -> let next_path = [n] in
        next_paths @ (search_succs_path (List.nth succs_list 0) next_path []) @ (search_succs_path (List.nth succs_list 1) next_path []))
    | I_loop_exit -> 
      let next_path = path @ [n] in 
      search_succs_path (List.nth succs_list 0) next_path paths
    | I_function_exit -> paths @ [path @ [n]]
    | I_call _ -> 
      (* print_endline "I_call 어떻게 해야하지?"; *)
      let next_paths = paths @ [path @ [n]] in
      let next_path = path @ [n] in 
      next_paths @ search_succs_path (List.nth succs_list 0) next_path [] in

    search_succs_path entry [] []

let print_basic_path : basic_path -> unit
= fun path -> match path with
  | hd::path -> print_string (string_of_int (Node.get_nodeid hd)); 
    List.iter (fun n ->
      print_string (" -> " ^(string_of_int (Node.get_nodeid n)))
    ) path;
    print_endline "";
  | _ -> ignore ""

let print_basic_paths : basic_paths -> unit
= fun paths -> 
  print_endline "basic_paths : ";
  List.iteri (fun i path ->
    print_string ((string_of_int (i + 1)) ^ " : ");
    print_basic_path path;
  ) paths;
  print_endline ""


(* trans *)
let rec exp_trans: exp -> Fmla.t
= fun e -> match e with
| E_int i -> Expr.of_int i
| E_bool b -> Expr.of_bool b 
| E_lv lv -> (
  (* print_endline ("여기 호출" ^ string_of_lv lv); *)
  match lv with
  | V_var id -> Expr.create_var (Expr.sort_of_int()) ~name: id
  | V_arr (id, e) -> Expr.read_arr (Expr.create_var (Expr.sort_of_arr(Expr.sort_of_int())) ~name: id) ~idx:(exp_trans e)
)
| E_arr_update (id, e1, e2) -> 
  let arr = Expr.create_var (Expr.sort_of_arr(Expr.sort_of_int())) ~name: id in 
  let up_arr = Expr.update_arr arr ~idx:(exp_trans e1) ~value:(exp_trans e2) in
  Expr.create_eq arr up_arr
  (* up_arr *)
| E_add (e1, e2) -> Expr.create_add (exp_trans e1) (exp_trans e2)
| E_sub (e1, e2) -> Expr.create_sub (exp_trans e1) (exp_trans e2)
| E_mul (e1, e2) -> Expr.create_mul (exp_trans e1) (exp_trans e2)
| E_div (e1, e2) -> Expr.create_div (exp_trans e1) (exp_trans e2)
| E_neg e ->  Expr.create_neg (exp_trans e)
| E_len id ->  Expr.create_var (Expr.sort_of_int()) ~name: ("|" ^ id ^ "|")
| E_not e -> Expr.create_not (exp_trans e)
| E_eq (e1, e2) -> Expr.create_eq (exp_trans e1) (exp_trans e2)
| E_neq (e1, e2) -> Expr.create_neq (exp_trans e1) (exp_trans e2)
| E_lt (e1, e2) -> Expr.create_lt (exp_trans e1) (exp_trans e2)
| E_gt (e1, e2) -> Expr.create_gt (exp_trans e1) (exp_trans e2)
| E_le (e1, e2) -> Expr.create_le (exp_trans e1) (exp_trans e2)
| E_ge (e1, e2) -> Expr.create_ge (exp_trans e1) (exp_trans e2)

let lv_trans : lv -> typ -> Expr.t
= fun lv t-> 
  match lv with
| V_var id -> Expr.create_var (typ_trans t) ~name: id
| V_arr (id, e) -> Expr.read_arr (Expr.create_var (Expr.sort_of_arr((typ_trans t))) ~name: id) ~idx:(exp_trans e)

let rec fmla_trans: fmla -> Fmla.t
= fun fmla -> 
  (* print_endline (string_of_fmla fmla);  *)

  (* bool type 취급해야하는 경우 처리 *)
  let fmla_trans_check_lv : fmla -> Fmla.t
    = fun f -> 
      match f with
      | F_exp (E_lv (V_var id)) -> lv_trans (V_var id) T_bool
      | _ -> fmla_trans f in
      
  match fmla with
  | F_exp e -> Fmla.create_exp (exp_trans e)
  (* | F_order (e1_l, e2_l) -> Fmla.create_imply *)
  | F_not f -> Fmla.create_not (fmla_trans f)
  | F_and f_l -> Fmla.create_and (List.map (fun f -> fmla_trans f) f_l)
  | F_or f_l -> Fmla.create_or (List.map (fun f -> fmla_trans f) f_l)
  | F_imply (f1, f2) -> 
    (* print_endline (string_of_fmla f1 ^ " ::: " ^ string_of_fmla f2); 
    print_endline (Fmla.to_string (fmla_trans f1) ^ " ::: " ^ Fmla.to_string (fmla_trans f2));  *)
    Fmla.create_imply (fmla_trans_check_lv f1) (fmla_trans_check_lv f2)
  | F_iff (f1, f2) -> 
    Fmla.create_iff (fmla_trans_check_lv f1) (fmla_trans_check_lv f2)
  | F_forall (id, Some t, f) -> Fmla.create_forall (Expr.create_var (typ_trans t) ~name: id) (fmla_trans f)
  | F_forall (id, None, f) -> Fmla.create_forall (Expr.create_var (Expr.sort_of_int()) ~name: id) (fmla_trans f)
  | F_exists (id, Some t, f) -> Fmla.create_exists (Expr.create_var (typ_trans t) ~name: id) (fmla_trans f)
  | F_exists (id, None, f) -> Fmla.create_exists (Expr.create_var (Expr.sort_of_int()) ~name: id) (fmla_trans f)
  | F_sorted (id, e1, e2) -> let v_i = E_lv (V_var "var_i")  in
    let v_j = E_lv (V_var "var_j") in
    let f_v_i = exp_trans v_i in
    let f_v_j = exp_trans v_j in
    let l = exp_trans e1 in
    let u = exp_trans e2 in
    Fmla.create_forall (f_v_i) (
      Fmla.create_forall (f_v_j) (
        Fmla.create_imply 
          (Fmla.create_and [Expr.create_le l f_v_i; Expr.create_le f_v_i f_v_j; Expr.create_le f_v_j u]) 
          (Expr.create_le (lv_trans (V_arr (id, v_i)) T_int ) (lv_trans (V_arr (id, v_j)) T_int))
      )
    )
  | F_partitioned (id, e1, e2, e3, e4) -> let v_i = E_lv (V_var "var_i")  in
    let v_j = E_lv (V_var "var_j") in
    let f_v_i = exp_trans v_i in
    let f_v_j = exp_trans v_j in
    let l1 = exp_trans e1 in
    let u1 = exp_trans e2 in
    let l2 = exp_trans e3 in
    let u2 = exp_trans e4 in
    Fmla.create_forall (f_v_i) (
      Fmla.create_forall (f_v_j) (
        Fmla.create_imply 
          (Fmla.create_and [Expr.create_le l1 f_v_i; Expr.create_le f_v_i u1;  Expr.create_le u1 l2; Expr.create_le l2 f_v_j; Expr.create_le f_v_j u2;]) 
          (Expr.create_le (lv_trans (V_arr (id, v_i)) T_int) (lv_trans (V_arr (id, v_j)) T_int))
      )
    )
  | _ -> print_endline (string_of_fmla fmla); raise (Failure "정의 안된 fmla_trans")

let inv_trans : inv option -> Fmla.t
= fun inv_option -> match inv_option with
  (* | Some (_id, fmla) -> print_endline (string_of_fmla fmla); print_endline (Fmla.to_string (fmla_trans fmla)); fmla_trans fmla *)
  | Some (_id, fmla) -> fmla_trans fmla
  | None -> Fmla.true_()

let get_fmla_from_inv_option : inv option -> fmla
= fun inv_option -> match inv_option with
  | Some (_id, fmla) -> fmla
  | None -> F_exp (E_eq (E_int 1, E_int 0))

(* skip 해도 되는 node 필터링  *)
let filter_skip_node: Node.t list -> Node.t list
= fun n_l -> List.filter (fun n -> 
  match Node.get_instr n with
  | I_assign _
  | I_assume _
  | I_return _
  | I_call _
  | I_loop_entry
  | I_function_entry
  | I_function_exit -> true
  | _ -> false
) n_l


let path_trans : basic_path -> Cfg.t -> Fmla.t
= fun path cfg -> 
  (* let fmla_list = if (Cfg.is_loophead (List.nth path 0) cfg) then List.tl path else path in *)
  let r_node_list = List.rev (filter_skip_node path) in
  (* Weakest Precondition *)
  let n_post = List.hd r_node_list in
  let f_post = Cfg.get_invariant n_post cfg |> get_fmla_from_inv_option in
  (* print_endline (Node.to_string (List.hd r_node_list) ^ " : " ^ (string_of_fmla f_post)); *)
  let f_path = List.fold_left (fun post n  -> 
    (* print_endline ((Node.to_string n) ^ " -> " ^ (string_of_fmla post)); *)
    let f = match Node.get_instr n with
    | I_assume exp -> F_imply ((F_exp exp), post)
    | I_assign (lv, exp) -> (
      match lv with
      | V_var id -> replace_fmla id exp post
      | V_arr (id, e) -> F_imply ((F_exp (E_arr_update (id, e, exp))), post)
    )
    | I_return exp -> replace_fmla "rv" exp post
    | I_call (id1, _fun_name, _param_l) ->
      if (Node.compare n_post n) = 0 then 
        (* let _ = print_endline "I_call 이거 호출되긴 함?" in *)
        let pre = Cfg.get_invariant n cfg |> get_fmla_from_inv_option in 
          F_iff (pre, post)
      else
        (* 재귀 함수에서만 해당 *)
        let v_id1 = E_lv (V_var id1) in 
        let real_post = Cfg.get_invariant (Cfg.get_exit cfg) cfg |> get_fmla_from_inv_option in
        replace_fmla "rv" v_id1 real_post
    | I_loop_entry 
    | I_function_entry  
    | I_function_exit -> let pre = Cfg.get_invariant n cfg |> get_fmla_from_inv_option in 
      print_endline ("" ^ string_of_fmla (F_iff (pre, post))); 
     F_iff (pre, post)
    | _ -> raise (Failure ("no!! " ^ Node.to_string n)) in
    f
  ) f_post (List.tl r_node_list)  in
  fmla_trans f_path
  

let args_trans: decl list -> Expr.t list
= fun args -> List.map (fun (typ, id) -> 
    Expr.create_var (typ_trans typ) ~name: id
  ) args
  
let verify_partial_correctness : Syntax.pgm -> bool 
=fun pgm -> 
  let cfg = Graph.pgm2cfg pgm in 
  (* Basic Path 만들기 *)
  let basic_paths = get_basic_paths cfg in
  print_basic_paths basic_paths;
  List.fold_left (fun r path -> 
    match r with
    | true -> (
      print_basic_path path;
      let f_path = path_trans path cfg in
      Fmla.to_string f_path |> print_endline;
      print_endline "";
      match Solver.check_satisfiability [f_path] with
      | _, Some _model -> true
      | _, None -> false 
    )
    | false -> false
    ) true basic_paths
  (* let result = List.fold_left (fun f path -> 
      print_basic_path path;
      let f_path = path_trans path cfg in
      Fmla.create_imply f_path f
  ) (Expr.true_()) basic_paths in
  match Solver.check_satisfiability [result] with
      | _, Some _model -> true
      | _, None -> false  *)

let verify_termination : Syntax.pgm -> bool 
=fun pgm -> 
  let cfg = Graph.pgm2cfg pgm in 
  ignore cfg; true 
