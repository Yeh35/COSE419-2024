open Smt
open Utils 

type pgm = var list * lib list * var 
and lib = var * var list * var * phi 
and var = string 
and phi = EQ of exp * exp 
and exp = 
  | INT of int 
  | VAR of var 
  | ADD of exp * exp  
  | MUL of exp * exp  

type spec = int list * int 

type formula = 
  | True
  | Var of string 
  | INT of int
  | Add of formula * formula
  | Mul of formula * formula
  | And of formula list
  | Or of formula list
  | Neq of formula * formula
  | Eq of formula * formula
  | Be of formula * formula (* big eq *)
  | Le of formula * formula (* litle eq *)
  | Lt of formula * formula (* litle then *)
  | Imply of formula * formula 

let forall f l = And (List.map f l)
let exists f l = Or (List.map f l)


let string_of_pgm (invars, libs, outvar) = 
  "def f(" ^ string_of_list id invars ~first:"" ~last:"" ^ "): " ^ "\n" ^
  list_fold (fun (name, ins, out, _) s -> 
    s ^ "  " ^ out ^ " := " ^ name ^ string_of_list id ins ^ "\n"
  ) libs "" ^ 
  "  return " ^ outvar ^ "\n"

let string_of_format (formula) =
  let rec aux = function
    | True -> "True"
    | Var v -> v
    | INT n -> string_of_int n
    | Add (f1, f2) -> "(" ^ aux f1 ^ " + " ^ aux f2 ^ ")"
    | Mul (f1, f2) -> "(" ^ aux f1 ^ " * " ^ aux f2 ^ ")"
    | And fs -> "(" ^ String.concat " /\\ " (List.map aux fs) ^ ")"
    | Or fs -> "(" ^ String.concat " \\/ " (List.map aux fs) ^ ")"
    | Neq (f1, f2) -> "(" ^ aux f1 ^ " != " ^ aux f2 ^ ")"
    | Eq (f1, f2) -> "(" ^ aux f1 ^ " = " ^ aux f2 ^ ")"
    | Be (f1, f2) -> "(" ^ aux f1 ^ " >= " ^ aux f2 ^ ")"
    | Le (f1, f2) -> "(" ^ aux f1 ^ " <= " ^ aux f2 ^ ")"
    | Lt (f1, f2) -> "(" ^ aux f1 ^ " < " ^ aux f2 ^ ")"
    | Imply (f1, f2) -> "(" ^ aux f1 ^ " => " ^ aux f2 ^ ")"
  in aux formula

let params_encode : string list -> int list -> formula =
  fun vars valus -> 
    List.fold_left (fun f index -> 
      And [f; (Eq (Var (List.nth vars index), INT (List.nth valus index)))]
    ) True (range (List.length vars))

let rec exp_encode : string list -> exp -> formula = 
  fun vars exp -> 
    match exp with 
    | INT n -> INT n
    | VAR v -> Var v
    | ADD (e1, e2) -> 
      Add (exp_encode vars e1, exp_encode vars e2)
    | MUL (e1, e2) -> 
      Mul (exp_encode vars e1, exp_encode vars e2)

let phi_encode : string list -> phi -> formula = 
  fun vars phi -> 
    match phi with 
    | EQ (e1, e2) -> 
      Eq (exp_encode vars e1, exp_encode vars e2)

let lib_encode : lib -> formula = 
  fun (_, ins, _out, phi) -> phi_encode ins phi
    (* Eq (Var out, phi_encode ins phi) *)

let lib_list_encode : lib list -> formula = 
  fun libs -> 
    List.fold_left (fun f lib -> 
      And [f; lib_encode lib]
    ) True libs

let rec trans: formula -> Fmla.t
= fun formula ->
  match formula with
  | True -> Expr.true_()
  | Var v -> Expr.create_var (Expr.sort_of_int()) ~name: (v)
  | INT n -> Expr.of_int n
  | Add (f1, f2) -> Expr.create_add (trans f1) (trans f2)
  | Mul (f1, f2) -> Expr.create_mul (trans f1) (trans f2)
  | And fs -> Fmla.create_and (List.map trans fs)
  | Or fs -> Fmla.create_or (List.map trans fs)
  | Neq (f1, f2) -> Expr.create_neq (trans f1) (trans f2)
  | Eq (f1, f2) -> Expr.create_eq (trans f1) (trans f2)
  | Be (f1, f2) -> Expr.create_ge (trans f1) (trans f2)
  | Le (f1, f2) -> Expr.create_le (trans f1) (trans f2)
  | Lt (f1, f2) -> Expr.create_lt (trans f1) (trans f2)
  | Imply (f1, f2) -> Fmla.create_imply (trans f1) (trans f2)

let verify : pgm -> spec -> bool
= fun (params, libs, result) (s_params, s_result) -> 
  let params_formula = params_encode params  s_params in
  let lib_formula = lib_list_encode libs in
  let result_formula = Eq (Var result, INT s_result) in
  let formula = And [params_formula; lib_formula; result_formula] in
  print_endline (string_of_format formula);
  match Solver.check_satisfiability [trans formula] with
  | _, Some _model -> true
  | _, None -> false

(* ------------------ *)
(* Synthesize *)
(* ------------------ *)

let loc_var : string -> formula
= fun v -> Var ("l_" ^ v)

(* 문자열에서 불필요한 괄호를 제거하고 정수로 변환하는 함수 *)
let parse_to_int s =
  (* 괄호 제거 *)
  let clean_string =
    s
    |> String.to_seq
    |> Seq.filter (fun c -> c <> '(' && c <> ')' && c <> ' ')
    |> String.of_seq
    |> String.trim
  in
  int_of_string clean_string

(* Location 모델에서 꺼내기 *)
let loc_from_model : Model.t -> var -> int
= fun model v ->
  match Model.eval (trans (loc_var v)) ~model:model with
  | Some expr -> (
      (* Fmla.to_string expr |> print_endline;
      Expr.to_string expr |> print_endline; *)
      parse_to_int (Expr.to_string expr)
    )
  | None -> raise (Failure "model2solution")

(* Location 이 같은 변수 찾기 *)
let find_same_loc : Model.t -> var list -> var -> var option
= fun model source v  -> 
  let loc_v = loc_from_model model v in
  match List.find_opt (fun x -> x != v && loc_from_model model x = loc_v) source with
  | Some finded_v -> Some finded_v
  | None -> None

let synthesize : lib list -> spec -> pgm option 
=fun libs (spec_inputs, spec_output) ->
  (* N *)
  let lib_lengh = List.length libs in

  (* I, Lib inputs *)
  let lib_inputs = List.fold_left(fun ins (_name, inputs, _output, _phi) ->
    ins @ inputs
  ) [] libs in 
 
  (* O, Lib outputs *)
  let lib_outputs = List.fold_left(fun outs (_name, _inputs, output, _phi) ->
    outs @ [output]
  ) [] libs in 
  
  (* L, variable *)
  let input_len = List.length spec_inputs in
  let inpput_vars = List.map (fun i -> "I" ^ string_of_int i) (range input_len) in
  let _loc = lib_inputs @ lib_outputs @ inpput_vars @ ["O"] in

  (* Location Condition, input이 여러개 일 수 있음으로 input 한정 마이너스 *)
  let l_c1 = And [
    (* Eq (loc_var "I", INT 0); *)
    (* forall (fun i -> Eq (loc_var i, INT 0)) inpput_vars; *)
    And (List.mapi (fun i x -> Eq (loc_var x, INT (0 - i))) inpput_vars);
    Eq (loc_var "O", INT lib_lengh);
    forall (fun i -> And [Be (loc_var i, INT (0 - input_len + 1)); Le (loc_var i, INT lib_lengh)]) lib_inputs;
    forall (fun o -> And [Be (loc_var o, INT 1); Le (loc_var o, INT lib_lengh)]) lib_outputs;
  ] in 

  (* Location Condition *)
  let l_c2 = forall ( fun o_i -> 
    forall ( fun o_j -> 
      if o_i != o_j then Neq ((loc_var o_i), (loc_var o_j)) else True
      (* Imply (Neq (Var o_i, Var o_j), Neq ((loc_var o_i), (loc_var o_j))) *)
    ) (lib_outputs)
  ) lib_outputs in

  (* Location Condition *)
  let l_c3 = forall (fun (_name, inputs, output, _phi) ->
      forall (fun input -> 
        Lt (loc_var input, loc_var output)
      ) inputs
  ) libs in

  (* All Location Condition *)
  let f_wfp = And [l_c1; l_c2; l_c3] in

  (* Transition Relation *)
  (* let f_conn = And [
    (* I -> O *)
    forall (fun i -> 
      Imply (Eq (loc_var i, loc_var "O"), Eq (Var i, Var "O"))
    ) inpput_vars;
    (* I -> lib i *)
    forall (fun x -> 
      forall (fun y -> 
        Imply (Eq (loc_var x, loc_var y), Eq (Var x, Var y))
      ) lib_inputs
    ) inpput_vars;
    (* lib o -> lib i *)
    forall (fun x -> 
      forall (fun y -> 
        Imply (Eq (loc_var x, loc_var y), Eq (Var x, Var y))
      ) lib_inputs
    ) lib_outputs;
    (* lib o -> O *)
     forall (fun x -> 
      Imply (Eq (loc_var x, loc_var "O"), Eq (Var x, Var "O"))
    ) lib_outputs;
  ] in *)

  let f_conn = forall (fun x -> 
    forall (fun y -> 
      Imply (Eq (loc_var x, loc_var y), Eq (Var x, Var y))
    ) _loc 
  ) _loc in
  

  (* Library *)
  let f_lib = forall (fun lib ->
    lib_encode lib
  ) libs in

  (* I/O *)
  let f_io = And [
    forall (fun i -> 
      Eq (Var (List.nth inpput_vars i), INT (List.nth spec_inputs i))
    ) (range (List.length spec_inputs));
    Eq (Var "O", INT spec_output)
  ] in

  let model2solution : Model.t -> pgm
  = fun model ->
    let input_vars = List.map (fun i -> "I" ^ string_of_int i) (range (List.length spec_inputs)) in
    let sorted_libs = List.sort (fun (_, _, o1, _) (_, _, o2, _) -> (loc_from_model model o1) - (loc_from_model model o2)) libs in
    let updated_libs = List.map (fun (name, inputs, output, phi) ->
      let updated_inputs = List.map (fun i -> 
        match find_same_loc model (inpput_vars @ lib_outputs) i with
        | Some finded_v -> finded_v
        | None -> i
      ) inputs in
      (name, updated_inputs, output, phi)
    ) sorted_libs in
    let output_v = match find_same_loc model (inpput_vars @ lib_outputs) "O" with
      | Some finded_v -> finded_v
      | None -> "O" in
    (
      input_vars,
      updated_libs,
      output_v
    ) in

  (* Program *)
  let f_pgm = And [f_wfp; f_conn; f_lib; f_io] in
  match Solver.check_satisfiability [trans f_pgm] with
  | _, Some model -> 
    (* Model.to_string model |> print_endline; *)
    Some (model2solution model)
  | _, None -> None

