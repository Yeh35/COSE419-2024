open Utils
open Hw3



(* tests *)
let p1 : string * lib list * spec = ("f(x, y) = x + y", 
  [
    ("add", ["i11"; "i12"], "o1", EQ (VAR "o1", ADD (VAR "i11", VAR "i12")));
  ], 
  ([2; 3], 5)
)

let p2 : string * lib list * spec = ("f(x) = x^8", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("mul", ["i21"; "i22"], "o2", EQ (VAR "o2", MUL (VAR "i21", VAR "i22")));
    ("mul", ["i31"; "i32"], "o3", EQ (VAR "o3", MUL (VAR "i31", VAR "i32")));
  ], 
  ([2], 256)
)

let p3 : string * lib list * spec = ("f(a,b,c,h) = a * b^h + b * h + c", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("mul", ["i21"; "i22"], "o2", EQ (VAR "o2", MUL (VAR "i21", VAR "i22")));
    ("add", ["i31"; "i32"], "o3", EQ (VAR "o3", ADD (VAR "i31", VAR "i32")));
    ("add", ["i41"; "i42"], "o4", EQ (VAR "o4", ADD (VAR "i41", VAR "i42")));
  ], 
  ([1; 5; 3; 4], 648)
)

let p4 : string * lib list * spec = ("f(x) = x^31", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("mul", ["i21"; "i22"], "o2", EQ (VAR "o2", MUL (VAR "i21", VAR "i22")));
    ("mul", ["i31"; "i32"], "o3", EQ (VAR "o3", MUL (VAR "i31", VAR "i32")));
    ("mul", ["i41"; "i42"], "o4", EQ (VAR "o4", MUL (VAR "i41", VAR "i42")));
    ("mul", ["i51"; "i52"], "o5", EQ (VAR "o5", MUL (VAR "i51", VAR "i52")));
    ("mul", ["i61"; "i62"], "o6", EQ (VAR "o6", MUL (VAR "i61", VAR "i62")));
    ("mul", ["i71"; "i72"], "o7", EQ (VAR "o7", MUL (VAR "i71", VAR "i72")));
  ], 
  ([2], 2147483648)
)

let p5 : string * lib list * spec = ("f(x) = (2*(x-1))^2", 
  [
    ("mul", ["i11"; "i12"], "o1", EQ (VAR "o1", MUL (VAR "i11", VAR "i12")));
    ("sub1", ["i2"], "o2", EQ (ADD (VAR "o2", INT 1), VAR "i2"));
    ("mul2", ["i3"], "o3", EQ (VAR "o3", MUL (VAR "i3", INT 2)));
  ], 
  ([5], 64)
) 

let pgm1 : pgm * spec = (
  (
    ["x"],
    [
      ("add", ["x"; "x"], "y", EQ (VAR "y", ADD (VAR "x", VAR "x")));
      ("mul", ["y"; "y"], "z", EQ (VAR "z", MUL (VAR "y", VAR "y")))
    ],
    "z"
  ),
  ([3], 36)
)

let test () = 
  List.iter (fun (idx, (desc, libs, spec)) ->
    print_endline (string_of_int (idx + 1) ^ ": " ^ desc);
    match synthesize libs spec with 
    | Some pgm -> 
      print_endline (string_of_pgm pgm); 
      print_endline ("Verification result: " ^ string_of_bool (verify pgm spec));
      print_endline ("-----\n")
    | None -> 
      print_endline "failed to synthesize";
      print_endline ("-----\n");
  ) (enumerate [p1; p2; p3; p4; p5;])

let _ = test ()

let test_4_verify() =
  print_endline "verify test: ";
  List.iter (fun (idx, (pgm, spec)) ->
    print_endline ("pgm" ^ string_of_int (idx + 1));
    print_endline ("Verification result: " ^ string_of_bool (verify pgm spec));
    print_endline ("-----\n");
  ) (enumerate [pgm1;])


let _ = test_4_verify ()

(*
I () = 0)
I0 () = 2)
I1 () = 3)
i11 () = 5)
i12 () = 0)
o1 () = 5)
O () = 5)
l_I () = 0)
l_O () = 1)
l_o1 () = 1)
l_i11 () = 1)
l_i12 () = 0)
*)

(*
I0 =  2)
I1 =  3)
i11 =  3)
i12 =  2)
o1 =  5)
O =  5)
l_I1 =  (- 1))
l_I0 =  0)
l_i11 =  (- 1))
l_i12 =  0)
l_o1 =  1)
l_O =  1)
*)

(*
I0 = 2)
i11 = 16)
i12 = 16)
i21 = 2)
i22 = 2)
i31 = 4)
i32 = 4)
o1 = 256)
o2 = 4)
o3 = 16)

l_I0 = 0)
O = 256)
l_O = 3)
l_o1 = 3)
l_o2 = 1)
l_i12 = 2)
l_o3 = 2)
l_i22 = 0)
l_i21 = 0)
l_i11 = 2)
l_i32 = 1)
l_i31 = 1)
*)