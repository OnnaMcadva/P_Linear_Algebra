type vector = float array

(* cross product for 3-dimensional vectors only.
   Formula: (u2*v3 - u3*v2, u3*v1 - u1*v3, u1*v2 - u2*v1)
   Each component computed with a single FMA: a*b - c*d = fma(a, b, -(c*d))
   but simplest and clearest here is fma(a, b, -c*d) done as fma(u2, v3, -.(u3 *. v2)) *)
let cross_product (u : vector) (v : vector) : vector =
  let result = Array.make 3 0.0 in
  result.(0) <- Float.fma u.(1) v.(2) (-. (u.(2) *. v.(1)));
  result.(1) <- Float.fma u.(2) v.(0) (-. (u.(0) *. v.(2)));
  result.(2) <- Float.fma u.(0) v.(1) (-. (u.(1) *. v.(0)));
  result

let format_float (x : float) : string =
  if Float.is_integer x then Printf.sprintf "%.1f" x
  else Printf.sprintf "%g" x

let print_vector (u : vector) : unit =
  Array.iter (fun x -> Printf.printf "[%s]\n" (format_float x)) u

let () =
  print_vector (cross_product [| 0.; 0.; 1. |] [| 1.; 0.; 0. |]);
  (* expected [0.0] [1.0] [0.0] *)
  print_endline "";

  print_vector (cross_product [| 1.; 2.; 3. |] [| 4.; 5.; 6. |]);
  (* expected [-3.0] [6.0] [-3.0] *)
  print_endline "";

  print_vector (cross_product [| 4.; 2.; -3. |] [| -2.; -5.; 16. |])
  (* expected [17.0] [-58.0] [-16.0] *)