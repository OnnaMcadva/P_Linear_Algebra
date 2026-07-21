type vector = float array

(* dot product: sum of u.(i) * v.(i), accumulated with FMA *)
let dot (u : vector) (v : vector) : float =
  let n = Array.length u in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := Float.fma u.(i) v.(i) !acc
  done;
  !acc

let format_float (x : float) : string =
  if Float.is_integer x then Printf.sprintf "%.1f" x
  else Printf.sprintf "%g" x

let () =
  print_endline "-- dot --";
  Printf.printf "%s\n" (format_float (dot [| 0.; 0. |] [| 1.; 1. |]));
  (* expected 0.0 *)

  Printf.printf "%s\n" (format_float (dot [| 1.; 1. |] [| 1.; 1. |]));
  (* expected 2.0 *)

  Printf.printf "%s\n" (format_float (dot [| -1.; 6. |] [| 3.; 2. |]))
  (* expected 9.0 *)