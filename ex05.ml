type vector = float array

(* reused from ex03 *)
let dot (u : vector) (v : vector) : float =
  let n = Array.length u in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := Float.fma u.(i) v.(i) !acc
  done;
  !acc

(* reused from ex04 - norm_2, uses pow which was allowed in that exercise *)
let norm (v : vector) : float =
  let n = Array.length v in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := Float.fma v.(i) v.(i) !acc
  done;
  Float.pow !acc 0.5

(* angle_cos: cos(u, v) = <u|v> / (||u|| * ||v||) *)
let angle_cos (u : vector) (v : vector) : float =
  (dot u v) /. (norm u *. norm v)

(* Tolerant formatting: rounds to a fixed precision first, so tiny
   floating-point rounding errors (eg. from pow-based sqrt) don't
   prevent a value that is "morally" an integer from printing as one. *)
let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e9) /. 1e9 in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let () =
  let u = [| 1.; 0. |] in
  let v = [| 1.; 0. |] in
  Printf.printf "%s\n" (format_float (angle_cos u v));
  (* expected 1.0 *)

  let u = [| 1.; 0. |] in
  let v = [| 0.; 1. |] in
  Printf.printf "%s\n" (format_float (angle_cos u v));
  (* expected 0.0 *)

  let u = [| -1.; 1. |] in
  let v = [| 1.; -1. |] in
  Printf.printf "%s\n" (format_float (angle_cos u v));
  (* expected -1.0 *)

  let u = [| 2.; 1. |] in
  let v = [| 4.; 2. |] in
  Printf.printf "%s\n" (format_float (angle_cos u v));
  (* expected 1.0 *)

  let u = [| 1.; 2.; 3. |] in
  let v = [| 4.; 5.; 6. |] in
  Printf.printf "%s\n" (format_float (angle_cos u v))
  (* expected 0.974631846 *)