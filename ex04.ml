type vector = float array

(* absolute value without using any library function - just a comparison *)
let abs_val (x : float) : float =
  if x < 0.0 then -.x else x

(* 1-norm (Manhattan norm): sum of absolute values *)
let norm_1 (v : vector) : float =
  let n = Array.length v in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := Float.fma (abs_val v.(i)) 1.0 !acc
  done;
  !acc

(* 2-norm (Euclidean norm): square root of sum of squares.
   sqrt is not an allowed function, so we use pow(x, 0.5) instead. *)
let norm (v : vector) : float =
  let n = Array.length v in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := Float.fma v.(i) v.(i) !acc
  done;
  Float.pow !acc 0.5

(* infinity norm (supremum norm): maximum absolute value *)
let norm_inf (v : vector) : float =
  let n = Array.length v in
  let result = ref 0.0 in
  for i = 0 to n - 1 do
    result := Float.max !result (abs_val v.(i))
  done;
  !result

let format_float (x : float) : string =
  if Float.is_integer x then Printf.sprintf "%.1f" x
  else Printf.sprintf "%g" x

let () =
  let u = [| 0.; 0.; 0. |] in
  Printf.printf "%s, %s, %s\n"
    (format_float (norm_1 u)) (format_float (norm u)) (format_float (norm_inf u));
  (* expected 0.0, 0.0, 0.0 *)

  let u = [| 1.; 2.; 3. |] in
  Printf.printf "%s, %s, %s\n"
    (format_float (norm_1 u)) (format_float (norm u)) (format_float (norm_inf u));
  (* expected 6.0, 3.74165738, 3.0 *)

  let u = [| -1.; -2. |] in
  Printf.printf "%s, %s, %s\n"
    (format_float (norm_1 u)) (format_float (norm u)) (format_float (norm_inf u))
  (* expected 3.0, 2.236067977, 2.0 *)