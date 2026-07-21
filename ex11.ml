type matrix = float array array

(* 2x2 determinant: ad - bc, computed as a single FMA *)
let det_2x2 (a : matrix) : float =
  Float.fma a.(0).(0) a.(1).(1) (-. (a.(0).(1) *. a.(1).(0)))

(* 3x3 determinant via cofactor expansion along the first row,
   reusing the 2x2 method for each minor (as the subject suggests
   "recycling the technique from lower dimensions"). *)
let det_3x3 (a : matrix) : float =
  let minor01 = [| [| a.(1).(1); a.(1).(2) |]; [| a.(2).(1); a.(2).(2) |] |] in
  let minor02 = [| [| a.(1).(0); a.(1).(2) |]; [| a.(2).(0); a.(2).(2) |] |] in
  let minor03 = [| [| a.(1).(0); a.(1).(1) |]; [| a.(2).(0); a.(2).(1) |] |] in
  let term1 = Float.fma a.(0).(0) (det_2x2 minor01) 0.0 in
  let term2 = Float.fma a.(0).(1) (det_2x2 minor02) 0.0 in
  let term3 = Float.fma a.(0).(2) (det_2x2 minor03) 0.0 in
  term1 -. term2 +. term3

(* 4x4 determinant via cofactor expansion along the first row,
   reusing the 3x3 method for each minor. *)
let det_4x4 (a : matrix) : float =
  let minor_of (skip_col : int) : matrix =
    Array.init 3 (fun i ->
      let row = a.(i + 1) in
      Array.init 3 (fun j ->
        if j < skip_col then row.(j) else row.(j + 1)))
  in
  let term0 = Float.fma a.(0).(0) (det_3x3 (minor_of 0)) 0.0 in
  let term1 = Float.fma a.(0).(1) (det_3x3 (minor_of 1)) 0.0 in
  let term2 = Float.fma a.(0).(2) (det_3x3 (minor_of 2)) 0.0 in
  let term3 = Float.fma a.(0).(3) (det_3x3 (minor_of 3)) 0.0 in
  term0 -. term1 +. term2 -. term3

(* determinant: dispatches to the method for the matrix's dimension (n <= 4) *)
let determinant (a : matrix) : float =
  let n = Array.length a in
  match n with
  | 1 -> a.(0).(0)
  | 2 -> det_2x2 a
  | 3 -> det_3x3 a
  | 4 -> det_4x4 a
  | _ -> 0.0 (* undefined behaviour for n > 4, per subject *)

let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e7) /. 1e7 in
  let rounded = if rounded = 0.0 then 0.0 else rounded in (* normalize -0.0 to 0.0 *)
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let () =
  Printf.printf "%s\n" (format_float (determinant [| [| 1.; -1. |]; [| -1.; 1. |] |]));
  (* expected 0.0 *)

  Printf.printf "%s\n" (format_float (determinant
    [| [| 2.; 0.; 0. |]; [| 0.; 2.; 0. |]; [| 0.; 0.; 2. |] |]));
  (* expected 8.0 *)

  Printf.printf "%s\n" (format_float (determinant
    [| [| 8.; 5.; -2. |]; [| 4.; 7.; 20. |]; [| 7.; 6.; 1. |] |]));
  (* expected -174.0 *)

  Printf.printf "%s\n" (format_float (determinant
    [| [| 8.; 5.; -2.; 4. |];
       [| 4.; 2.5; 20.; 4. |];
       [| 8.; 5.; 1.; 4. |];
       [| 28.; -4.; 17.; 1. |] |]))
  (* expected 1032.0 *)