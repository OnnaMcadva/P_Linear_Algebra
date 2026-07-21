type vector = float array
type matrix = float array array

(* mul_vec: A (m x n) times u (n) = result (m)
   time O(n*m), space O(n*m) as required *)
let mul_vec (a : matrix) (u : vector) : vector =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let result = Array.make m 0.0 in
  for i = 0 to m - 1 do
    let acc = ref 0.0 in
    for j = 0 to n - 1 do
      acc := Float.fma a.(i).(j) u.(j) !acc
    done;
    result.(i) <- !acc
  done;
  result

(* mul_mat: A (m x n) times B (n x p) = result (m x p)
   time O(n*m*p), space O(n*m + m*p + n*p) as required *)
let mul_mat (a : matrix) (b : matrix) : matrix =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let p = if n = 0 then 0 else Array.length b.(0) in
  let result = Array.make_matrix m p 0.0 in
  for i = 0 to m - 1 do
    for k = 0 to p - 1 do
      let acc = ref 0.0 in
      for j = 0 to n - 1 do
        acc := Float.fma a.(i).(j) b.(j).(k) !acc
      done;
      result.(i).(k) <- !acc
    done
  done;
  result

let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e9) /. 1e9 in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let print_vector (u : vector) : unit =
  Array.iter (fun x -> Printf.printf "[%s]\n" (format_float x)) u

let print_matrix (u : matrix) : unit =
  Array.iter (fun row ->
    let strs = Array.to_list (Array.map format_float row) in
    Printf.printf "[%s]\n" (String.concat ", " strs)
  ) u

let () =
  print_endline "-- mul_vec --";
  print_vector (mul_vec [| [| 1.; 0. |]; [| 0.; 1. |] |] [| 4.; 2. |]);
  (* expected [4.0] [2.0] *)
  print_endline "";

  print_vector (mul_vec [| [| 2.; 0. |]; [| 0.; 2. |] |] [| 4.; 2. |]);
  (* expected [8.0] [4.0] *)
  print_endline "";

  print_vector (mul_vec [| [| 2.; -2. |]; [| -2.; 2. |] |] [| 4.; 2. |]);
  (* expected [4.0] [-4.0] *)
  print_endline "";

  print_endline "-- mul_mat --";
  print_matrix (mul_mat [| [| 1.; 0. |]; [| 0.; 1. |] |] [| [| 1.; 0. |]; [| 0.; 1. |] |]);
  (* expected [1.0, 0.0] [0.0, 1.0] *)
  print_endline "";

  print_matrix (mul_mat [| [| 1.; 0. |]; [| 0.; 1. |] |] [| [| 2.; 1. |]; [| 4.; 2. |] |]);
  (* expected [2.0, 1.0] [4.0, 2.0] *)
  print_endline "";

  print_matrix (mul_mat [| [| 3.; -5. |]; [| 6.; 8. |] |] [| [| 2.; 1. |]; [| 4.; 2. |] |])
  (* expected [-14.0, -7.0] [44.0, 22.0] *)