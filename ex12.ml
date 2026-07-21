type matrix = float array array

let abs_val (x : float) : float =
  if x < 0.0 then -.x else x

let copy_matrix (a : matrix) : matrix =
  Array.map Array.copy a

exception Singular_matrix

(* inverse: Gauss-Jordan elimination on the augmented matrix [A | I]
   until the left side becomes I, leaving the inverse on the right side.
   Raises Singular_matrix if A has no inverse.
   Allowed mathematical functions: fused multiply-add. *)
let inverse (a : matrix) : matrix =
  let n = Array.length a in
  (* build augmented matrix [A | I], size n x 2n *)
  let aug = Array.make_matrix n (2 * n) 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      aug.(i).(j) <- a.(i).(j)
    done;
    aug.(i).(n + i) <- 1.0
  done;

  for col = 0 to n - 1 do
    (* partial pivoting: find row with largest absolute value in this column *)
    let best_row = ref col in
    for r = col + 1 to n - 1 do
      if abs_val aug.(r).(col) > abs_val aug.(!best_row).(col) then
        best_row := r
    done;
    if abs_val aug.(!best_row).(col) < 1e-10 then
      raise Singular_matrix;

    (* swap pivot row into place *)
    let tmp = aug.(col) in
    aug.(col) <- aug.(!best_row);
    aug.(!best_row) <- tmp;

    (* normalize pivot row so pivot element becomes 1 *)
    let pivot_val = aug.(col).(col) in
    for c = 0 to 2 * n - 1 do
      aug.(col).(c) <- aug.(col).(c) /. pivot_val
    done;

    (* eliminate this column from all other rows *)
    for r = 0 to n - 1 do
      if r <> col then begin
        let factor = aug.(r).(col) in
        for c = 0 to 2 * n - 1 do
          aug.(r).(c) <- Float.fma (-. factor) aug.(col).(c) aug.(r).(c)
        done
      end
    done
  done;

  (* extract the right half as the inverse *)
  let result = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      result.(i).(j) <- aug.(i).(n + j)
    done
  done;
  result

let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e7) /. 1e7 in
  let rounded = if rounded = 0.0 then 0.0 else rounded in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let print_matrix (u : matrix) : unit =
  Array.iter (fun row ->
    let strs = Array.to_list (Array.map format_float row) in
    Printf.printf "[%s]\n" (String.concat ", " strs)
  ) u

let () =
  print_matrix (inverse
    [| [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.; 0.; 1. |] |]);
  (* expected identity *)
  print_endline "";

  print_matrix (inverse
    [| [| 2.; 0.; 0. |]; [| 0.; 2.; 0. |]; [| 0.; 0.; 2. |] |]);
  (* expected [0.5,0,0] [0,0.5,0] [0,0,0.5] *)
  print_endline "";

  print_matrix (inverse
    [| [| 8.; 5.; -2. |]; [| 4.; 7.; 20. |]; [| 7.; 6.; 1. |] |]);
  print_endline "";

  (* singular matrix example: should raise Singular_matrix *)
  print_endline "-- singular matrix test --";
  (try
    let _ = inverse [| [| 1.; 1. |]; [| 1.; 1. |] |] in
    print_endline "ERROR: should have raised Singular_matrix"
  with Singular_matrix ->
    print_endline "correctly detected: matrix is singular, no inverse exists")