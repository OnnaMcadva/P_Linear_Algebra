type matrix = float array array

let abs_val (x : float) : float =
  if x < 0.0 then -.x else x

let copy_matrix (a : matrix) : matrix =
  Array.map Array.copy a

(* rank: forward Gaussian elimination with partial pivoting,
   then count the number of non-zero (pivot) rows produced.
   Allowed mathematical functions: None -> only +, -, *, /, and comparisons. *)
let rank (a : matrix) : int =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let mat = copy_matrix a in
  let pivot_row = ref 0 in
  let col = ref 0 in
  while !pivot_row < m && !col < n do
    let best_row = ref !pivot_row in
    for r = !pivot_row + 1 to m - 1 do
      if abs_val mat.(r).(!col) > abs_val mat.(!best_row).(!col) then
        best_row := r
    done;
    if abs_val mat.(!best_row).(!col) < 1e-10 then
      col := !col + 1
    else begin
      let tmp = mat.(!pivot_row) in
      mat.(!pivot_row) <- mat.(!best_row);
      mat.(!best_row) <- tmp;

      (* eliminate this column in all rows below the pivot *)
      for r = !pivot_row + 1 to m - 1 do
        let factor = mat.(r).(!col) /. mat.(!pivot_row).(!col) in
        for c = !col to n - 1 do
          mat.(r).(c) <- mat.(r).(c) -. factor *. mat.(!pivot_row).(c)
        done
      done;

      pivot_row := !pivot_row + 1;
      col := !col + 1
    end
  done;
  !pivot_row

let () =
  Printf.printf "%d\n" (rank
    [| [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.; 0.; 1. |] |]);
  (* expected 3 *)

  Printf.printf "%d\n" (rank
    [| [| 1.; 2.; 0.; 0. |]; [| 2.; 4.; 0.; 0. |]; [| -1.; 2.; 1.; 1. |] |]);
  (* expected 2 *)

  Printf.printf "%d\n" (rank
    [| [| 8.; 5.; -2. |]; [| 4.; 7.; 20. |]; [| 7.; 6.; 1. |]; [| 21.; 18.; 7. |] |])
  (* expected 3 *)