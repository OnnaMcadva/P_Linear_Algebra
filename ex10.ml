type matrix = float array array

let abs_val (x : float) : float =
  if x < 0.0 then -.x else x

(* Deep copy so the original input matrix is never mutated *)
let copy_matrix (a : matrix) : matrix =
  Array.map Array.copy a

(* row_echelon: Gauss-Jordan elimination with partial pivoting,
   producing the reduced row-echelon form.
   Allowed mathematical functions: None -> only +, -, *, /, and comparisons. *)
let row_echelon (a : matrix) : matrix =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let mat = copy_matrix a in
  let pivot_row = ref 0 in
  let col = ref 0 in
  while !pivot_row < m && !col < n do
    (* find the row with the largest absolute value in this column, at or below pivot_row *)
    let best_row = ref !pivot_row in
    for r = !pivot_row + 1 to m - 1 do
      if abs_val mat.(r).(!col) > abs_val mat.(!best_row).(!col) then
        best_row := r
    done;
    if abs_val mat.(!best_row).(!col) < 1e-10 then
      (* column is all zeros (numerically) below pivot_row: move to next column *)
      col := !col + 1
    else begin
      (* swap best_row into pivot_row position *)
      let tmp = mat.(!pivot_row) in
      mat.(!pivot_row) <- mat.(!best_row);
      mat.(!best_row) <- tmp;

      (* normalize pivot row so the pivot element becomes 1 *)
      let pivot_val = mat.(!pivot_row).(!col) in
      for c = 0 to n - 1 do
        mat.(!pivot_row).(c) <- mat.(!pivot_row).(c) /. pivot_val
      done;

      (* eliminate this column in all other rows (above and below) *)
      for r = 0 to m - 1 do
        if r <> !pivot_row then begin
          let factor = mat.(r).(!col) in
          for c = 0 to n - 1 do
            mat.(r).(c) <- mat.(r).(c) -. factor *. mat.(!pivot_row).(c)
          done
        end
      done;

      pivot_row := !pivot_row + 1;
      col := !col + 1
    end
  done;
  mat

let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e7) /. 1e7 in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let print_matrix (u : matrix) : unit =
  Array.iter (fun row ->
    let strs = Array.to_list (Array.map format_float row) in
    Printf.printf "[%s]\n" (String.concat ", " strs)
  ) u

let () =
  print_matrix (row_echelon
    [| [| 1.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.; 0.; 1. |] |]);
  (* expected identity matrix unchanged *)
  print_endline "";

  print_matrix (row_echelon [| [| 1.; 2. |]; [| 3.; 4. |] |]);
  (* expected [1.0, 0.0] [0.0, 1.0] *)
  print_endline "";

  print_matrix (row_echelon [| [| 1.; 2. |]; [| 2.; 4. |] |]);
  (* expected [1.0, 2.0] [0.0, 0.0] *)
  print_endline "";

  print_matrix (row_echelon
    [| [| 8.; 5.; -2.; 4.; 28. |];
       [| 4.; 2.5; 20.; 4.; -4. |];
       [| 8.; 5.; 1.; 4.; 17. |] |])
  (* expected [1.0, 0.625, 0.0, 0.0, -12.1666667]
              [0.0, 0.0, 1.0, 0.0, -3.6666667]
              [0.0, 0.0, 0.0, 1.0, 29.5] *)