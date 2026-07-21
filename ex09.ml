type matrix = float array array

(* transpose: A (m x n) -> B (n x m), where B.(j).(i) = A.(i).(j)
   Allowed mathematical functions: None -> only assignment, no arithmetic needed here *)
let transpose (a : matrix) : matrix =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let result = Array.make_matrix n m 0.0 in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      result.(j).(i) <- a.(i).(j)
    done
  done;
  result

let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e9) /. 1e9 in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let print_matrix (u : matrix) : unit =
  Array.iter (fun row ->
    let strs = Array.to_list (Array.map format_float row) in
    Printf.printf "[%s]\n" (String.concat ", " strs)
  ) u

let () =
  print_matrix (transpose [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |] |])
  (* input is 2x3, expected output is 3x2:
     [1.0, 4.0]
     [2.0, 5.0]
     [3.0, 6.0] *)