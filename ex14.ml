type matrix = float array array

let pi = 3.14159265358979323846

(* projection: builds a 4x4 perspective projection matrix.
   fov is expected in degrees, converted to radians before use.
   Allowed mathematical functions: tan, fused multiply-add. *)
let projection (fov : float) (ratio : float) (near : float) (far : float) : matrix =
  let fov_rad = fov *. pi /. 180.0 in
  let f = 1.0 /. tan (fov_rad /. 2.0) in
  (* Standard row-major math layout of the matrix (row i, col j): *)
  [|
    [| f /. ratio; 0.0; 0.0;                                0.0 |];
    [| 0.0;        f;   0.0;                                0.0 |];
    [| 0.0;        0.0; far /. (near -. far);
       Float.fma far near (0.0) /. (near -. far) |];
    [| 0.0;        0.0; -1.0;                               0.0 |];
  |]

(* Print a matrix in column-major order: each line is one column,
   listing that column's row0..row3 values. This is the format
   expected by the provided display tool. *)
let print_column_major (m : matrix) : unit =
  let n = Array.length m in
  for col = 0 to n - 1 do
    let values = Array.init n (fun row -> m.(row).(col)) in
    let strs = Array.to_list (Array.map (Printf.sprintf "%g") values) in
    Printf.printf "%s\n" (String.concat ", " strs)
  done

let () =
  let fov = 90.0 in
  let ratio = 16.0 /. 9.0 in
  let near = 0.1 in
  let far = 100.0 in
  let proj = projection fov ratio near far in
  print_column_major proj