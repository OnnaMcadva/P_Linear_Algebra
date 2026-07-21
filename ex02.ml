type vector = float array
type matrix = float array array

(* lerp for scalars: u + t * (v - u), computed with a single FMA *)
let lerp_scalar (u : float) (v : float) (t : float) : float =
  Float.fma t (v -. u) u

(* lerp for vectors: applies scalar lerp element-wise *)
let lerp_vector (u : vector) (v : vector) (t : float) : vector =
  let n = Array.length u in
  let result = Array.make n 0.0 in
  for i = 0 to n - 1 do
    result.(i) <- lerp_scalar u.(i) v.(i) t
  done;
  result

(* lerp for matrices: applies scalar lerp element-wise *)
let lerp_matrix (u : matrix) (v : matrix) (t : float) : matrix =
  let rows = Array.length u in
  let result = Array.make rows [||] in
  for i = 0 to rows - 1 do
    let cols = Array.length u.(i) in
    let row = Array.make cols 0.0 in
    for j = 0 to cols - 1 do
      row.(j) <- lerp_scalar u.(i).(j) v.(i).(j) t
    done;
    result.(i) <- row
  done;
  result

let format_float (x : float) : string =
  if Float.is_integer x then Printf.sprintf "%.1f" x
  else Printf.sprintf "%g" x

let print_vector (u : vector) : unit =
  Array.iter (fun x -> Printf.printf "[%s]\n" (format_float x)) u

let print_matrix (u : matrix) : unit =
  Array.iter (fun row ->
    let strs = Array.to_list (Array.map format_float row) in
    Printf.printf "[%s]\n" (String.concat ", " strs)
  ) u

let () =
  print_endline "-- lerp_scalar --";
  Printf.printf "%s\n" (format_float (lerp_scalar 0. 1. 0.));
  (* expected 0.0 *)
  Printf.printf "%s\n" (format_float (lerp_scalar 0. 1. 1.));
  (* expected 1.0 *)
  Printf.printf "%s\n" (format_float (lerp_scalar 0. 1. 0.5));
  (* expected 0.5 *)
  Printf.printf "%s\n" (format_float (lerp_scalar 21. 42. 0.3));
  (* expected 27.3 *)

  print_endline "-- lerp_vector --";
  print_vector (lerp_vector [| 2.; 1. |] [| 4.; 2. |] 0.3);
  (* expected [2.6] [1.3] *)

  print_endline "-- lerp_matrix --";
  print_matrix (lerp_matrix
    [| [| 2.; 1. |]; [| 3.; 4. |] |]
    [| [| 20.; 10. |]; [| 30.; 40. |] |]
    0.5)
  (* expected [11.0, 5.5] [16.5, 22.0] *)