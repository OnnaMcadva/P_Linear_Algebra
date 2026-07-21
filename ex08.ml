type matrix = float array array

(* trace: sum of diagonal elements A.(i).(i)
   Allowed mathematical functions: None -> plain addition only, no FMA here *)
let trace (a : matrix) : float =
  let n = Array.length a in
  let acc = ref 0.0 in
  for i = 0 to n - 1 do
    acc := !acc +. a.(i).(i)
  done;
  !acc

let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e9) /. 1e9 in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let () =
  Printf.printf "%s\n" (format_float (trace [| [| 1.; 0. |]; [| 0.; 1. |] |]));
  (* expected 2.0 *)

  Printf.printf "%s\n" (format_float (trace
    [| [| 2.; -5.; 0. |]; [| 4.; 3.; 7. |]; [| -2.; 3.; 4. |] |]));
  (* expected 9.0 *)

  Printf.printf "%s\n" (format_float (trace
    [| [| -2.; -8.; 4. |]; [| 1.; -23.; 4. |]; [| 0.; 6.; 4. |] |]))
  (* expected -21.0 *)