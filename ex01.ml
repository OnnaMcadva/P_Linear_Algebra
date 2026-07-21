type vector = float array

let linear_combination (u : vector array) (coefs : float array) : vector =
  let k = Array.length u in
  let dim = if k = 0 then 0 else Array.length u.(0) in
  let result = Array.make dim 0.0 in
  for i = 0 to dim - 1 do
    let acc = ref 0.0 in
    for j = 0 to k - 1 do
      acc := Float.fma coefs.(j) u.(j).(i) !acc
    done;
    result.(i) <- !acc
  done;
  result

let format_float (x : float) : string =
  if Float.is_integer x then Printf.sprintf "%.1f" x
  else Printf.sprintf "%g" x

let print_vector (u : vector) : unit =
  Array.iter (fun x -> Printf.printf "[%s]\n" (format_float x)) u

let () =
  let e1 = [| 1.; 0.; 0. |] in
  let e2 = [| 0.; 1.; 0. |] in
  let e3 = [| 0.; 0.; 1. |] in

  print_endline "-- linear_combination (e1, e2, e3) --";
  print_vector (linear_combination [| e1; e2; e3 |] [| 10.; -2.; 0.5 |]);

  let v1 = [| 1.; 2.; 3. |] in
  let v2 = [| 0.; 10.; -100. |] in

  print_endline "-- linear_combination (v1, v2) --";
  print_vector (linear_combination [| v1; v2 |] [| 10.; -2. |])
