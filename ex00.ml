type vector = float array

type matrix = float array array

let vec_add (u : vector) (v : vector) : unit =
  let n = Array.length u in
  for i = 0 to n - 1 do
    u.(i) <- u.(i) +. v.(i)
  done

let vec_sub (u : vector) (v : vector) : unit =
  let n = Array.length u in
  for i = 0 to n - 1 do
    u.(i) <- u.(i) -. v.(i)
  done

let vec_scl (u : vector) (a : float) : unit =
  let n = Array.length u in
  for i = 0 to n - 1 do
    u.(i) <- u.(i) *. a
  done

let mat_add (u : matrix) (v : matrix) : unit =
  let rows = Array.length u in
  for i = 0 to rows - 1 do
    let cols = Array.length u.(i) in
    for j = 0 to cols - 1 do
      u.(i).(j) <- u.(i).(j) +. v.(i).(j)
    done
  done

let mat_sub (u : matrix) (v : matrix) : unit =
  let rows = Array.length u in
  for i = 0 to rows - 1 do
    let cols = Array.length u.(i) in
    for j = 0 to cols - 1 do
      u.(i).(j) <- u.(i).(j) -. v.(i).(j)
    done
  done

let mat_scl (u : matrix) (a : float) : unit =
  let rows = Array.length u in
  for i = 0 to rows - 1 do
    let cols = Array.length u.(i) in
    for j = 0 to cols - 1 do
      u.(i).(j) <- u.(i).(j) *. a
    done
  done

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
  print_endline "-- vec_add --";
  let u = [| 2.; 3. |] in
  let v = [| 5.; 7. |] in
  vec_add u v;
  print_vector u;

  print_endline "-- vec_sub --";
  let u = [| 2.; 3. |] in
  let v = [| 5.; 7. |] in
  vec_sub u v;
  print_vector u;

  print_endline "-- vec_scl --";
  let u = [| 2.; 3. |] in
  vec_scl u 2.;
  print_vector u;

  print_endline "-- mat_add --";
  let u = [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  let v = [| [| 7.; 4. |]; [| -2.; 2. |] |] in
  mat_add u v;
  print_matrix u;

  print_endline "-- mat_sub --";
  let u = [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  let v = [| [| 7.; 4. |]; [| -2.; 2. |] |] in
  mat_sub u v;
  print_matrix u;

  print_endline "-- mat_scl --";
  let u = [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  mat_scl u 2.;
  print_matrix u