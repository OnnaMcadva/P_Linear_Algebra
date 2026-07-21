(* Complex numbers implemented manually as (re, im) pairs *)
type complex = { re : float; im : float }

let c_add (a : complex) (b : complex) : complex =
  { re = a.re +. b.re; im = a.im +. b.im }

let c_sub (a : complex) (b : complex) : complex =
  { re = a.re -. b.re; im = a.im -. b.im }

let c_mul (a : complex) (b : complex) : complex =
  { re = a.re *. b.re -. a.im *. b.im;
    im = a.re *. b.im +. a.im *. b.re }

let c_scale (a : complex) (k : float) : complex =
  { re = a.re *. k; im = a.im *. k }

let c_conj (a : complex) : complex =
  { re = a.re; im = -. a.im }

let c_zero = { re = 0.0; im = 0.0 }

type cvector = complex array
type cmatrix = complex array array

(* --- ex00 style: add, sub, scl --- *)
let cvec_add (u : cvector) (v : cvector) : cvector =
  Array.init (Array.length u) (fun i -> c_add u.(i) v.(i))

let cvec_sub (u : cvector) (v : cvector) : cvector =
  Array.init (Array.length u) (fun i -> c_sub u.(i) v.(i))

let cvec_scl (u : cvector) (k : float) : cvector =
  Array.map (fun x -> c_scale x k) u

(* --- ex01 style: linear combination --- *)
let clinear_combination (u : cvector array) (coefs : float array) : cvector =
  let k = Array.length u in
  let dim = if k = 0 then 0 else Array.length u.(0) in
  Array.init dim (fun i ->
    let acc = ref c_zero in
    for j = 0 to k - 1 do
      acc := c_add !acc (c_scale u.(j).(i) coefs.(j))
    done;
    !acc)

(* --- ex02 style: linear interpolation --- *)
let clerp (u : cvector) (v : cvector) (t : float) : cvector =
  Array.init (Array.length u) (fun i ->
    c_add u.(i) (c_scale (c_sub v.(i) u.(i)) t))

(* --- ex03 style: dot product (Hermitian inner product: sum u_i * conj(v_i)) --- *)
let cdot (u : cvector) (v : cvector) : complex =
  let n = Array.length u in
  let acc = ref c_zero in
  for i = 0 to n - 1 do
    acc := c_add !acc (c_mul u.(i) (c_conj v.(i)))
  done;
  !acc

(* --- ex04 style: 2-norm (modulus of the Hermitian norm, always real) --- *)
let cnorm (u : cvector) : float =
  let self_dot = cdot u u in
  (* self_dot.im should be ~0 for a valid Hermitian inner product *)
  Float.pow self_dot.re 0.5

(* --- ex07 style: matrix-vector and matrix-matrix multiplication --- *)
let cmul_vec (a : cmatrix) (u : cvector) : cvector =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  Array.init m (fun i ->
    let acc = ref c_zero in
    for j = 0 to n - 1 do
      acc := c_add !acc (c_mul a.(i).(j) u.(j))
    done;
    !acc)

let cmul_mat (a : cmatrix) (b : cmatrix) : cmatrix =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  let p = if n = 0 then 0 else Array.length b.(0) in
  Array.init m (fun i ->
    Array.init p (fun k ->
      let acc = ref c_zero in
      for j = 0 to n - 1 do
        acc := c_add !acc (c_mul a.(i).(j) b.(j).(k))
      done;
      !acc))

(* --- ex08 style: trace --- *)
let ctrace (a : cmatrix) : complex =
  let n = Array.length a in
  let acc = ref c_zero in
  for i = 0 to n - 1 do
    acc := c_add !acc a.(i).(i)
  done;
  !acc

(* --- ex09 style: conjugate transpose (adjoint), the complex analogue of transpose --- *)
let cconjugate_transpose (a : cmatrix) : cmatrix =
  let m = Array.length a in
  let n = if m = 0 then 0 else Array.length a.(0) in
  Array.init n (fun j -> Array.init m (fun i -> c_conj a.(i).(j)))

(* --- ex11 style: 2x2 determinant --- *)
let cdet_2x2 (a : cmatrix) : complex =
  c_sub (c_mul a.(0).(0) a.(1).(1)) (c_mul a.(0).(1) a.(1).(0))

(* --- printing helpers --- *)
let format_float (x : float) : string =
  let rounded = Float.round (x *. 1e7) /. 1e7 in
  let rounded = if rounded = 0.0 then 0.0 else rounded in
  if Float.is_integer rounded then Printf.sprintf "%.1f" rounded
  else Printf.sprintf "%g" rounded

let format_complex (c : complex) : string =
  if c.im >= 0.0 then
    Printf.sprintf "%s+%si" (format_float c.re) (format_float c.im)
  else
    Printf.sprintf "%s-%si" (format_float c.re) (format_float (-. c.im))

let print_cvector (u : cvector) : unit =
  Array.iter (fun x -> Printf.printf "[%s]\n" (format_complex x)) u

let print_cmatrix (u : cmatrix) : unit =
  Array.iter (fun row ->
    let strs = Array.to_list (Array.map format_complex row) in
    Printf.printf "[%s]\n" (String.concat ", " strs)
  ) u

let () =
  let i = { re = 0.0; im = 1.0 } in
  let one = { re = 1.0; im = 0.0 } in

  print_endline "-- cvec_add: (1+2i, 3-1i) + (2+0i, 0+1i) --";
  print_cvector (cvec_add
    [| { re = 1.; im = 2. }; { re = 3.; im = -1. } |]
    [| { re = 2.; im = 0. }; { re = 0.; im = 1. } |]);

  print_endline "-- cvec_scl: (1+1i, 2-2i) * 2.0 --";
  print_cvector (cvec_scl [| { re = 1.; im = 1. }; { re = 2.; im = -2. } |] 2.0);

  print_endline "-- cdot: <(1+0i, i), (1+0i, i)> --";
  Printf.printf "%s\n" (format_complex (cdot [| one; i |] [| one; i |]));
  (* self dot product of a unit-modulus complex vector should be real: 2+0i *)

  print_endline "-- cnorm: ||(1+0i, i)|| --";
  Printf.printf "%s\n" (format_float (cnorm [| one; i |]));
  (* expected sqrt(2) approx 1.41421 *)

  print_endline "-- cmul_vec: [[i, 0],[0, i]] * (1+0i, 1+0i) --";
  print_cvector (cmul_vec [| [| i; c_zero |]; [| c_zero; i |] |] [| one; one |]);
  (* expected [0+1i] [0+1i] *)

  print_endline "-- ctrace: [[1+1i, 0],[0, 2-1i]] --";
  Printf.printf "%s\n" (format_complex (ctrace [| [| { re=1.;im=1. }; c_zero |]; [| c_zero; { re=2.;im=(-1.) } |] |]));
  (* expected 3+0i *)

  print_endline "-- cconjugate_transpose: [[1+2i, 3-1i],[0+0i, 2+0i]] --";
  print_cmatrix (cconjugate_transpose
    [| [| { re=1.;im=2. }; { re=3.;im=(-1.) } |]; [| c_zero; { re=2.;im=0. } |] |]);

  print_endline "-- cdet_2x2: [[i, 1],[1, i]] --";
  Printf.printf "%s\n" (format_complex (cdet_2x2 [| [| i; one |]; [| one; i |] |]))
  (* det = i*i - 1*1 = -1 - 1 = -2+0i *)