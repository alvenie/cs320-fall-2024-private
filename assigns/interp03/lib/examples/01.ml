(* basic unit literal test *)
let _ =
  let u : unit = () in
  assert (u = ())

(* basic int literal test *)
let _ =
  let n : int = -12320 in
  assert (n = (-12320))

(* basic float literal test *)
let _ =
  let n : float = 023.0324 in
  assert (n = 23.0324)

(* basic bool literal test *)
let _ =
  let b = true in
  let b : bool = false in
  assert true

(* basic list literal test *)
let _ =
  let l = [] in
  let l = [1;2;3] in
  let l : bool list = true :: false :: true :: [] in
  assert (l = true :: false :: true :: [])

(* basic option literal test *)
let _ =
  let op = None in
  let op : int option = None in
  let op : bool option = Some true in
  assert (op = Some true)

(* basic pair literal test *)
let _ =
  let p = (1, 2) in
  let p : unit * int = ((), -20) in
  assert (p = ((), -20))

(* basic fun literal test *)
let _ =
  let id = fun x -> x in
  let id : int -> int = fun x -> x in
  let id : unit -> unit = fun (x : unit) -> x in
  let k = fun x y -> x in
  assert (k 2 true = 2)

(* basic type variable test *)
let f (x : int) : 'a = x
let g (x : 'a) : int = x
let _ = assert (f 20 = g 20)

(* basic add test *)
let _ =
  let sum = 30 + 35 in
  assert (sum = 65)

(* basic sub test *)
let _ =
  let diff = 30 - 35 in
  assert (diff = (-5))

(* basic mul test *)
let _ =
  let mul = 30 * 2 in
  assert (mul = 60)

(* basic div test *)
let _ =
  let div : int = 30 / 2 in
  assert (div = 15)

(* basic mod test *)
let _ =
  let m : int = 30 mod 7 in
  assert (m = 2)

(* basic add float test *)
let _ =
  let sum : float = 2. +. 3. in
  assert (sum = 5.)

