open Assign01_01
open Assign01_02
open Assign01_03

let nth_prime n =
  let rec find_p c p =
    if n = c then p - 1
    else if is_prime p then find_p (c + 1) (p + 1)
    else find_p c (p + 1)
  in find_p (0) 2

let decode_s n =
  let rec decode n i =
    if n = 1 then []
    else
      let p = nth_prime i in
      let count = count_p n p in
      if count = 0 then decode n (i + 1)
      else (p, count) :: decode (n / (pow p count)) (i + 1)
    in decode n 1

let to_string n = 
  let sequence = decode_s n in
  let sorted = List.sort (fun (_, c1) (_, c2) -> compare c2 c1) sequence in
  let counts = List.map snd sorted in
  "[" ^ (String.concat "; " (List.map string_of_int counts)) ^ "]"