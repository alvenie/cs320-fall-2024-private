open Assign01_02

let rec count_p n p =
  if n mod p <> 0 then 0
  else 1 + count_p (n/p) p

let nth l n = 
  count_p l (nth_prime n)
