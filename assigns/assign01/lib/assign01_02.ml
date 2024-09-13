let is_prime x = 
  let rec check d =
    if d * d > x then true 
    else (x mod d <> 0 && check (d + 1)) 
    in x >= 2 && check 2

let nth_prime n =
  let rec find_p c p =
    if n = c then p - 1
    else if is_prime p then find_p (c + 1) (p + 1)
    else find_p c (p + 1)
  in find_p (-1) 2