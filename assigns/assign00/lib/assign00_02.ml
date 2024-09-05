let is_prime x = 
  let rec check d =
    if d * d > x then true 
    else (x mod d <> 0 && check (d + 1)) 
    in x >= 2 && check 2