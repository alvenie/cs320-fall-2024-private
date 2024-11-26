let rec fib (n : int) : int =
  if n < 0
  then -1
  else if n <= 1
  then n
  else fib (n - 1) + fib (n - 2)

let test : unit = assert (fib 7 = 13)