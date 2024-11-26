let test_eval_simple_add =
  let expected = VNum 5 in
  let actual = eval (Bop (Add, Num 2, Num 3)) in
  assert (expected = actual)