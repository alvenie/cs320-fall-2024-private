let gen_fib l k =
  let len = List.length l in
  if len = 0 || k < 0 then
    failwith "Invalid input"
  else if k < len then
    List.nth l k
  else
    let rec aux acc n =
      if n = k then
        match acc with
        | x :: _ -> x
        | [] -> failwith "Empty list"
      else
        let sum = List.fold_left (+) 0 acc in
        aux (sum :: List.take (len - 1) acc) (n + 1)
    in
    aux (List.rev l) (len - 1)