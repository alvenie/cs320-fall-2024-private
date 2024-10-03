let last_function_standing funcs start pred =
  let rec apply_until_fail f s count =
    if pred s then count
    else apply_until_fail f (f s) (count + 1)
  in
  
  let calculate_lifespan f =
    apply_until_fail f start 0
  in
  
  match funcs with
  | [] -> None
  | _ ->
      let lifespans = List.map (fun f -> (f, calculate_lifespan f)) funcs in
      let max_lifespan = List.fold_left (fun acc (_, l) -> max acc l) 0 lifespans in
      let max_funcs = List.filter (fun (_, l) -> l = max_lifespan) lifespans in
      match max_funcs with
      | [(f, _)] -> Some f
      | _ -> None