let mk_unique_keys list =
  let rec update_or_add key value acclist =
    match acclist with
    | [] -> [(key, value)]
    | (k, v) :: rest when k = key -> (k, v + value) :: rest
    | pair :: rest -> pair :: update_or_add key value rest
  in
  List.fold_left
    (fun acclist (key, value) -> update_or_add key value acclist)
    []
    list