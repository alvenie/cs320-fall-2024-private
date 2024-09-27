let group l =
  let rec aux acclist current_group = function
    | [] -> 
        if current_group = [] then Some (List.rev acclist)
        else Some (List.rev (List.rev current_group :: acclist))
    | 0 :: rest ->
        if current_group = [] then None
        else 
          (match rest with
          | x :: _ when (x > 0) <> (match current_group with y :: _ -> y > 0 | [] -> false) ->
               aux (List.rev current_group :: acclist) [] rest
          | _ -> None)
    | x :: rest ->
        match current_group with
        | [] -> aux acclist [x] rest
        | y :: _ when (x > 0) = (y > 0) -> aux acclist (x :: current_group) rest
        | _ -> None
  in
  match l with
  | [] -> Some []
  | [_] -> None
  | 0 :: _ -> None
  | x :: rest -> aux [] [x] rest