type tree = 
  | Leaf of int
  | Node of tree list

let rec collapse h t =
  if h <= 0 then
    failwith "Non positive height"
  else if h = 1 then
    collect_terminals t
  else
    match t with
    | Leaf _ -> t
    | Node children ->
        Node (List.map (collapse (h - 1)) children)

and collect_terminals t =
  match t with
  | Leaf _ -> t
  | Node children ->
      Node (List.concat_map collect_terminal_list children)

and collect_terminal_list t =
  match t with
  | Leaf _ -> [t]
  | Node [] -> [t]
  | Node children -> List.concat_map collect_terminal_list children