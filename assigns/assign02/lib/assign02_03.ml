type dir = 
| North
| South
| East
| West

type path = dir list

let dist dirs =
  let path_to_coords p =
    List.fold_left (fun (x, y) dir ->
      match dir with
      | North -> (x, y + 1)
      | South -> (x, y - 1)
      | East -> (x + 1, y)
      | West -> (x - 1, y)
    ) (0, 0) p
  in
  
  let (x, y) = path_to_coords dirs in
  
  sqrt (float_of_int (x * x + y * y))