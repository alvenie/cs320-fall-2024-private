type piece = 
| X
| O

type pos = 
| Piece of piece
| Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
| Top
| Middle
| Bottom

type col_index = 
| Left
| Middle
| Right

type pos_index = row_index * col_index

let get_pos (board: board) ((row, col): pos_index) : pos =
  let (top, middle, bottom) = board in
  match row with
  | Top ->
    (match col with
    | Left -> let (l, _, _) = middle in l
    | Middle -> let (_, m, _) = top in m
    | Right -> let (_, _, r) = top in r)
| Middle -> 
   (match col with
    | Left -> let (l, _, _) = middle in l
    | Middle -> let (_, m, _) = middle in m
    | Right -> let (_, _, r) = middle in r)
| Bottom -> 
   (match col with
    | Left -> let (l, _, _) = bottom in l
    | Middle -> let (_, m, _) = bottom in m
    | Right -> let (_, _, r) = bottom in r)

let winner (board: board) : bool =
  let check_line (p1: pos_index) (p2: pos_index) (p3: pos_index) : bool =
    match get_pos board p1, get_pos board p2, get_pos board p3 with
    | Piece X, Piece X, Piece X -> true
    | Piece O, Piece O, Piece O -> true
    | _ -> false
  in
    
  let winning_lines = [
    (* Rows *)
    ((Top, Left), (Top, Middle), (Top, Right));
    ((Middle, Left), (Middle, Middle), (Middle, Right));
    ((Bottom, Left), (Bottom, Middle), (Bottom, Right));
        
    (* Columns *)
    ((Top, Left), (Middle, Left), (Bottom, Left));
    ((Top, Middle), (Middle, Middle), (Bottom, Middle));
    ((Top, Right), (Middle, Right), (Bottom, Right));
        
    (* Diagonals *)
    ((Top, Left), (Middle, Middle), (Bottom, Right));
    ((Top, Right), (Middle, Middle), (Bottom, Left))
  ] in
    
  let rec check_lines = function
    | [] -> false
    | (p1, p2, p3) :: rest -> check_line p1 p2 p3 || check_lines rest
  in
  
  check_lines winning_lines