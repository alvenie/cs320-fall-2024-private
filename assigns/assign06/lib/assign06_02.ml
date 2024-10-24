open Utils

let parse toks =
  let rec parse_helper tokens stack =
    match tokens with
    | [] -> 
      (match stack with
       | [result] -> Some result
       | _ -> None)
    | TNum n :: rest ->
      parse_helper rest (Num n :: stack)
    | TAdd :: rest ->
      (match stack with
       | e2 :: e1 :: tail -> parse_helper rest (Add (e1, e2) :: tail)
       | _ -> None)
    | TLt :: rest ->
      (match stack with
       | e2 :: e1 :: tail -> parse_helper rest (Lt (e1, e2) :: tail)
       | _ -> None)
    | TIte :: rest ->
      (match stack with
       | e3 :: e2 :: e1 :: tail -> parse_helper rest (Ite (e1, e2, e3) :: tail)
       | _ -> None)
  in
  parse_helper toks []