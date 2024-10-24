open Utils

let lex s =
    let tokens = split s in
    let rec convert_tokens token_list acc =
      match token_list with
      | [] -> Some (List.rev acc)
      | Some tok :: rest -> convert_tokens rest (tok :: acc)
      | None :: _ -> None
    in
    let token_options = List.map tok_of_string_opt tokens in
    convert_tokens token_options []