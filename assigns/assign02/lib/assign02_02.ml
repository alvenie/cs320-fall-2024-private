type matrix = {
entries : float list list;
rows : int;
cols : int;
}

let mk_matrix entries (r, c) =
  let rec make_rows entries acc =
    if entries = [] then
      List.rev acc
    else
      let row = List.take c entries in
      let rest = List.drop c entries in
      make_rows rest (row :: acc)
  in
  {
    entries = make_rows entries [];
    rows = r;
    cols = c;
  }