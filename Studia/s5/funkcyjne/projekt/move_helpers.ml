let find_piece_from_position (board : board) (find : square) ((start_rank, start_row) : (int*int))=
  let rec find_row (rank : square list) (curr_row : int) (rank_index : int) (row_index : int) = 
    if curr_row >= row_index then
      match rank with
      | [] -> None
      | square :: rest -> 
        if square.piece = find.piece && square.color = find.color then 
          Some (rank_index, curr_row)
        else
          find_row rest (curr_row+1) rank_index (row_index+1)
    else 
      match rank with
      | [] -> failwith "row missed"
      | square :: rest -> 
      find_row rest (curr_row+1) rank_index row_index
  in
  let rec find_rank (board : board) (curr_rank : int) (rank_index : int) (row_index : int) = 
    if curr_rank >= rank_index then
      match board with 
      | [] -> None
      | rank :: rest ->
        match find_row rank 0 rank_index row_index with
        | Some pos -> Some pos
        | None -> find_rank rest (curr_rank+1) (rank_index+1) 0
    else
      match board with 
      | [] -> failwith "rank missed"
      | rank :: rest ->
        find_rank rest (curr_rank+1) rank_index row_index
  in
  find_rank board 0 start_rank start_row
;;

let get_square_from_pos (board : board) ((rank_id, row_id) : (int*int)) = 
  let rec find_row (rank : square list) (rank_index : int) (row_index : int)  ((rank_id, row_id) : (int*int)) = 
    match rank with
    | [] -> failwith "There is no such row"
    | square :: rest -> 
      if rank_id = rank_index && row_id = row_index then 
        square
      else
        find_row rest rank_index (row_index+1) (rank_id, row_id)
  in
  let rec find_rank (board : board) (rank_index : int) ((rank_id, row_id) : (int*int))= 
    match board with 
    | [] -> failwith "There is no such rank"
    | rank :: rest ->
      if rank_id = rank_index then 
        find_row rank rank_index 0 (rank_id, row_id) 
      else
        find_rank rest (rank_index+1) (rank_id, row_id)
  in
  find_rank board 0 (rank_id, row_id)
;;

let is_in_bounds ((new_rank, new_row) : (int*int)) =
  new_rank >= 0 && new_rank < 8 && new_row >= 0 && new_row < 8
;;

let is_free (board : board) ((new_rank, new_row) : (int*int)) =
  if is_in_bounds (new_rank, new_row) then
    let square = get_square_from_pos board (new_rank, new_row) in
      match square.piece with
      | Empty -> true
      | _     -> false
  else
    failwith "Cant stand on square which is not on board :D"
;;

let is_free_with_take (board : board) ((new_rank, new_row) : (int*int)) piece = 
  if is_in_bounds (new_rank, new_row) then
    let square = get_square_from_pos board (new_rank, new_row) in
      match square.piece with
      | Empty -> true
      | _     -> if square.color <> piece.color then true  else false
  else
    failwith "Cant stand on square which is not on board :D"
;;

let change_to_notation1 (pairs : (int * int) list) : string list =
  let notation_of_pair (rank, row) = 
    String.make 1 (Char.chr (row + Char.code 'a')) ^ string_of_int(8 - rank)
  in 
  List.map notation_of_pair pairs
;;

let change_to_notation2 (pairs : ((int * int) * (int * int)) list) : (string * string) list =
  let notation_of_pair ((rank, file), (original_rank, original_file)) =
    let rank_str = String.make 1 (Char.chr (file + Char.code 'a')) in
    let original_rank_str = String.make 1 (Char.chr (original_file + Char.code 'a')) in
    (rank_str ^ string_of_int (8 - rank), original_rank_str ^ string_of_int (8 - original_rank))
  in
  List.map notation_of_pair pairs
;;

let file_to_int file_char =
  Char.code (Char.lowercase_ascii file_char) - Char.code 'a'
;;

let rank_to_int rank_char =
  8 - (Char.code rank_char - Char.code '0')
;;

let change_from_notation1 (coordinates : string list) =
  List.map (fun coord ->
    match String.length coord with
    | 2 ->
      let file_char = String.get coord 0 in
      let rank_char = String.get coord 1 in
      (rank_to_int rank_char, file_to_int file_char)
    | _ -> failwith "Invalid coordinate format"
  ) coordinates
;;

let change_from_notation2 (coordinates : (string * string) list) : ((int * int) * (int * int)) list =
  let coordinate_from_pair (coord, original_coord) =
    match (String.length coord, String.length original_coord) with
    | (2, 2) ->
      let file_char = String.get coord 0 in
      let rank_char = String.get coord 1 in
      let original_file_char = String.get original_coord 0 in
      let original_rank_char = String.get original_coord 1 in
      ((rank_to_int rank_char, file_to_int file_char), (rank_to_int original_rank_char, file_to_int original_file_char))
    | _ -> failwith "Invalid coordinate format"
  in
  List.map coordinate_from_pair coordinates
;;

let find_all_pieces (board : board) piece =
  let pieces = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let square = get_square_from_pos board (i, j) in
      if square.piece = piece.piece && square.color = piece.color then
        pieces := (i, j) :: !pieces
    done;
  done;
  !pieces
;;

let find_all_pieces_of_color (board : board) color =
  let pieces = ref [] in
  for i = 0 to 7 do
    for j = 0 to 7 do
      let square = get_square_from_pos board (i, j) in
      if square.color = color then
        pieces := (i, j) :: !pieces
    done;
  done;
  !pieces
;;

let first_fun move_functions = 
  match move_functions with
  | first :: _ -> first
  | [] -> failwith "No functions in the list"
;;

let second_fun move_functions = 
  match move_functions with
  | _ :: second :: _ -> second
  | _ -> failwith "Not enough functions in the list"
;;

let find_opposing_color color = 
  match color with
  | Some White -> Black
  | Some Black -> White
  | None -> failwith "No such color"
;;

let make_move (board : board) (from : int * int) (to_ : int * int) : board =
  let copy_board = List.map List.rev (List.map List.rev board) in
  let (from_rank, from_file) = from in
  let (to_rank, to_file) = to_ in
  let piece_to_move = get_square_from_pos board from in
  let updated_board =
    List.mapi
      (fun i row ->
        List.mapi
          (fun j square ->
            if i = from_rank && j = from_file then
              { piece = Empty; color = None }
            else if i = to_rank && j = to_file then
              { piece = piece_to_move.piece; color = piece_to_move.color }
            else
              square)
          row)
      copy_board
  in
  if piece_to_move.piece = King then
    let is_castling_move =
      abs (to_file - from_file) = 2 && from_rank = to_rank
    in
    if is_castling_move then
      let rook_from_file, rook_to_file =
        if to_file > from_file then
          (7, 5) (* Kingside castling, adjust rook on h1 to g1 *)
        else
          (0, 3) (* Queenside castling, adjust rook on a1 to d1 *)
      in
      let rook_piece = get_square_from_pos board (from_rank, rook_from_file) in
      List.mapi
        (fun i row ->
          List.mapi
            (fun j square ->
              if i = from_rank && j = rook_from_file then
                { piece = Empty; color = None }
              else if i = to_rank && j = rook_to_file then
                { piece = rook_piece.piece; color = rook_piece.color }
              else
                square)
            row)
        updated_board
    else
      updated_board
  else if piece_to_move.piece = Pawn then 
    let (desty, destx) = to_ in 
    if (desty = 0 && piece_to_move.color = (Some White)) || (desty = 7 && piece_to_move.color = (Some Black)) then
      List.mapi
      (fun i row ->
        List.mapi
          (fun j square ->
            if i = desty && j = destx then
              { piece = Queen; color = piece_to_move.color}
            else
              square)
          row)
      updated_board
    else 
      updated_board
  else
    updated_board
  |> List.map List.rev
  |> List.map List.rev
;;