let gen_king_moves (position : fen_position) ((rank, row) : (int*int)) =
  let board = position.board in
  let king =  get_square_from_pos board (rank, row) in 
  let possible = [
    (rank - 1, row - 1);
    (rank - 1, row    );
    (rank - 1, row + 1);
    (rank,     row - 1);
    (rank,     row + 1);
    (rank + 1, row - 1);
    (rank + 1, row    );
    (rank + 1, row + 1);
  ]
  in
  let in_bounds = List.filter is_in_bounds possible in 
  let filtered = List.filter (fun pair -> is_free_with_take board pair king) in_bounds in
  List.map (fun original_pair -> ((rank, row), original_pair)) filtered 
;;

let gen_bishop_moves (position : fen_position) ((file, rank) : (int*int)) =
    let board = position.board in
    let bishop = get_square_from_pos board (file, rank) in
    let org_pair = (file, rank) in  
    let moves = ref [] in

    let break = ref false in

    for i = 1 to 7 do       
      if not !break && is_in_bounds (file - i, rank - i) then
        if is_free board (file - i, rank - i) then
          moves := (org_pair, (file - i, rank - i)) :: !moves
        else
          let occupied = get_square_from_pos board (file - i, rank - i) in
          if occupied.color <> bishop.color then begin
            moves := (org_pair, (file - i, rank - i)) :: !moves;
            break := true
          end else
            break := true
      else
        break := true
    done;
    
    break := false;
    for i = 1 to 7 do
      if not !break && is_in_bounds (file + i, rank + i) then
        if is_free board (file + i, rank + i) then
          moves := (org_pair, (file + i, rank + i)) :: !moves
        else
          let occupied = get_square_from_pos board (file + i, rank + i) in
          if occupied.color <> bishop.color then begin
            moves := (org_pair, (file + i, rank + i)) :: !moves;
            break := true
          end else
            break := true
      else
        break := true
    done;

    break := false;
    for i = 1 to 7 do
      if not !break && is_in_bounds (file + i, rank - i) then
        if is_free board (file + i, rank - i) then
          moves := (org_pair, (file + i, rank - i)) :: !moves
        else
          let occupied = get_square_from_pos board (file + i, rank - i) in
          if occupied.color <> bishop.color then begin
            moves := (org_pair, (file + i, rank - i)) :: !moves;
            break := true
          end else
            break := true
      else
        break := true
    done;

    break := false;
    for i = 1 to 7 do
      if not !break && is_in_bounds (file - i, rank + i) then
        if is_free board (file - i, rank + i) then
          moves := (org_pair, (file - i, rank + i)) :: !moves
        else
          let occupied = get_square_from_pos board (file - i, rank + i) in
          if occupied.color <> bishop.color then begin
            moves := (org_pair, (file - i, rank + i)) :: !moves;
            break := true
          end else
            break := true
      else
        break := true
    done;

    !moves
;;

let gen_rook_moves (position : fen_position) ((file, rank) : (int*int)) =
  let board = position.board in
    let rook = get_square_from_pos board (file, rank) in 
    let moves = ref [] in

    let break = ref false in

    break := false;
    for i = 1 to 7 do
      if not !break && is_in_bounds (file - i, rank) then
        if is_free board (file - i, rank) then
          moves := ((file, rank), (file - i, rank)) :: !moves
        else
          let occupied = get_square_from_pos board (file - i, rank) in
          if occupied.color <> rook.color then begin
            moves := ((file, rank), (file - i, rank)) :: !moves;
            break := true;
          end else
            break := true;
      else
        break := true;
    done;
     
    break := false;
    for i = 1 to 7 do       
      if not !break && is_in_bounds (file, rank - i) then
        if is_free board (file, rank - i) then
          moves := ((file, rank), (file, rank - i)) :: !moves
        else
          let occupied = get_square_from_pos board (file, rank - i) in
          if occupied.color <> rook.color then begin
            moves := ((file, rank), (file, rank - i)) :: !moves;
            break := true;
          end else
            break := true;
        else
        break := true;
    done;

    break := false;
    for i = 1 to 7 do
      if not !break && is_in_bounds (file, rank + i) then
        if is_free board (file, rank + i) then
          moves := ((file, rank), (file, rank + i)) :: !moves
        else
          let occupied = get_square_from_pos board(file, rank + i) in
          if occupied.color <> rook.color then begin
            moves := ((file, rank), (file, rank + i)) :: !moves;
            break := true;
          end else
            break := true;
      else
        break := true;
    done;

    break := false;
    for i = 1 to 7 do
      if not !break && is_in_bounds (file + i, rank) then
        if is_free board (file + i, rank) then
          moves := ((file, rank), (file + i, rank)) :: !moves
        else
          let occupied = get_square_from_pos board (file + i, rank) in
          if occupied.color <> rook.color then begin
            moves := ((file, rank), (file + i, rank)) :: !moves;
            break := true;
          end else
            break := true;
      else
        break := true;
    done;

    !moves
;;

let gen_knight_moves (position : fen_position) ((rank, row) : (int*int)) =
  let board = position.board in
  let knight = get_square_from_pos board (rank, row) in 
  let possible = [
      (rank + 1, row + 2);
      (rank + 2, row + 1);
      (rank + 2, row - 1);
      (rank + 1, row - 2);
      (rank - 1, row - 2);
      (rank - 2, row - 1);
      (rank - 2, row + 1);
      (rank - 1, row + 2);
    ]
    in
    let in_bounds = List.filter is_in_bounds possible in 
    let filtered = List.filter (fun pair -> is_free_with_take board pair knight) in_bounds in 
    List.map (fun original_pair -> ((rank, row), original_pair)) filtered
;;

let gen_pawn_moves (position : fen_position) ((rank, file) : (int*int)) = 
  let board = position.board in 
  let pawn = get_square_from_pos board (rank, file) in
    let forward_one =
      if pawn.color = Some White && is_in_bounds (rank - 1, file) && is_free board (rank - 1, file)
        then [((rank, file), (rank - 1, file))]
      else if pawn.color = Some Black && is_in_bounds (rank + 1, file) && is_free board (rank + 1, file)
        then [((rank, file), (rank + 1, file))]
      else []
    in
    let forward_two =
      if pawn.color = Some White && rank == 6 && is_free board (rank - 1, file) && is_free board (rank - 2, file)
        then [((rank, file), (rank - 2, file))] 
      else if pawn.color = Some Black && rank == 1 && is_free board (rank + 1, file) && is_free board (rank + 2, file)
        then [((rank, file), (rank + 2, file))]
      else []
    in
    let capture_left =
      if pawn.color = Some White && is_in_bounds (rank - 1, file - 1) && not (is_free board (rank - 1, file - 1)) && is_free_with_take board (rank - 1, file - 1) pawn
        then [((rank, file), (rank - 1, file - 1))]
      else if pawn.color = Some Black && is_in_bounds (rank + 1, file - 1) && not (is_free board (rank + 1, file - 1)) && is_free_with_take board (rank + 1, file - 1) pawn
        then [((rank, file), (rank + 1, file - 1))]
      else []
    in
    let capture_right =
      if pawn.color = Some White && is_in_bounds (rank - 1, file + 1) && not (is_free board (rank - 1, file + 1)) && is_free_with_take board (rank - 1, file + 1) pawn
        then [((rank, file), (rank - 1, file + 1))]
      else if pawn.color = Some Black && is_in_bounds (rank + 1, file + 1) && not (is_free board (rank + 1, file + 1)) && is_free_with_take board (rank + 1, file + 1) pawn
        then [((rank, file), (rank + 1, file + 1))]
      else []
    in
    let en_passant = 
      if pawn.color = Some White && is_in_bounds (rank - 1, file + 1) && change_to_notation1 [(rank - 1, file + 1)] = [position.en_passant]
        then  [((rank, file), (rank - 1, file + 1))]
      else if pawn.color = Some White && is_in_bounds (rank - 1, file - 1) && change_to_notation1 [(rank - 1, file - 1)] = [position.en_passant]
        then  [((rank, file), (rank - 1, file - 1))] 
      else if pawn.color = Some Black && is_in_bounds (rank + 1, file - 1) && change_to_notation1 [(rank + 1, file - 1)] = [position.en_passant]
        then  [((rank, file), (rank + 1, file - 1))] 
      else if pawn.color = Some Black && is_in_bounds (rank + 1, file + 1) && change_to_notation1 [(rank + 1, file + 1)] = [position.en_passant]
        then  [((rank, file), (rank + 1, file + 1))]
      else []  
    in 
    forward_one @ forward_two @ capture_left @ capture_right @ en_passant
;; 
  
let find_generating_function piece = 
  match piece.piece with 
  | King -> [gen_king_moves]
  | Bishop -> [gen_bishop_moves]
  | Knight -> [gen_knight_moves]
  | Rook -> [gen_rook_moves]
  | Pawn -> [gen_pawn_moves]
  | Queen -> [gen_bishop_moves; gen_rook_moves]
  | Empty -> failwith "Invalid piece"
;;

let rec gen_attacked_for_all (position : fen_position) pieces_pos =
  match pieces_pos with
  | [] -> []
  | piece :: rest ->
    let list_fun = (find_generating_function (get_square_from_pos position.board piece)) in
    if (List.length list_fun) > 1 then
      let moves1 = (first_fun list_fun) position piece in
      let moves2 = (second_fun list_fun) position piece in
      moves1 @ moves2 @ gen_attacked_for_all position rest
    else
      let moves = (first_fun list_fun) position piece in 
      moves @ gen_attacked_for_all position rest
;;

let is_checked (position : fen_position) (opp_col : color) : bool =
  let good_col = find_opposing_color (Some opp_col) in 
  let king_pos = find_piece_from_position position.board { piece = King; color = (Some good_col) } (0, 0) in
  let all_enemy = find_all_pieces_of_color position.board (Some opp_col) in 
  let all_attacked = gen_attacked_for_all position all_enemy in 

  match king_pos with
  | Some king_pos ->
    List.exists (fun (_, to_) -> to_ = king_pos) all_attacked
  | None -> false
;;

let remove_attacked (position : fen_position) (good_so_far : ((int*int)*(int*int)) list) piece = 
  let opp_col = find_opposing_color piece.color in 
  
  let is_move_safe (from, to_) =
    let board_after_move = make_move position.board from to_ in
    let new_position = { position with board = board_after_move } in
    not (is_checked new_position  opp_col)
  in

  List.filter is_move_safe good_so_far
;;

let castle_short position king (rank, row) =
  if  king.color = Some White && String.contains position.castling 'K' && rank = 7 && row = 4 && 
      is_free position.board (rank, row + 1) && is_free position.board (rank, row + 2) &&
      not (is_checked position (find_opposing_color king.color)) &&
      not (is_checked {position with board = (make_move position.board (7, 4) (7, 5))} (find_opposing_color king.color)) && 
      not (is_checked {position with board = (make_move position.board (7, 4) (7, 6))} (find_opposing_color king.color))
  then [((7, 4), (7, 6))]
  else if king.color = Some Black && String.contains position.castling 'k' && rank = 0 && row = 4 && 
      is_free position.board (rank, row + 1) && is_free position.board (rank, row + 2) &&
      not (is_checked position (find_opposing_color king.color)) &&
      not (is_checked {position with board = (make_move position.board (0, 4) (0, 5))} (find_opposing_color king.color)) && 
      not (is_checked {position with board = (make_move position.board (0, 4) (0, 6))} (find_opposing_color king.color))
  then [((0, 4), (0, 6))]
  else []
;;

let castle_long position king (rank, row) =
  if  king.color = Some White && String.contains position.castling 'Q' && rank = 7 && row = 4 && 
      is_free position.board (rank, row - 1) && is_free position.board (rank, row - 2) && is_free position.board (rank, row - 3) &&
      not (is_checked position (find_opposing_color king.color)) &&
      not (is_checked {position with board = (make_move position.board (7, 4) (7, 3))} (find_opposing_color king.color)) && 
      not (is_checked {position with board = (make_move position.board (7, 4) (7, 2))} (find_opposing_color king.color))
  then [((7, 4), (7, 2))]
  else if king.color = Some Black && String.contains position.castling 'q' && rank = 0 && row = 4 && 
      is_free position.board (rank, row - 1) && is_free position.board (rank, row - 2) && is_free position.board (rank, row - 3) &&
      not (is_checked position (find_opposing_color king.color)) &&
      not (is_checked {position with board = (make_move position.board (0, 4) (0, 3))} (find_opposing_color king.color)) && 
      not (is_checked {position with board = (make_move position.board (0, 4) (0, 2))} (find_opposing_color king.color))
  then [((0, 4), (0, 2))]
  else []
;;

let rec gen_moves_for_all (position : fen_position) pieces_pos =
  match pieces_pos with
  | [] -> []
  | piece :: rest ->
    let list_fun = (find_generating_function (get_square_from_pos position.board piece)) in
    if (List.length list_fun) > 1 then
      let figure = get_square_from_pos position.board piece in 
      let moves1 = (first_fun list_fun) position piece in
      let moves2 = (second_fun list_fun) position piece in
      (remove_attacked position moves1 figure) @  (remove_attacked position moves2 figure) @ gen_moves_for_all position rest
    else
      let moves = (first_fun list_fun) position piece in
      let figure = get_square_from_pos position.board piece in
      if figure.piece <> King then    
        (remove_attacked position moves figure) @ gen_moves_for_all position rest
      else
        (remove_attacked position moves figure) @ (castle_short position figure piece) @ (castle_long position figure piece) @ gen_moves_for_all position rest
;;
      