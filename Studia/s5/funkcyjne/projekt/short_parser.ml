let algebraic_piece piece = 
  match piece.piece with
  | King -> "K"
  | Queen -> "Q"
  | Rook -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | Pawn -> ""
  | _ -> failwith "not recognized piece"
;;

let rec loop_with_check (position : fen_position) (filtered : (int * int) list) ((sx, sy) : (int * int)) ((dx, dy) : (int * int)) same_x same_y any = 
  match filtered with
  | [] -> (!same_x, !same_y, !any)
  | current_pair :: rest -> 
    let moves_for_current = gen_moves_for_all position [current_pair] in
    if List.exists (fun (first_pair, second_pair) -> second_pair = (dx, dy)) moves_for_current then begin
      let new_same_y = !same_y || (List.exists (fun (first, _) -> first = sx) [current_pair]) in
      let new_same_x = !same_x || (List.exists (fun (_, second) -> second = sy) [current_pair]) in
      let new_any = !any || true in 
      loop_with_check position rest  (sx, sy) (dx, dy) (ref new_same_x) (ref new_same_y) (ref new_any) 
    end else
      loop_with_check position rest  (sx, sy) (dx, dy) same_x same_y any
;;

let get_ambigious (position : fen_position) (piece : square) (x, y) (dx, dy) = 
  let all_same_pieces = find_all_pieces position.board piece in
  let filtered = List.filter (fun (a, b) -> (a, b) <> (x, y)) all_same_pieces in
  let (same_x, same_y, any) = loop_with_check position filtered (x, y) (dx, dy) (ref false) (ref false) (ref false) in
  let notation = first_fun (change_to_notation1 [(x, y)]) in
  let file = if same_y then String.make 1 (String.get notation 0) else "" in
  let rank = if same_x then String.make 1 (String.get notation 1) else "" in
  let file = if ((not same_y) && (not same_x) && any) then String.make 1 (String.get notation 0) else file in
  file ^ rank
;; 

let algebraic_move (position : fen_position) (square : square) ((starty, startx), (desty, destx)) =
  if square.piece = King && abs(startx - destx) > 1 then
    if destx < 4 then "O-O-O" else "O-O"
  else
    let end_square = get_square_from_pos position.board (desty, destx) in
    let dest = first_fun(change_to_notation1 [(desty, destx)]) in 
    let temp_board = make_move position.board (starty, startx) (desty, destx) in
    let temp_fen = {board = temp_board; turn = position.turn; castling = position.castling; en_passant = position.en_passant; halfmove_clock = position.halfmove_clock; fullmove_number = position.fullmove_number} in
    let checked = is_checked temp_fen position.turn in
    let all_enemy = find_all_pieces_of_color temp_board (Some (find_opposing_color (Some position.turn))) in 
    let all_enemy_moves = gen_moves_for_all temp_fen all_enemy in 
    let piece = algebraic_piece square in
    let taken = if end_square.piece <> Empty then
      if square.piece = Pawn then
        String.make 1 (String.get (first_fun(change_to_notation1 [(starty, startx)])) 0) ^ "x" else "x" 
      else "" in
    let ambigous = get_ambigious position square (starty, startx) (desty, destx) in
    if (List.length all_enemy_moves) = 0 then 
      piece ^ ambigous ^ taken ^ dest ^ "#"
    else if checked <> false then
      piece ^ ambigous ^ taken ^ dest ^ "+"
    else
      piece ^ ambigous ^ taken ^ dest   
;;  

let change_to_short position list_of_moves =
  List.map (fun (from, to_) -> algebraic_move position (get_square_from_pos position.board from) (from, to_)) list_of_moves
;; 

let remove (s : string) (char_list : char list) : string =
  let is_char_to_remove c = List.mem c char_list in
  let filtered = String.concat "" (List.map (String.make 1) (List.filter (fun c -> not (is_char_to_remove c)) (String.to_seq s |> List.of_seq))) in 
  if filtered = "" then "-" else filtered
;; 

let position_after_move (position : fen_position) (fromy, fromx) (toy, tox) = 
  let square = get_square_from_pos position.board (fromy, fromx) in
  let dest_square = get_square_from_pos position.board (toy, tox) in
  let en_passant = if toy - fromy > 0 then (toy - 1, tox) else (toy + 1, tox) in  
  let old_fen_board = make_move position.board (fromy, fromx) (toy, tox) in 
  let old_fen_turn = (find_opposing_color (Some position.turn)) in
  let old_fen_castling = if square.piece = King then
    if square.color = (Some White) then (remove position.castling ['K'; 'Q']) else (remove position.castling ['k'; 'q'])
    else if square.piece = Rook then 
      if fromx > 4 then if square.color = (Some White) then (remove position.castling ['K']) else (remove position.castling ['k']) 
      else if square.color = (Some White) then (remove position.castling ['Q']) else (remove position.castling ['q'])
    else position.castling in
  let old_fen_en_passant = if square.piece = Pawn && abs(fromy - toy) > 1 then first_fun(change_to_notation1 [en_passant]) else "-" in
  let old_fen_halfmove_clock = if (square.piece = Pawn || dest_square.piece <> Empty) then 0 else position.halfmove_clock + 1 in
  let old_fen_fullmove_number = if position.turn = White then position.fullmove_number else position.fullmove_number + 1 in
  {board = old_fen_board; turn = old_fen_turn; castling = old_fen_castling; en_passant = old_fen_en_passant; halfmove_clock = old_fen_halfmove_clock; fullmove_number = old_fen_fullmove_number}
;;

let is_move_legal (position : fen_position) (short : string) = 
  let all_pieces = find_all_pieces_of_color position.board (Some position.turn) in 
  let all_moves = gen_moves_for_all position all_pieces in 
  let all_moves_short = change_to_short position all_moves in  
  List.exists (fun elem -> elem = short) all_moves_short
;;

let index_of e l = 
  let rec index_rec i = function
    | [] -> raise Not_found
    | hd::tl -> if hd = e then i else index_rec (i+1) tl
  in
  index_rec 0 l
;;

let find_pos_in_list (position : fen_position) (short : string) =
  let all_pieces = find_all_pieces_of_color position.board (Some position.turn) in 
  let all_moves = gen_moves_for_all position all_pieces in 
  let all_moves_short = change_to_short position all_moves in  
  index_of short all_moves_short
;;

let find_move (position : fen_position) (short : string) = 
  let all_pieces = find_all_pieces_of_color position.board (Some position.turn) in 
  let all_moves = gen_moves_for_all position all_pieces in 
  let all_moves_short = change_to_short position all_moves in  
  let id = index_of short all_moves_short in 
  List.nth all_moves id
;; 
  