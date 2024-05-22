(* Parser test *)
(* let fen_input = "rnb1kb1r/pppp1ppp/5n2/4p2Q/4PN2/q1P5/PP1P1PPP/RNB1KB1R w KQkq - 0 1"*) (* RANDOM *)
(* let fen_input = "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 2";; *) (* FRANCUSKI *)
(* let fen_board = "rnb1k2r/pppP1ppp/5n2/2B1p2Q/4PN2/2P3P1/1P3P2/RNq1KB1R";; *)
(* let fen_input = fen_board ^ " w KQkq - 0 1";; *)
(* let fen_board = "8/4pk2/6Q1/3pP3/8/8/8/4K3";; *)
(* let fen_input = fen_board ^ " w - - 0 2" Check for king moving into attack *)
(* let fen_input = "rnbqkbnr/ppp2ppp/4p3/3p4/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq d6 0 3" *)
let fen_board = "r3kb1r/pppp1ppp/5n2/2B1p2Q/4PN2/q1P3P1/1P1P1P2/RN2KB1R";;
let fen_input = fen_board ^ " w KQkq - 0 1";;  (* MORE MOVES *)
let fen_position = parse_fen fen_input;;
print_endline "Chessboard state:";
print_chessboard fen_position.board;;

(* Find test *)
let position = parse_fen fen_input;;

(* Test pos -> FEN *)
assert (fen_board = fen_of_board position.board);;

let black_king = {piece = King; color = Some Black};;
let white_king = {piece = King; color = Some White};;
let white_king_pos = find_piece_from_position position.board white_king (0,0);;
assert (white_king_pos = Some (7,4)) ;;

let black_knight = {piece = Knight; color = Some Black};;

let black_knight_pos = find_piece_from_position position.board black_knight (0,2);;
assert (black_knight_pos = Some (2,5)) ;;
let black_knight_pos = find_piece_from_position position.board black_knight (2,6);;
assert (black_knight_pos = None) ;;

(* Get square test*)
let test_square = get_square_from_pos position.board (4,4);;
assert (test_square = {piece = Pawn; color = Some White});;

(* Test bounds*)
let test_boundT = is_in_bounds (4, 4);;
let test_boundF = is_in_bounds (4, 9);;
assert (test_boundF = false);;
assert (test_boundT = true);;

(* Test bounds*)
let posT = is_free position.board (4,6);;
let posF = is_free position.board (4,5);;
assert (posF = false);;
assert (posT = true);;

(* Test find all pieces *)
let bishop = {piece = Bishop; color = Some White};;
let all_bishops = find_all_pieces position.board bishop;;
let all_bishops_not = change_to_notation1 all_bishops;;
assert (all_bishops_not = ["f1"; "c5"]);;

let rook = {piece = Rook; color = Some White};;
let all_rooks = find_all_pieces position.board rook;;
let all_rooks_not = change_to_notation1 all_rooks;;
assert (all_rooks_not = ["h1"; "a1"]);;

let queen = {piece = Queen; color = Some Black};;
let all_queens = find_all_pieces position.board queen;;
let all_queens_not = change_to_notation1 all_queens;;
assert (all_queens_not = ["a3"]);;

let knight = {piece = Knight; color = Some White};;
let all_knights = find_all_pieces position.board knight;;
let all_knights_not = change_to_notation1 all_knights;;
assert (all_knights_not = ["b1"; "f4"]);;

let pawn = {piece = Pawn; color = Some White};;
let all_pawns = find_all_pieces position.board pawn;;
let all_pawns_not = change_to_notation1 all_pawns;;
assert (all_pawns_not = ["f2"; "d2"; "b2"; "g3"; "c3"; "e4"]);;

(* Test gen moves*)
let king_possible = 
  let temp = gen_king_moves position (7,4) in 
  change_to_notation2 temp;;
assert (king_possible = [("e1", "e2"); ("e1", "d1")]);;

let all_bishop_moves = gen_moves_for_all position all_bishops;;
let all_bishop_moves_not = change_to_notation2 all_bishop_moves;;
assert (all_bishop_moves_not = [
("f1", "h3"); ("f1", "g2"); ("f1", "a6"); ("f1", "b5"); ("f1", "c4");
("f1", "d3"); ("f1", "e2"); ("c5", "f8"); ("c5", "e7"); ("c5", "d6");
("c5", "a3"); ("c5", "b4"); ("c5", "e3"); ("c5", "d4"); ("c5", "a7");
("c5", "b6")])

let short_bish = List.map (fun (from, to_) -> algebraic_move position (get_square_from_pos position.board from) (from, to_)) all_bishop_moves;;
let short_bish2 = change_to_short position all_bishop_moves;;
assert (short_bish = short_bish2);;

let all_rook_moves = gen_moves_for_all position all_rooks;;
let all_rook_moves_not = change_to_notation2 all_rook_moves;;
assert (all_rook_moves_not = [
("h1", "g1"); ("h1", "h4"); ("h1", "h3"); ("h1", "h2"); ("a1", "a3");
("a1", "a2")]);;

let all_queen_moves = gen_moves_for_all position all_queens;;
let all_queen_moves_not = change_to_notation2 all_queen_moves;;
assert (all_queen_moves_not = [
("a3", "c5"); ("a3", "b4"); ("a3", "b2"); ("a3", "a1"); ("a3", "a2");
("a3", "c3"); ("a3", "b3"); ("a3", "a6"); ("a3", "a5"); ("a3", "a4")]);;

let all_knight_moves = gen_moves_for_all position all_knights;;
let all_knight_moves_not = change_to_notation2 all_knight_moves;;
assert (all_knight_moves_not = [
("b1", "a3"); ("f4", "h3"); ("f4", "g2"); ("f4", "e2"); ("f4", "d3");
("f4", "d5"); ("f4", "e6"); ("f4", "g6")]);;

let all_pawn_moves = gen_moves_for_all position all_pawns;;
let all_pawn_moves_not = change_to_notation2 all_pawn_moves;;
assert (all_pawn_moves_not = [
("f2", "f3"); ("d2", "d3"); ("d2", "d4"); ("b2", "b3"); ("b2", "b4");
("b2", "a3"); ("g3", "g4"); ("c3", "c4")]);;

(* Test find all pieces of color*)
let all_black = find_all_pieces_of_color position.board (Some Black);;
let all_black_not = change_to_notation1 all_black;;
assert (all_black_not =  [
"a3"; "e5"; "f6"; "h7"; "g7"; "f7"; "d7"; "c7"; "b7"; "a7"; "h8"; "f8";
"e8"; "a8"]);;

let all_black_moves = gen_moves_for_all position all_black;;
let all_black_moves_not = change_to_notation2 all_black_moves;;
assert (all_black_moves_not = [
("a3", "c5"); ("a3", "b4"); ("a3", "b2"); ("a3", "a1"); ("a3", "a2");
("a3", "c3"); ("a3", "b3"); ("a3", "a6"); ("a3", "a5"); ("a3", "a4");
("e5", "f4"); ("f6", "h5"); ("f6", "g4"); ("f6", "e4"); ("f6", "d5");
("f6", "g8"); ("h7", "h6"); ("g7", "g6"); ("g7", "g5"); ("d7", "d6");
("d7", "d5"); ("c7", "c6"); ("b7", "b6"); ("b7", "b5"); ("a7", "a6");
("a7", "a5"); ("h8", "g8"); ("f8", "c5"); ("f8", "d6"); ("f8", "e7");
("e8", "d8"); ("e8", "c8"); ("a8", "d8"); ("a8", "c8"); ("a8", "b8")]) ;;

(* Test gen attacked *)
let all_black_attacked = gen_attacked_for_all position all_black;;
let all_black_attacked_not = change_to_notation2 all_black_attacked;;
assert (all_black_attacked_not = [
("a3", "c5"); ("a3", "b4"); ("a3", "b2"); ("a3", "a1"); ("a3", "a2");
("a3", "c3"); ("a3", "b3"); ("a3", "a6"); ("a3", "a5"); ("a3", "a4");
("e5", "f4"); ("f6", "h5"); ("f6", "g4"); ("f6", "e4"); ("f6", "d5");
("f6", "g8"); ("h7", "h6"); ("g7", "g6"); ("g7", "g5"); ("d7", "d6");
("d7", "d5"); ("c7", "c6"); ("b7", "b6"); ("b7", "b5"); ("a7", "a6");
("a7", "a5"); ("h8", "g8"); ("f8", "c5"); ("f8", "d6"); ("f8", "e7");
("e8", "d8"); ("e8", "e7"); ("a8", "d8"); ("a8", "c8"); ("a8", "b8")]);;

(* Test from_notation*)
assert (all_black_moves = (change_from_notation2 all_black_moves_not));;
assert (all_black = (change_from_notation1 all_black_not));;

let fen_board = "r3kb1r/pppp1ppp/5n2/2B1p2q/b1q1PN2/2P3P1/1P1P1P2/RN1BKB1R";;
(* let fen_board = "r3kb1r/pppp1ppp/5n2/2B1p2q/b1q1P3/2P3P1/1P1P1P2/RN1BKB1R"   Tests for queens *)

(* let fen_board = "rnbq1bnr/ppppppNp/k7/8/8/4N1N1/PPPPPPPP/RNBQKB1R";;  Tests for knights *)
(* let fen_input = fen_board ^ " w KQ - 0 1";; *)
(* let fen_board = "r3k2r/pppp1ppp/5n2/2B1p2Q/4PN2/q1P3P1/1P1P1P2/R3K2R" Tests for castling *)
(* let fen_input = fen_board ^ " w KQkq - 0 1" *)
(* let fen_position = parse_fen fen_input;; *)
let position = parse_fen fen_input;;
print_endline "\nChessboard state:";
print_chessboard fen_position.board;;

(* Test algebraic_piece*) 
let queen_alg = algebraic_piece queen;;
assert (queen_alg = "Q")

(* let move = algebraic_move position queen ((3, 7), (6, 4));; 
assert (move = "Qxd");; *)

let white_knight = {piece = Knight; color = Some White};;
let ambigous = get_ambigious position queen (3, 7) (6, 4);;

let move = algebraic_move position white_knight ((5, 6), (3, 5));; 
let ambigous = get_ambigious position white_knight (5, 4) (3, 5);;

let wanted_move = "Qd1";;
let ((from), (to_)) = if is_move_legal position wanted_move then find_move position wanted_move else failwith "Wrong_move";; 

let new_position = position_after_move position  from to_;; (*Move queen back to d1*)
let new_fen = fen_of_position new_position;; 
let new_pos = parse_fen new_fen;;


let wanted_move2 = "Qb3";;
let ((from2), (to_2)) = if is_move_legal new_pos wanted_move2 then find_move new_pos wanted_move2 else failwith "Wrong_move";; 

let new_position2 = position_after_move new_pos  from2 to_2;; 
let new_fen2 = fen_of_position new_position2;; 
let new_pos2 = parse_fen new_fen2;;


let fen_input = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";;
let position = parse_fen fen_input;;
let all_pieces = find_all_pieces_of_color position.board (Some position.turn);;
let all_moves = gen_moves_for_all position all_pieces;;
let all_moves_short = change_to_short position all_moves;;