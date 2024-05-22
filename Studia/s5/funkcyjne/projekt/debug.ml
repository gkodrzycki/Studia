let fen_input = "1nb1k2r/P1pp1ppp/5n2/2B1p2Q/4PN2/q1P3P1/pP1P1P1P/1N2KB1R w Kk - 0 1"
let fen_position = parse_fen fen_input;;
let position = parse_fen fen_input;;
let piece = {piece = Pawn; color = Some White};;
let all_pieces = find_all_pieces position.board piece;;
let all_pieces_not = change_to_notation1 all_pieces;;
let all_piece_moves = gen_moves_for_all position all_pieces;;
let all_piece_moves_not = change_to_notation2 all_piece_moves;;
let all_piece_moves_short = change_to_short position all_piece_moves;;

game fen_position;;