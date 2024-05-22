let fen_input = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";;
(* let fen_input = "2b1k2r/2pp1ppp/1Q3n2/2B1p2Q/4PN2/q1P3P1/qP1P1P1P/1N2KB1R b Kk - 98 3"   *)
let position = parse_fen fen_input;;

let rec game pos =
  print_chessboard pos.board;
  print_string "\n";
  print_string "Write move\nWrite 2 if you want to print current fen\n";
  let decide = read_line() in
  if decide <> "2" && pos.halfmove_clock < 100 then
    let wanted_move = decide in 
    let ((from), (to_)) = 
    if is_move_legal pos wanted_move then 
      find_move pos wanted_move 
    else begin
      print_string "Wrong_move\n"; 
      game pos end in
    
    let new_position = position_after_move pos from to_ in
    let new_fen = fen_of_position new_position in
    let new_pos = parse_fen new_fen in
    game new_pos
  else if decide <> "2" && pos.halfmove_clock > 99 then
    failwith "It's a draw by 50 moves rule!"
  else 
    let cur_fen = fen_of_position pos in 
    print_string "Current FEN is: ";
    print_string cur_fen;
    print_string "\n";
    game pos
;;

game position;;