type piece = King | Queen | Rook | Bishop | Knight | Pawn | Empty

type color = White | Black

type square = { piece: piece; color: color option }

type board = square list list

type fen_position = {
  board: board;
  turn: color;
  castling: string;
  en_passant: string;
  halfmove_clock: int;
  fullmove_number: int;
}

let parse_rank rank_str =
  let rec aux acc count = function
    | [] -> List.rev acc
    | hd :: tl ->
      if hd = '/' then
        aux acc count tl
      else if '1' <= hd && hd <= '8' then
        aux (List.rev_append (List.init (Char.code hd - Char.code '0') (fun _ -> { piece = Empty; color = None })) acc) 0 tl
      else
        let piece =
          match hd with
          | 'K' -> { piece = King; color = Some White }
          | 'Q' -> { piece = Queen; color = Some White }
          | 'R' -> { piece = Rook; color = Some White }
          | 'B' -> { piece = Bishop; color = Some White }
          | 'N' -> { piece = Knight; color = Some White }
          | 'P' -> { piece = Pawn; color = Some White }
          | 'k' -> { piece = King; color = Some Black }
          | 'q' -> { piece = Queen; color = Some Black }
          | 'r' -> { piece = Rook; color = Some Black }
          | 'b' -> { piece = Bishop; color = Some Black }
          | 'n' -> { piece = Knight; color = Some Black }
          | 'p' -> { piece = Pawn; color = Some Black }
          | _ -> failwith "Invalid FEN string"
        in
        aux (piece :: acc) (count + 1) tl
  in
  aux [] 0 (String.to_seq rank_str |> List.of_seq)
;;

let parse_fen fen_str : fen_position =
  match String.split_on_char ' ' fen_str with
  | board_str :: turn_str :: castling :: en_passant :: halfmove_clock_str :: fullmove_number_str :: [] ->
    let board = List.map parse_rank (String.split_on_char '/' board_str) in
    let turn = match turn_str with "w" -> White | "b" -> Black | _ -> failwith "Invalid FEN string" in
    let halfmove_clock = int_of_string halfmove_clock_str in
    let fullmove_number = int_of_string fullmove_number_str in
    { board; turn; castling; en_passant; halfmove_clock; fullmove_number }
  | _ -> failwith "Invalid FEN string"
;;  

let fen_of_board (position : board) : string =
  let piece_to_char piece =
    match piece.color with
    | Some White -> (
        match piece.piece with
        | King -> "K"
        | Queen -> "Q"
        | Rook -> "R"
        | Bishop -> "B"
        | Knight -> "N"
        | Pawn -> "P"
        | Empty -> ""
      )
    | Some Black -> (
        match piece.piece with
        | King -> "k"
        | Queen -> "q"
        | Rook -> "r"
        | Bishop -> "b"
        | Knight -> "n"
        | Pawn -> "p"
        | Empty -> ""
      )
    | None -> ""
  in

  let row_to_fen row =
    let rec count_empty acc count = function
      | [] -> if count > 0 then acc ^ string_of_int count else acc
      | hd :: tl -> (
          match hd.piece with
          | Empty -> count_empty acc (count + 1) tl
          | _ ->
            let char = piece_to_char hd in
            let acc' =
              if count > 0 then acc ^ string_of_int count ^ char
              else acc ^ char
            in
            count_empty acc' 0 tl)
    in
    count_empty "" 0 row
  in

  let board_str =
    List.map row_to_fen position
    |> String.concat "/"
  in

  board_str
;;

let fen_of_position (position : fen_position) = 
  let pos_board = fen_of_board position.board in
  let pos_turn = if position.turn = White then "w" else "b" in
  let pos_halfmove_clock = string_of_int position.halfmove_clock in
  let pos_fullmove_number = string_of_int position.fullmove_number in
  String.concat " " [pos_board; pos_turn; position.castling; position.en_passant; pos_halfmove_clock; pos_fullmove_number]
;;

let print_chessboard (board : board) =
  List.iter
    (fun row ->
      List.iter
        (fun square ->
          match square.piece with
          | King -> (
            match square.color with 
            | Some White -> print_string "K "
            | Some Black -> print_string "k "
            | None -> failwith "Missing piece's color"
          )
          | Queen -> (
            match square.color with 
            | Some White -> print_string "Q "
            | Some Black -> print_string "q "
            | None -> failwith "Missing piece's color"
          )
          | Rook -> (
            match square.color with 
            | Some White -> print_string "R "
            | Some Black -> print_string "r "
            | None -> failwith "Missing piece's color"
          )
          | Bishop -> (
            match square.color with 
            | Some White -> print_string "B "
            | Some Black -> print_string "b "
            | None -> failwith "Missing piece's color"
          )
          | Knight -> (
            match square.color with 
            | Some White -> print_string "N "
            | Some Black -> print_string "n "
            | None -> failwith "Missing piece's color"
          )
          | Pawn -> (
            match square.color with 
            | Some White -> print_string "P "
            | Some Black -> print_string "p "
            | None -> failwith "Missing piece's color"
          )
          | Empty -> print_string ". ")
        row;
      print_endline "")
    board
;;
