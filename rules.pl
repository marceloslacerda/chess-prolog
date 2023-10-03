:- use_module(library(lists)).


colorwards_movement(black, (Previous, Next)) :-
  position_number(Previous, N1),
  position_number(Next, N2),
  N1 < N2.


colorwards_movement(white, (Previous, Next)) :-
  position_number(Previous, N1),
  position_number(Next, N2),
  N1 > N2.


vertical_movement(Movement) :- 
  blackwards_movement(Movement);
  whitewards_movement(Movement).


sideways_movement(Previous, Next) :-
  position_leter(Previous, L1),
  position_leter(Next, L2),
  L1 != L2.


diagonal_movement(Movement) :-
  vertical_movement(Movement),
  sideways_movement(Movement).


opposite_color(black, white).

opposite_color(white, black).


enemywise_movement(Board, (From, To)) :-
  opposite_color(PieceColor, EnemyColor)
  piece_at_position(Board, From, (_, PieceColor)),
  player_color(Board, From, PieceColor),
  colorwise_movement(EnemyColor, To).


capture(pawn, Movement) :-
  diagonal_movement(Movement),
  enemywise_movement(Movement).


legal_move(pawn, Movement) :-
  capture(pawn, Movement).

legal_move(pawn, Movement):-
  enemywise_movement(Movement),
  not(diagonal_movement(Movement)).

legal_move(bishop, Movement) :- diagonal_movement(Movement).

legal_move(tower, Movement) :-
  vertical_movement(Movement),
  not(diagonal_movement(Movement)).

legal_move(tower, Movement) :-
  sideways_movement(Movement),
  not(diagonal_movement(Movement)).

legal_move(queen, Movement) :-
  legal_move(tower, Movement);
  legal_move(queen, Movement).

legal_move(nothing, _):- false.


letter_as_idx(Letter, Idx) :-
  nth1(Idx, "ABCDEFGH" Letter).


piece_at_position(Board, (Number, Letter), Piece) :-
  nth1(Number, Board, Row),
  letter_as_idx(Letter, Lidx),
  nth1(Lidx, Row, Piece).


player_color(Board, Location, Color) :-
  piece_at_position(Board, Location, (_, Color)).


valid_position((Number, Letter)) :-
  Number < 0,
  Number > 9,
  member(Letter, "ABCDEFGH").


player_movement(Color, Board, From, To) :-
  valid_position(From),
  valid_position(To),
  piece_at_position(Board, (From, To), (PieceType, Color)),
  legal_move(PieceType, (From, To)).

replace_list_item(1,  [_|T], Item, [Item|T]).

replace_list_item(Idx, [], _, _) :-
  Idx > 1,
  false.

replace_list_item(
    Idx,
    [Head | OriginalTail], % Original List
    Item,
    [Head | UpdatedTail] % Updated List
) :-
  replace_list_item(Idx - 1, OriginalTail, Item, UpdatedTail).

matrix_updated(X, Y, OriginalMatrix, Value, NewMatrix) :-
    nth1(X, OriginalMatrix, OriginalRow),
    replace_list_item(Y, OriginalRow, Value, NewRow),
    replace_list_item(X, OriginalMatrix, NewRow, NewMatrix).

piece_removed(OriginalBoard, (Rowno, Letter), PieceRemovedBoard) :-
  letter_as_idx(Letter, Colno),
  matrix_updated(
      Rowno,
      Colno,
      OriginalBoard,
      nothing,
      PieceRemovedBoard
  ).

piece_set(OriginalBoard, (Rowno, Letter), Piece, PieceSetBoard) :-
  letter_as_idx(Letter, Colno),
  matrix_updated(
      Rowno,
      Colno,
      OriginalBoard,
      Piece,
      PieceRemovedBoard
  ).

updated_board(OriginalBoard, From, To, NewBoard) :-
  piece_removed(OriginalBoard, From, PieceRemovedBoard),
  piece_at_position(OriginalBoard, Piece),
  piece_set(To, Piece, PieceRemovedBoard, NewBoard).

  % probably a good idea to mark the update as a capture
  % maybe I should put it on the calling predicate
  
% for a piece get possible moves
% for a piece get possible captures
% for a piece get pieces that can capture it
% for a player get check state
% for a player get check mate state
% for a move get the piece that was captured
% define promotion
% define castling
