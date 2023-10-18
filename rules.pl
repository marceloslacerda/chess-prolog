:- use_module(library(lists)).
:- use_module(library(clpfd)).


valid_row(Row) :-
    Row #< 9,
    Row #> 0.

valid_letter(Letter) :-
  member(Letter, [a, b, c, d, e, f, g, h]).

position((Row, Letter), Row, Letter) :-
  valid_row(Row),
  valid_letter(Letter).

colorwards_movement(black, (Previous, Next)) :-
  position(Previous, N1, _),
  position(Next, N2, _),
  N1 #< N2.


colorwards_movement(white, (Previous, Next)) :-
  position(Previous, N1, _),
  position(Next, N2, _),
  N1 #> N2.


vertical_movement((Previous, Next)) :- 
  position(Previous, N1, _),
  position(Next, N2, _),
  N1 #\= N2.


sideways_movement((Previous, Next)) :-
  position(Previous, _, L1),
  position(Next, _, L2),
  L1 \= L2.

letter_as_idx(Letter, Idx) :-
  nth1(Idx, [a, b, c, d, e, f, g, h], Letter).


diagonal_movement((Previous, Next)) :-
  position(Previous, R1, L1),
  position(Next, R2, L2),
  letter_as_idx(L1, Col1),
  letter_as_idx(L2, Col2),
  is_diagonal(R1, Col1, R2, Col2).


is_diagonal(X1, Y1, X2,  Y2):-
  C #= abs(X1 - X2),
  C #= abs(Y1 - Y2),
  C #> 1.


opposite_color(black, white).

opposite_color(white, black).


piece_at_position(Board, (Number, Letter), Piece) :-
  nth1(Number, Board, Row),
  letter_as_idx(Letter, Lidx),
  nth1(Lidx, Row, Piece).

player_color(Board, Location, Color) :-
  piece_at_position(Board, Location, (_, Color)).

enemywise_movement(Board, (From, To)) :-
  opposite_color(PieceColor, EnemyColor),
  player_color(Board, From, PieceColor),
  colorwards_movement(EnemyColor, (From, To)).

vertical_distance(((FromRow, _), (ToRow, _)), Distance):-
  Distance #= abs(FromRow - ToRow).

simple_pawn_movement(Board, Movement) :-
  enemywise_movement(Board, Movement),
  not(sideways_movement(Movement)),
  vertical_distance(Movement, 1).

bishop_movement(Movement) :-
  diagonal_movement(Movement).

rook_movement(Movement) :-
  vertical_movement(Movement),
  not(sideways_movement(Movement)).

rook_movement(Movement) :-
  sideways_movement(Movement),
  not(vertical_movement(Movement)).

queen_movement(Movement) :-
  rook_movement(Movement);
  bishop_movement(Movement).


knight_movement((From, To)):-
  position(From, R1, L1),
  position(To, R2, L2),
  letter_as_idx(L1, Col1),
  letter_as_idx(L2, Col2),
  (
    (
      2 #= abs(R1 - R2),
      1 #= abs(Col1 - Col2)
    );
    (
      1 #= abs(R1 - R2),
      2 #= abs(Col1 - Col2)
    )
  ).

king_movement((From, To)):-
  position(From, R1, L1),
  position(To, R2, L2),
  letter_as_idx(L1, Col1),
  letter_as_idx(L2, Col2),
  DistV #= abs(R1 - R2),
  DistH #= abs(Col1 - Col2),
  DistV #< 2,
  DistH #< 2,
  0 #< DistV + DistH,
  3 #> DistV + DistH.

historyless_boardless_movement(Movement):-
  king_movement(Movement)
  ; knight_movement(Movement)
  ; bishop_movement(Movement)
  ; queen_movement(Movement)
  ; rook_movement(Movement).

historyless_movement(Board, Movement):-
  simple_pawn_movement(Board, Movement)
  ; historyless_boardless_movement(Movement).

% define castling: castling is not a move from->to because two pieces are moved
% so it's better to define it elsewhere other than legal_move
% same for en-passant

standard_player_movement(Board, (PieceType, Color), Movement) :-
  piece_at_position(Board, Movement, (PieceType, Color)),
  legal_move(PieceType, Board, Movement).

replace_list_item(1,  [_|T], Item, [Item|T]).

replace_list_item(Idx, [], _, _) :-
  Idx #> 1,
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
      PieceSetBoard
  ).


updated_board(OriginalBoard, From, To, NewBoard) :-
  piece_removed(OriginalBoard, From, PieceRemovedBoard),
  standard_player_movement(OriginalBoard, Piece, (From, To)),
  piece_set(To, Piece, PieceRemovedBoard, NewBoard).


% create updated_history predicate
% maybe I should put it on the calling predicate

% for a piece get possible moves legal_move, minus castling and en-passant
% en-passant requires a predicate that has the history of the game as a variable
% castling also requires the history since neither the rook nor the king must have been moved before
% create caputred predicate
% implement cant capture your own piece
% for a piece get possible captures
% for a piece get pieces that can capture it
% for a player get check state
% for a player get check mate state
% for a move get the piece that was captured

