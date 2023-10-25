:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- ['util.pl'].


valid_row(Row) :-
    Row #< 9,
    Row #> 0.

valid_letter(Letter) :-
  member(Letter, [a, b, c, d, e, f, g, h]).

position(Row, Letter):-
  valid_row(Row),
  valid_letter(Letter).

valid_piece(king).
valid_piece(queen).
valid_piece(rook).
valid_piece(bishop).
valid_piece(knight).
valid_piece(pawn).

valid_color(white).
valid_color(black).


square_contents(nothing).

square_contents(Color, Piece):-
  valid_piece(Piece),
  valid_color(Color).

movement(From, To):-
  From=position(_, _),
  To=position(_, _).

colorwards_movement(black, movement(Previous, Next)) :-
  Previous=position(N1, _),
  Next=position(N2, _),
  N1 #< N2.


colorwards_movement(white, movement(Previous, Next)) :-
  Previous=position(N1, _),
  Next=position(N2, _),
  N1 #> N2.


vertical_movement(movement(Previous, Next)) :- 
  Previous=position(N1, _),
  Next=position(N2, _),
  N1 #\= N2.


sideways_movement(movement(Previous, Next)) :-
  Previous=position(_, L1),
  Next=position(_, L2),
  L1 \= L2.

letter_as_idx(Letter, Idx) :-
  nth1(Idx, [a, b, c, d, e, f, g, h], Letter).


diagonal_movement(movement(Previous, Next)) :-
  Previous=position(R1, L1),
  Next=position(R2, L2),
  letter_as_idx(L1, Col1),
  letter_as_idx(L2, Col2),
  is_diagonal(R1, Col1, R2, Col2).


is_diagonal(X1, Y1, X2,  Y2):-
  C #= abs(X1 - X2),
  C #= abs(Y1 - Y2),
  C #> 0.


opposite_color(black, white).

opposite_color(white, black).


piece_at_position(Board, position(Number, Letter), Piece) :-
  (
    Piece = square_contents(_, _);
    Piece = square_contents(_)
  ),
  Piece,
  nth1(Number, Board, Row),
  letter_as_idx(Letter, Lidx),
  nth1(Lidx, Row, Piece).


player_color(Board, Location, Color) :-
  piece_at_position(Board, Location, square_contents(Color, _)).

enemywise_movement(Board, movement(From, To)) :-
  player_color(Board, From, PieceColor),
  opposite_color(PieceColor, EnemyColor),
  colorwards_movement(EnemyColor, movement(From, To)).

vertical_distance(movement(position(FromRow, _), position(ToRow, _)), Distance):-
  Distance #= abs(FromRow - ToRow).


simple_pawn_movement(Board, Movement) :-
  enemywise_movement(Board, Movement),
  not(sideways_movement(Movement)),
  vertical_distance(Movement, 1).


horizontal_distance(FromLetter, ToLetter, Distance):-
  letter_as_idx(FromLetter, FromColumn),
  letter_as_idx(ToLetter, ToColumn),
  Distance #= abs(ToColumn-FromColumn).


skip_two_squares_pawn_move(Board, Movement):-
  Movement = movement(position(2, FromLetter), position(4, ToLetter)),
  piece_at_position(Board, position(2, FromLetter), square_contents(white, pawn)),
  horizontal_distance(FromLetter, ToLetter, 0).

skip_two_squares_pawn_move(Board, Movement):-
  Movement = movement(position(7, FromLetter), position(5, ToLetter)),
  piece_at_position(Board, position(7, FromLetter), square_contents(black, pawn)),
  horizontal_distance(FromLetter, ToLetter, 0).

pawn_movement(Board, Movement):-
  simple_pawn_movement(Board, Movement).

pawn_movement(Board, Movement):-
  skip_two_squares_pawn_move(Board, Movement).

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


knight_movement(movement(From, To)):-
  From = position(R1, L1),
  To = position(R2, L2),
  From, To,
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

king_movement(movement(From, To)):-
  From = position(R1, L1),
  To = position(R2, L2),
  From, To,
  letter_as_idx(L1, Col1),
  letter_as_idx(L2, Col2),
  DistV #= abs(R1 - R2),
  DistH #= abs(Col1 - Col2),
  DistV #< 2,
  DistH #< 2,
  0 #< DistV + DistH,
  3 #> DistV + DistH.

stateless_movement(Board, Movement):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, king)),
  king_movement(Movement).

stateless_movement(Board, Movement):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, knight)),
  knight_movement(Movement).

stateless_movement(Board, Movement):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, bishop)),
  bishop_movement(Movement).

stateless_movement(Board, Movement):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, queen)),
  queen_movement(Movement).

stateless_movement(Board, Movement):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, rook)),
  rook_movement(Movement).

stateless_movement(Board, Movement):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, pawn)),
  pawn_movement(Board, Movement).


% holds for a movement where a piece lands on top of another (different colors)
stateless_capture(Board, movement(From, To), Captured):-
  piece_at_position(Board, From, square_contents(CapturerColor, _)),
  piece_at_position(Board, To, Captured),
  Captured=square_contents(CapturedColor, _),
  opposite_color(CapturedColor, CapturerColor).

% Only models the movement of capture, not the capture itself
simple_pawn_capture_movement(Board, Movement):-
  enemywise_movement(Board, Movement),
  diagonal_movement(Movement),
  vertical_distance(Movement, 1).


stateless_action(Board, Movement, Captured):-
  stateless_movement(Board, Movement),
  stateless_capture(Board, Movement, Captured).

stateless_action(Board, Movement, Captured):-
  Movement=movement(From, _),
  piece_at_position(Board, From, square_contents(_, pawn)), % todo forgot to test this
  simple_pawn_capture_movement(Board, Movement),
  stateless_capture(Board, Movement, Captured).

stateless_action(Board, Movement, square_contents(nothing)):-
  stateless_movement(Board, Movement),
  Movement=movement(_, To),
  piece_at_position(Board, To, square_contents(nothing)).


castling_side(queen).
castling_side(king).

castling_avaliability(Color, []):-
  valid_color(Color).

castling_avaliability(Color, [CastlingSide|Others]):-
  castling_side(CastlingSide),
  castling_avaliability(Color, Others).

enpassant_target(nowhere).

enpassant_target(Target):-
  Target=position(_, _),
  Target.

board_state(
  _, % List of list of square_contents
  ActiveColor,
  WhiteCastlingAvaliability,
  BlackCastlingAvaliability,
  EnPassantTarget,
  HalfmoveClock,
  FullmoveNumber
  ):-
  valid_color(ActiveColor),
  WhiteCastlingAvaliability = castling_avaliability(white, _),
  BlackCastlingAvaliability = castling_avaliability(black, _),
  EnPassantTarget=enpassant_target(_),
  EnPassantTarget,
  HalfmoveClock #>= 0,
  HalfmoveClock #=< 50,
  FullmoveNumber #>= 1. 



% todo historied actions

% define castling: castling is not a move from->to because two pieces are moved
% so it's better to define it elsewhere other than legal_move
% same for en-passant

/*standard_player_movement(Board, (PieceType, Color), Movement) :-
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

*/