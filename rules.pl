:- use_module(library(lists)).

blackwards_movement((Previous, Next)) :-
  position_number(Previous, N1),
  position_number(Next, N2),
  N1 < N2.

whitewards_movement((Previous, Next)) :-
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

enemywise_movement(Board, (From, _)) :-
  player_color(Board, From, black),
  whitewise_movement(Movement).

enemywise_movement((From, _)) :-
  player_color(From, white),
  blackwise_movement(Movement).

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

player_owns(Location) :-
  

player_movement(Color, Board, From, To) :-
  valid_position(From),
  valid_position(To),
  piece_at_position(Board, (From, To), Piece),
  player_owns(Color, Piece),
  legal_move(Piece, (From, To)).

