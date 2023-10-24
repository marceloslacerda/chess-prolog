:- begin_tests(rules).

:- ["rules.pl"].

test(valid_row) :-
    \+ valid_row(0),
    valid_row(1),
    valid_row(8),
    \+ valid_row(9).

test(valid_letter) :-
    valid_letter(a),
    valid_letter(h),
    \+ valid_letter(j).

test(position):-
    position(1, a),
    \+ position(0, _),
    \+ position(_, z).

test(colorwards_movement):-
    colorwards_movement(black, movement(position(1, _), position(2, _))),
    \+ colorwards_movement(black, movement(position(1, _), position(1, _))),
    \+ colorwards_movement(black, movement(position(2, _), position(1, _))),
    colorwards_movement(white, movement(position(2, _), position(1, _))),
    \+ colorwards_movement(white, movement(position(1, _), position(1, _))),
    \+ colorwards_movement(white, movement(position(1, _), position(2, _))).

test(vertical_movement):-
    vertical_movement(movement(position(1, _), position(2, _))),
    vertical_movement(movement(position(2, _), position(1, _))),
    \+ vertical_movement(movement(position(1, _), position(1, _))).


test(sideways_movement):-
    sideways_movement(movement(position(_, a), position(_, b))),
    sideways_movement(movement(position(_, b), position(_, a))),
    \+ sideways_movement(movement(position(_, a), position(_, a))).

test(letter_as_idx):-
    letter_as_idx(a, 1),
    letter_as_idx(b, 2),
    \+ letter_as_idx(z, _).

test(diagonal_movement):-
    diagonal_movement(movement(position(1, a), position(4, d))),
    diagonal_movement(movement(position(6, h), position(1, c))),
    diagonal_movement(movement(position(1, h), position(8, a))),
    \+ diagonal_movement(movement(position(1, a), position(3, b))).

test(piece_at_position):-
    piece_at_position([[
          square_contents(white, pawn)
        ]], position(1, a), square_contents(white, pawn)),
    piece_at_position([[
          square_contents(nothing),
          square_contents(white, pawn)
        ]], position(1, b), square_contents(white, pawn)),
    piece_at_position([
          [square_contents(nothing)],
          [square_contents(white, pawn)]
        ], position(2, a), square_contents(white, pawn)),
    piece_at_position([
            [square_contents(nothing)],
            [square_contents(white, pawn)]
          ], position(1, a), square_contents(nothing)),
    \+ piece_at_position([
        [square_contents(nothing)],
        [square_contents(white, pawn)]
      ], position(1, a), square_contents(white, pawn)).


test(vertical_distance):-
    vertical_distance(movement(position(1, _), position(2, _)), 1),
    vertical_distance(movement(position(1, _), position(6, _)), 5),
    vertical_distance(movement(position(7, _), position(2, _)), 5),
    \+ vertical_distance(movement(position(7, _), position(2, _)), 4).

test(player_color):-
    player_color([[square_contents(white, _)]], position(1, a), white).


test(enemywise_movement):-
    enemywise_movement(
        [
            [square_contents(white, _)],
            [square_contents(nothing)]
        ],
        movement(position(1, a), position(2, a))),
    enemywise_movement(
        [
            [square_contents(nothing)],
            [square_contents(black, _)]
        ], movement(position(2, a), position(1, a))),
    \+ enemywise_movement(
        [
            [(black, _)]
        ], movement(position(1, a), position(2, a))),
    \+ enemywise_movement(
        [
            [square_contents(nothing)],
            [square_contents(white, _)]
        ], movement(position(2, a), position(1, a))).
    
test(simple_pawn_movement):-
    simple_pawn_movement([[square_contents(white, pawn)], [square_contents(nothing)]], (position(1, a), position(2, a))),
    \+ simple_pawn_movement([[square_contents(white, pawn)], [square_contents(nothing)]], (position(1, a), position(3, a))),
    \+ simple_pawn_movement([[square_contents(white, pawn)], [square_contents(nothing)]], (position(1, a), position(2, b))).

test(bishop_movement):-
    bishop_movement(movement(position(1, a), position(3, c))),
    bishop_movement(movement(position(3, c), position(1, a))),
    bishop_movement(movement(position(4, h), position(1, e))),
    bishop_movement(movement(position(1, e), position(4, h))),
    \+ bishop_movement(movement(position(1, a), position(3, b))),
    \+ bishop_movement(movement(position(1, a), position(2, a))),
    \+ bishop_movement(movement(position(1, a), position(1, b))).

test(rook_movement):-
    rook_movement(movement(position(1, a), position(1, c))),
    rook_movement(movement(position(1, c), position(1, a))),
    rook_movement(movement(position(1, a), position(3, a))),
    rook_movement(movement(position(3, a), position(1, a))),
    \+ rook_movement(movement(position(4, h), position(1, e))),
    \+ rook_movement(movement(position(1, e), position(4, h))),
    \+ rook_movement(movement(position(1, a), position(3, b))).

test(queen_movement):-
    queen_movement(movement(position(1, a), position(1, c))),
    queen_movement(movement(position(1, c), position(1, a))),
    queen_movement(movement(position(1, a), position(3, a))),
    queen_movement(movement(position(3, a), position(1, a))),
    queen_movement(movement(position(1, a), position(3, c))),
    queen_movement(movement(position(3, c), position(1, a))),
    queen_movement(movement(position(4, h), position(1, e))),
    queen_movement(movement(position(1, e), position(4, h))),
    \+ queen_movement(movement(position(1, a), position(3, b))).

test(knight_movement):-
    knight_movement(movement(position(1, a), position(3, b))),
    knight_movement(movement(position(1, b), position(3, a))),
    knight_movement(movement(position(1, a), position(2, c))),
    knight_movement(movement(position(1, a), position(2, c))),
    knight_movement(movement(position(2, a), position(1, c))),
    \+ knight_movement(movement(position(1, a), position(1, c))),
    \+ knight_movement(movement(position(3, a), position(1, a))),
    \+ knight_movement(movement(position(1, a), position(3, c))),
    \+ knight_movement(movement(position(3, c), position(1, a))).


test(king_movement):-
    king_movement(movement(position(2, b), position(1, a))),
    king_movement(movement(position(2, b), position(1, b))),
    king_movement(movement(position(2, b), position(2, a))),
    king_movement(movement(position(2, b), position(3, a))),
    king_movement(movement(position(2, b), position(3, b))),
    king_movement(movement(position(2, b), position(3, c))),
    king_movement(movement(position(2, b), position(2, c))),
    \+ king_movement(movement(position(2, b), position(4, b))).

test(historyless_movement):-
    historyless_movement([[square_contents(white, pawn)]], movement(position(1, a), position(2, a))),
    \+ historyless_movement([[square_contents(white, pawn)], [square_contents(nothing)]], movement(position(1, a), position(2, b))),
    historyless_movement([[square_contents(white, bishop)]], movement(position(1, a), position(3, c))),
    \+ historyless_movement([[square_contents(white, bishop)]], movement(position(1, a), position(3, b))),
    historyless_movement([[square_contents(white, rook)]], movement(position(1, a), position(1, c))),
    \+ historyless_movement([[square_contents(white, rook)]], movement(position(1, a), position(3, c))),
    historyless_movement([[square_contents(white, queen)]], movement(position(1, a), position(1, c))),
    \+ historyless_movement([[square_contents(white, queen)]], movement(position(1, a), position(3, b))),
    historyless_movement([[square_contents(knight, white)]], movement(position(1, a), position(3, b))),
    \+ historyless_movement([[square_contents(knight, white)]], movement(position(1, a), position(1, c))),
    historyless_movement([[square_contents(white, king)]], movement(position(1, a), position(2, b))),
    \+ historyless_movement([[square_contents(white, king)]], movement(position(1, a), position(1, c))),
    true.

test(simple_pawn_capture_movement):-
    simple_pawn_capture_movement([[square_contents(white, pawn)],[square_contents(nothing), square_contents(pawn, black)]], movement(position(1, a), position(2, b))),
    true.

:- end_tests(rules).