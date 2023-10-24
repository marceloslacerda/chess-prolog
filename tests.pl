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
    simple_pawn_movement([[(pawn, white)], [nothing]], ((1, a), (2, a))),
    \+ simple_pawn_movement([[(pawn, white)], [nothing]], ((1, a), (3, a))),
    \+ simple_pawn_movement([[(pawn, white)], [nothing]], ((1, a), (2, b))).

test(bishop_movement):-
    bishop_movement(((1, a), (3, c))),
    bishop_movement(((3, c), (1, a))),
    bishop_movement(((4, h), (1, e))),
    bishop_movement(((1, e), (4, h))),
    \+ bishop_movement(((1, a), (3, b))),
    \+ bishop_movement(((1, a), (2, a))),
    \+ bishop_movement(((1, a), (1, b))).

test(rook_movement):-
    rook_movement(((1, a), (1, c))),
    rook_movement(((1, c), (1, a))),
    rook_movement(((1, a), (3, a))),
    rook_movement(((3, a), (1, a))),
    \+ rook_movement(((4, h), (1, e))),
    \+ rook_movement(((1, e), (4, h))),
    \+ rook_movement(((1, a), (3, b))).

test(queen_movement):-
    queen_movement(((1, a), (1, c))),
    queen_movement(((1, c), (1, a))),
    queen_movement(((1, a), (3, a))),
    queen_movement(((3, a), (1, a))),
    queen_movement(((1, a), (3, c))),
    queen_movement(((3, c), (1, a))),
    queen_movement(((4, h), (1, e))),
    queen_movement(((1, e), (4, h))),
    \+ queen_movement(((1, a), (3, b))).

test(knight_movement):-
    knight_movement(((1, a), (3, b))),
    knight_movement(((1, b), (3, a))),
    knight_movement(((1, a), (2, c))),
    knight_movement(((1, a), (2, c))),
    knight_movement(((2, a), (1, c))),
    \+ knight_movement(((1, a), (1, c))),
    \+ knight_movement(((3, a), (1, a))),
    \+ knight_movement(((1, a), (3, c))),
    \+ knight_movement(((3, c), (1, a))).


test(king_movement):-
    king_movement(((2, b), (1, a))),
    king_movement(((2, b), (1, b))),
    king_movement(((2, b), (2, a))),
    king_movement(((2, b), (3, a))),
    king_movement(((2, b), (3, b))),
    king_movement(((2, b), (3, c))),
    king_movement(((2, b), (2, c))),
    \+ king_movement(((2, b), (4, b))).

test(historyless_movement):-
    historyless_movement([[(pawn, white)]], ((1, a), (2, a))),
    \+ historyless_movement([[(pawn, white)], [nothing]], ((1, a), (2, b))),
    historyless_movement([[(bishop, white)]], ((1, a), (3, c))),
    \+ historyless_movement([[(bishop, white)]], ((1, a), (3, b))),
    historyless_movement([[(rook, white)]], ((1, a), (1, c))),
    \+ historyless_movement([[(rook, white)]], ((1, a), (3, c))),
    historyless_movement([[(queen, white)]], ((1, a), (1, c))),
    \+ historyless_movement([[(queen, white)]], ((1, a), (3, b))),
    historyless_movement([[(knight, white)]], ((1, a), (3, b))),
    \+ historyless_movement([[(knight, white)]], ((1, a), (1, c))),
    historyless_movement([[(king, white)]], ((1, a), (2, b))),
    \+ historyless_movement([[(king, white)]], ((1, a), (1, c))),
    true.

test(simple_pawn_capture_movement):-
    simple_pawn_capture_movement([[(pawn, white)],[nothing, (pawn, black)]], ((1, a), (2, b))),
    true.

:- end_tests(rules).