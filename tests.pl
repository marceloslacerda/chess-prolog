:- begin_tests(rules).

:- ["rules.pl"].

test(valid_row) :-
    not(valid_row(0)),
    valid_row(1),
    valid_row(8),
    not(valid_row(9)).

test(valid_letter) :-
    valid_letter(a),
    valid_letter(h),
    not(valid_letter(j)).

test(position):-
    position((1, a), 1, a),
    not(position((1, a), 2, a)),
    not(position((0, _), 0, _)),
    not(position((_, z), _, z)).

test(colorwards_movement):-
    colorwards_movement(black, ((1, _), (2, _))),
    \+ colorwards_movement(black, ((1, _), (1, _))),
    \+ colorwards_movement(black, ((2, _), (1, _))),
    colorwards_movement(white, ((2, _), (1, _))),
    \+ colorwards_movement(white, ((1, _), (1, _))),
    \+ colorwards_movement(white, ((1, _), (2, _))).

test(vertical_movement):-
    vertical_movement(((1, _), (2, _))),
    vertical_movement(((2, _), (1, _))),
    \+ vertical_movement(((1, _), (1, _))).


test(sideways_movement):-
    sideways_movement(((_, a), (_, b))),
    sideways_movement(((_, b), (_, a))),
    \+ sideways_movement(((_, a), (_, a))).

test(letter_as_idx):-
    letter_as_idx(a, 1),
    letter_as_idx(b, 2),
    \+ letter_as_idx(z, _).

test(diagonal_movement):-
    diagonal_movement(((1, a), (4, d))),
    diagonal_movement(((6, h), (1, c))),
    diagonal_movement(((1, h), (8, a))),
    \+ diagonal_movement(((1, a), (3, b))).

test(piece_at_position):-
    piece_at_position([[mypiece]], (1, a), mypiece),
    piece_at_position([[nothing, mypiece]], (1, b), mypiece),
    piece_at_position([nothing, [mypiece]], (2, a), mypiece),
    \+ piece_at_position([[nothing], [mypiece]], (1, a), mypiece).

test(enemywise_movement):-
    enemywise_movement([[(_, white)], [nothing]], ((1, _), (2, _))),
    enemywise_movement([empty, [(_, black)]], ((2, _), (1, _))),
    \+ enemywise_movement([[(_, black)]], ((1, _), (2, _))),
    \+ enemywise_movement([empty, [(_, white)]], ((2, _), (1, _))).

test(legal_move_pawn):-
    legal_move(pawn, [[(pawn, white)], [nothing]], ((1, a), (2, a))),
    \+ legal_move(pawn, [[(pawn, white)], [nothing]], ((1, a), (3, a))),
    \+ legal_move(pawn, [[(pawn, white)], [nothing]], ((1, a), (2, b))).

test(legal_move_bishop):-
    legal_move(bishop, _, ((1, a), (3, c))),
    legal_move(bishop, _, ((3, c), (1, a))),
    legal_move(bishop, _, ((4, h), (1, e))),
    legal_move(bishop, _, ((1, e), (4, h))),
    \+ legal_move(bishop, _, ((1, a), (3, b))),
    \+ legal_move(bishop, _, ((1, a), (2, a))),
    \+ legal_move(bishop, _, ((1, a), (1, b))).

test(legal_move_tower):-
    legal_move(tower, _, ((1, a), (1, c))),
    legal_move(tower, _, ((1, c), (1, a))),
    legal_move(tower, _, ((1, a), (3, a))),
    legal_move(tower, _, ((3, a), (1, a))),
    \+ legal_move(tower, _, ((4, h), (1, e))),
    \+ legal_move(tower, _, ((1, e), (4, h))).

test(legal_move_queen):-
    legal_move(queen, _, ((1, a), (1, c))),
    legal_move(queen, _, ((1, c), (1, a))),
    legal_move(queen, _, ((1, a), (3, a))),
    legal_move(queen, _, ((3, a), (1, a))),
    legal_move(queen, _, ((1, a), (3, c))),
    legal_move(queen, _, ((3, c), (1, a))),
    legal_move(queen, _, ((4, h), (1, e))),
    legal_move(queen, _, ((1, e), (4, h))),
    \+ legal_move(queen, _, ((1, a), (3, b))).


:- end_tests(rules).