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
    not(position((0, _), _, _)),
    not(position((_, _), 0, _)),
    not(position((_, z), _, _)),
    not(position((_, _), _, z)).

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
    \+ letter_as_idx(z, 2).

test(diagonal_movement):-
    diagonal_movement(?).

:- end_tests(rules).