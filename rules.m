:- module rules.
:- interface.

:- implementation.

:- type colors ---> black; white.

:- type pices --->
    king;
    queen;
    rook;
    bishop;
    knight;
    pawn.

:- type square_contents 
    ---> empty_square;
         square_contents(
             pice :: pices,
            color :: colors
         ).

:- type position
    ---> positions(rank :: int, file :: int).

:- type movements
    ---> movement(from :: position, to :: position).

:- pred colorwards_movement(colors, movement) is nondet.
colorwards_movement(black, movement(position(FromRank, _), position(ToRank, _))):-
    FromRank < ToRank.