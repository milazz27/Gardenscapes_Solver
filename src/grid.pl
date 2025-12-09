
%-------------------------------------------------------------------------------------------%
% Defining Obstacle Objects:
%-------------------------------------------------------------------------------------------%

% is_obstacle(X) :- X in {leaf, flower, apple, berry, water}
is_obstacle(leaf).
is_obstacle(flower).
is_obstacle(apple).
is_obstacle(berry).
is_obstacle(water).

% obstacle(X, A, B) :- X in {leaf, flower, apple, berry, water} AND A AND B
obstacle(leaf, pos(Row, Col)).
obstacle(flower, pos(Row, Col)).
obstacle(apple, pos(Row, Col)).
obstacle(berry, pos(Row, Col)).
obstacle(water, pos(Row, Col)).

%-------------------------------------------------------------------------------------------%
% Defining Objects:
%-------------------------------------------------------------------------------------------%

% is_object(X) :- X in {lemonade}
is_object(lemonade).

% object(X,A,B) :- X in {lemonade} AND A AND B
object(lemonade, pos(Row, Col)).

%-------------------------------------------------------------------------------------------%
% Definition for Empty Cells
%-------------------------------------------------------------------------------------------%

is_none(empty).

none(empty, pos(Row, Col)).

%-------------------------------------------------------------------------------------------%
% Definition for Extra Grid Utilities:
%-------------------------------------------------------------------------------------------%

get_row_col(pos(R, C), R, C).

get_row(pos(R, _), R).

get_col(pos(_,C), C).

%-------------------------------------------------------------------------------------------%
% Definition for Adjacency:
%-------------------------------------------------------------------------------------------%

% check right
adjacent(R1, C1, R2, C2) :-
    Right is R1 + 1,
    R2 =:= Right.

% check below
adjacent(R1, C1, R2, C2) :-
    Below is C1 + 1,
    C2 =:= Below.

% check left
adjacent(R1, C1, R2, C2) :-
    Left is R1 - 1,
    R2 =:= Left.

% check above
adjacent(R1, C1, R2, C2) :-
    Above is C1 - 1,
    C2 =:= Above.

%-------------------------------------------------------------------------------------------%
% Definition for Valid Swaps:
%-------------------------------------------------------------------------------------------%

unswappable(P1, P2) :- 
    is_empty(P1),
    is_empty(P2),
    !.

unswappable(P1, P2) :- 
    is_object(P1),
    is_object(P2).

valid_swap(R1, C1, R2, C2) :-
    piece(Type1, R1, C1),
    piece(Type2, R2, C2),
    \+ unswappable(Type1, Type2),
    adjacent(R1, C1, R2, C2).

