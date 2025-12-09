:- ['gridArrangement1.pl'].

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
obstacle(leaf, Row, Col).
obstacle(flower, Row, Col).
obstacle(apple, Row, Col).
obstacle(berry, Row, Col).
obstacle(water, Row, Col).

%-------------------------------------------------------------------------------------------%
% Defining Objects:
%-------------------------------------------------------------------------------------------%

% is_object(X) :- X in {lemonade}
is_object(lemonade).

% object(X,A,B) :- X in {lemonade} AND A AND B
object(lemonade, Row, Col).

%-------------------------------------------------------------------------------------------%
% Definition for Empty Cells
%-------------------------------------------------------------------------------------------%

is_none(empty).

none(empty, Row, Col).

%-------------------------------------------------------------------------------------------%
% Definition for Extra Grid Utilities:
%-------------------------------------------------------------------------------------------%

% find the max row value
row_bound(Bound) :-
    grid_height(Height),
    Bound is Height - 1.

% find the max col value
col_bound(Bound) :-
    grid_width(Width),
    Bound is Width - 1.

% in_bounds(X,Y) :- X >= 0 AND X <= B1 AND Y >= 0 AND Y <= B2
in_bounds(Row, Col) :-
    row_bound(B1),
    col_bound(B2),
    Row >= 0,
    Row =< B1,
    Col >= 0,
    Col =< B2.

% at_floor(Y) :- Y == B
at_floor(Col) :-
    row_bound(B),
    Col =:= B.

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

