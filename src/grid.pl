
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

%===============================================================================================%
%   Defining Getters:                                                                           %
%===============================================================================================%

%===============================================================================================%
%   Dimension Getters:                                                                          %
%===============================================================================================%

get_row_col(pos(R, C), R, C).
get_row(pos(R, _), R).
get_col(pos(_,C), C).



%===============================================================================================%
%   Getters for Cell Type :                                                                     %
%===============================================================================================%

% Determining if cell holds an obstacle type
cell_holds_obstacle(State, Pos) :-
    piece_at(State, Pos, Type),
    is_obstacle(Type).

% Determining if cell holds a none type
cell_holds_empty(State, Pos) :-
    piece_at(State, Pos, Type),
    is_none(Type).

% Determining if cell holds an object type
cell_holds_object(State, Pos) :-
    piece_at(State, Pos, Type),
    is_object(Type).
