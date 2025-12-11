% grid.pl --- core definitions + access methods for elements of the grid %

%===============================================================================================%
%   Type Definitions:                                                                           %
%===============================================================================================%

%===============================================================================================%
%   Defining Obstacle Types:                                                                    %
%===============================================================================================%

% is_obstacle(X) :- X in {leaf, flower, apple, berry, water}
is_obstacle(leaf).
is_obstacle(flower).
is_obstacle(apple).
is_obstacle(berry).
is_obstacle(water).

% obstacle(X, A, B) :- X in {leaf, flower, apple, berry, water} AND A AND B
obstacle(leaf, pos(_, _)).
obstacle(flower, pos(_, _)).
obstacle(apple, pos(_, _)).
obstacle(berry, pos(_, _)).
obstacle(water, pos(_, _)).

%===============================================================================================%
%   Defining Object Types:                                                                      %
%===============================================================================================%

% is_object(X) :- X in {lemonade}
is_object(lemonade).

% object(X,A,B) :- X in {lemonade} AND A AND B
object(lemonade, pos(_, _)).

%===============================================================================================%
%   Defining Empty Types:                                                                       %
%===============================================================================================%

% is_none(X) :- X in {empty}
is_none(empty).

% none(X,A,B) :- X in {empty} AND A AND B
none(empty, pos(_, _)).

%===============================================================================================%
%   Defining Getters:                                                                           %
%===============================================================================================%

%===============================================================================================%
%   Position/Dimension Getters:                                                                 %
%===============================================================================================%

% Get Row & Column
get_row_col(pos(R, C), R, C).

% Get Row
get_row(pos(R, _), R).

% Get Column
get_col(pos(_,C), C).

% Get grid dimensions
get_grid_dimensions(Width, Height) :-
    grid_width(Width),
    grid_height(Height).

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

%===============================================================================================%
%   Piece-Type Getter Methods:                                                                  %
%===============================================================================================%

% finds full type: type(subtype)
full_cell_at(State, Pos, Kind) :-
    member(cell(Kind, Pos), State).

% piece is an obstacle
piece_at(State, Pos, Type) :-
    member(cell(obstacle(Type), Pos), State).

% piece is an object
piece_at(State, Pos, Type) :-
    member(cell(object(Type), Pos), State).

% empty cell at position
piece_at(State, Pos, Type) :-
    member(cell(none(Type), Pos), State).