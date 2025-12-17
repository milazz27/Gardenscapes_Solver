% grid.pl --- core definitions + access methods for elements of the grid %

%===============================================================================================%
%   Type Definitions:                                                                           %
%===============================================================================================%

%===============================================================================================%
%   Defining Obstacle Types:                                                                    %
%===============================================================================================%

% obstacle(X) :- X in {leaf, flower, apple, berry, water}
obstacle(leaf).
obstacle(flower).
obstacle(apple).
obstacle(berry).
obstacle(water).

%===============================================================================================%
%   Defining Object Types:                                                                      %
%===============================================================================================%

% object(X) :- X in {lemonade}
object(lemonade).

%===============================================================================================%
%   Defining Empty Types:                                                                       %
%===============================================================================================%

% none(X,A,B) :- X in {empty}
none(empty).

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

% Get Grid Width
get_width(Width) :-
    width(Width).

% Get Grid Height
get_height(Height) :-
    height(Height).

% Get grid dimensions
get_grid_dimensions(Width, Height) :-
    width(Width),
    height(Height).

%===============================================================================================%
%   Getters for Cell Type :                                                                     %
%===============================================================================================%

% Determining if cell holds an obstacle type
cell_holds_obstacle(State, Pos) :-
    piece_at(State, Pos, Type),
    obstacle(Type).

% Determining if cell holds a none type
cell_holds_empty(State, Pos) :-
    piece_at(State, Pos, Type),
    none(Type).

% Determining if cell holds an object type
cell_holds_object(State, Pos) :-
    piece_at(State, Pos, Type),
    object(Type).

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

%===============================================================================================%
%   Defining The Goal State:                                                                    %
%===============================================================================================%

% Base Case: Ground fully traversed
parse_ground(_, [], Ctr, Ctr).

% Do not increment counter if cell doesn't hold object
parse_ground(State, [H|T], Ctr, Fin) :-
    \+ cell_holds_object(State, H),
    parse_ground(State, T, Ctr, Fin).

% Increment counter if another object is hit
parse_ground(State, [H|T], Ctr, Fin) :-
    cell_holds_object(State, H),
    NewCount is Ctr + 1,
    parse_ground(State, T, NewCount, Fin).

% Caller function for goal state check
goal_complete(State) :-
    rows(Rows),
    get_height(H),
    Ind is H - 1,
    nth0(Ind, Rows, Ground),
    parse_ground(State, Ground, 0, Fin),
    total_objects(T),
    T =:= Fin.