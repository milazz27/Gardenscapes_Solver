
:- ['grid.pl'].
:- use_module(library(http/json)).

%===========================================================================================%
%  Definition for Adjacency:                                                                %
%===========================================================================================%

% check right 
adjacent(Pos1, Pos2) :-
    get_row_col(Pos1, Row1, Col1),
    get_row_col(Pos2, Row2, Col2),
    Row1 =:= Row2,
    Col2 =:= Col1 + 1.

% check left
adjacent(Pos1, Pos2) :-
    get_row_col(Pos1, Row1, Col1),
    get_row_col(Pos2, Row2, Col2),
    Row1 =:= Row2,
    Col1 =:= Col2 + 1.

% check up
adjacent(Pos1, Pos2) :-
    get_row_col(Pos1, Row1, Col1),
    get_row_col(Pos2, Row2, Col2),
    Col1 =:= Col2,
    Row2 =:= Row1 + 1.

%check down
adjacent(Pos1, Pos2) :-
    get_row_col(Pos1, Row1, Col1),
    get_row_col(Pos2, Row2, Col2),
    Col1 =:= Col2,
    Row1 =:= Row2 + 1.

%===============================================================================================%
%   Definitions for Valid Swaps:                                                                %
%===============================================================================================%

% Cannot swap 2 empty cells
unswappable(State, Pos1, Pos2) :- 
    cell_holds_empty(State, Pos1),
    cell_holds_empty(State, Pos2),
    !.

% Cannot swap 2 objects
unswappable(State, Pos1, Pos2) :- 
    cell_holds_object(State, Pos1),
    cell_holds_object(State, Pos2).

% Defines a valid Swap: pieces must not be both empty or both objects
% pieces must be adjacent to swap them.
valid_swap(State, Pos1, Pos2) :-
    /+ unswappable(State, Pos1, Pos2),
    adjacent(Pos1, Pos2).




initial_state(State) :-
    findall(piece(Type, Row, Col), piece(Type, Row, Col), State).

% Get grid dimensions
get_grid_dimensions(Width, Height) :-
    grid_width(Width),
    grid_height(Height).


% swap operation
swap_op(State, Type1, R1, C1, Type2, R2, C2, NewState) :-
    select(piece(Type1, R1, C2), State, TempState),
    select(piece(Type2, R2, C2), TempState, TempState2),
    NewState = [piece(Type1, R2, C2), piece(Type2, R1, C1)].

%swap routine
swap(State, R1, C1, R2, C2, StateNew) :-
    identify_piece(State, R1, C1, Type1),
    identify_piece(State, R2, C2, Type2),
    \+ unswappable(Type1, Type2),
    adjacent(R1, C1, R2, C2),
    swap_op(State, Type1, R1, C1, Type2, R2, C2, StateNew).


