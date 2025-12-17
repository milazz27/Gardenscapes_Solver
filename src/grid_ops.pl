% grid_ops.pl -- defines valid movements on grid

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
%   Checks if Swap Generates Match:                                                             %
%===============================================================================================%

generates_match(Pos1, Pos2, State) :-
    full_cell_at(State, Pos1, Type1),
    full_cell_at(State, Pos2, Type2),
    swap_op(State, Type1, Pos1, Type2, Pos2, TempState),
    piece_in_match(TempState, Pos2).

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

col_index_in_bounds(Pos1, Pos2) :-
    width(W),
    get_col(Pos1, C1),
    get_col(Pos2, C2),
    WBound is W - 1,
    C1 =< WBound, C1 >= 0,
    C2 =< WBound, C2 >= 0.

row_index_in_bounds(Pos1, Pos2) :-
    height(H),
    get_row(Pos1, R1),
    get_row(Pos2, R2),
    HBound is H - 1,
    R1 =< HBound, R1 >= 0,
    R2 =< HBound, R2 >= 0.

% Defines a valid Swap: pieces must not be both empty or both objects
% pieces must be adjacent to swap them.
valid_swap(State, Pos1, Pos2) :-
    col_index_in_bounds(Pos1, Pos2),
    row_index_in_bounds(Pos1, Pos2),
    \+ unswappable(State, Pos1, Pos2),
    adjacent(Pos1, Pos2),
    generates_match(Pos1, Pos2, State).

%===============================================================================================%
%   Defining The Swap Operation:                                                                %
%===============================================================================================%

% swap operation
swap_op(State, Type1, Pos1, Type2, Pos2, NewState) :-
    select(cell(Type1, Pos1), State, Temp1),
    select(cell(Type2, Pos2), Temp1, Temp2),
    NewState = [cell(Type1, Pos2), cell(Type2, Pos1) | Temp2].

%swap routine
swap(State, R1, C1, R2, C2, StateNew) :-
    identify_piece(State, R1, C1, Type1),
    identify_piece(State, R2, C2, Type2),
    \+ unswappable(Type1, Type2),
    adjacent(R1, C1, R2, C2),
    swap_op(State, Type1, R1, C1, Type2, R2, C2, StateNew).




