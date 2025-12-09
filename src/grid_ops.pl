:- ['gridArrangement1.pl'].
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

%-------------------------------------------------------------------------------------------%
% Definition for Valid Swaps:
%-------------------------------------------------------------------------------------------%
% -- takes in type cell(type(name), pos(r, c))

% Cannot swap 2 empty cells
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






initial_state(State) :-
    findall(piece(Type, Row, Col), piece(Type, Row, Col), State).

% Get grid dimensions
get_grid_dimensions(Width, Height) :-
    grid_width(Width),
    grid_height(Height).

% Lookup piece type
identify_piece(State, Row, Col, Type) :-
    member(piece(obstacle(Type), Row, Col), State).

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


