:- ['gridArrangement1.pl'].
:- ['grid.pl'].
:- use_module(library(http/json)).

initial_state(State) :-
    findall(piece(Type, Row, Col), piece(Type, Row, Col), State).

% Get grid dimensions
get_grid_dimensions(Width, Height) :-
    grid_width(Width),
    grid_height(Height).

% Lookup piece type
identify_piece(State, Row, Col, Type) :-
    member(piece(Type, Row, Col), State).

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


