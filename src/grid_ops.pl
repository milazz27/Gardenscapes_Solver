:- ['gridArrangement1.pl'].
:- ['grid.pl'].
:- use_module(library(http/json)).

initial_state(State) :-
    findall(piece(Type, Row, Col), piece(Type, Row, Col), State).

% Get grid dimensions
get_grid_dimensions(Width, Height) :-
    grid_width(Width),
    grid_height(Height).

