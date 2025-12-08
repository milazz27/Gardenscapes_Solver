:- use_module(library(http/json)).
:- ['grid_ops.pl'].

% Convert compound piece terms to simple format
piece_to_json(piece(obstacle(Name), Row, Col), json{type: Name, category: obstacle, row: Row, col: Col}).
piece_to_json(piece(object(Name), Row, Col), json{type: Name, category: object, row: Row, col: Col}).

% Convert state to JSON format
state_to_json(State, JSON) :-
    findall(JSONPiece,
            (member(Piece, State),
             piece_to_json(Piece, JSONPiece)),
            JSON).

% Export with metadata
export_state_to_json(State, Filename) :-
    state_to_json(State, Pieces),
    get_grid_dimensions(Width, Height),
    GridData = json{
        width: Width,
        height: Height,
        pieces: Pieces
    },
    open(Filename, write, Stream),
    json_write(Stream, GridData, []),
    close(Stream),
    format('Exported to ~w~n', [Filename]).

% Helper - export initial state directly
export_initial_state(Filename) :-
    initial_state(State),
    export_state_to_json(State, Filename).