:- use_module(library(http/json)).
:- consult('../test/testStates2.pl').

% Convert compound piece terms to simple format
piece_to_json(cell(obstacle(Name), pos(Row, Col)), json{type: Name, category: obstacle, row: Row, col: Col}).
piece_to_json(cell(object(Name), pos(Row, Col)), json{type: Name, category: object, row: Row, col: Col}).
piece_to_json(cell(none(Name), pos(Row, Col)), json{type: Name, category: object, row: Row, col: Col}).

% Convert state to JSON format
state_to_json(State, JSON) :-
    findall(JSONPiece,
            (member(Piece, State),
             piece_to_json(Piece, JSONPiece)),
            JSON).

%===============================================================================================%
%   Public Methods:                                                                             %
%===============================================================================================%

% Export with metadata
export_state_to_json(State, Filename) :-
    state_to_json(State, Pieces),
    width(Width),
    height(Height),
    GridData = json{
        width: Width,
        height: Height,
        pieces: Pieces
    },
    open(Filename, write, Stream),
    json_write(Stream, GridData, []),
    close(Stream),
    format('Exported to ~w~n', [Filename]).
