:- use_module(library(http/json)).

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

states_to_json(States, JSONStates) :-
    states_to_json(States, 0, JSONStates).

states_to_json([], _, []).
states_to_json([State | Rest], Step, [JSONState | Tail]) :-
    state_to_json(State, Pieces),
    JSONState = json{
        step: Step,
        pieces: Pieces
    },
    NextStep is Step + 1,
    states_to_json(Rest, NextStep, Tail).

export_solution_to_json(States, Filename) :-
    width(Width),
    height(Height),
    states_to_json(States, JSONStates),
    GridData = json{
        width: Width,
        height: Height,
        states: JSONStates
    },
    open(Filename, write, Stream),
    json_write(Stream, GridData, []),
    close(Stream),
    format('Exported solution (~w states) to ~w~n',
           [len(States), Filename]).
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
