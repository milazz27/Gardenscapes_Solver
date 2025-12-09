:- consult('../test/testStates.pl').
:- ['grid_ops.pl'].
:- ['grid.pl'].

%-------------------------------------------------------------------------------------------%
% Grid Traversal Methods:
%-------------------------------------------------------------------------------------------%

% Move Right
right_step(Row, Col, NewCol) :-
    NewCol is Col + 1,
    in_bounds(Row, NewCol).

step_down(Row, NewRow, Col) :-
    NewRow is Row + 1,
    in_bounds(NewRow, Col).

%-------------------------------------------------------------------------------------------%
% Verifying Horizontal Matches:
%-------------------------------------------------------------------------------------------%

% checks if piece is contained within a run of same pieces
piece_in_match(Row, Col, Matches) :-
    member(Sub, Matches),
    member((Row, Col), Sub).

%-------------------------------------------------------------------------------------------%
% Verifying Horizontal Matches:
%-------------------------------------------------------------------------------------------%

% Test Run:  state_four_match(State), row_traversal(State, 2, 0, V).
row_traversal(_, _, Col, Visited, Visited) :-
    Col >= 4.

row_traversal(State, Row, Col, Visited, Final) :-
    Col < 3,
    append(Visited, [(Row, Col)], Updated),
    right_step(Row, Col, NewCol),
    row_traversal(State, Row, NewCol, Updated, Final).



%-------------------------------------------------------------------------------------------%
% Public Calls For Single-Piece Membership in Match.
%-------------------------------------------------------------------------------------------%

creates_match(State, Row, Col, AllMatches) :-
    check_row(State, Row, 0, [], AllMatches),
    piece_in_match(Row, Col, AllMatches).