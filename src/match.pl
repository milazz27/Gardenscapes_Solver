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

% case where we can add a piece to matches
check_move_right(State, GoalType, Row, Col, Matches, NewMatches, []) :-
    identify_piece(State, Row, Col, Type),
    GoalType == Type,
    append([(Row, Col)], Matches, NewMatches).

% case where we need to clear matches (we hit a diff piece type and our anchor piece not in matches)
check_move_right(State, GoalType, Row, Col, Matches, [], [Matches]) :-
    identify_piece(State, Row, Col, Type),
    GoalType \= Type,
    length(Matches, Len),
    Len >= 3 .

% Discard short run (different type AND length < 3)
check_move_right(State, GoalType, Row, Col, Matches, [], []) :-
    identify_piece(State, Row, Col, Type),
    GoalType \= Type,
    length(Matches, Len),
    Len < 3.

%base case for check_row (trying to move beyond the bound)
check_row(_, _, Col, Matches, [Matches]) :-
    col_bound(Bound),
    Col > Bound, !,
    length(Matches, Len),
    Len >= 3.

% Main recursive call for verifying matches in a row.
check_row(State, Row, Col, Matches, AllMatches) :-
    col_bound(Bound),
    Col =< Bound,
    identify_piece(State, Row, Col, Type),
    check_move_right(State, Type, Row, Col, Matches, NewMatches, NewGroup),
    right_step(Row, Col, NewCol),
    check_row(State, Row, NewCol, NewMatches, SubMatches),
    append(NewGroup, SubMatches, AllMatches).

%-------------------------------------------------------------------------------------------%
% Public Calls For Single-Piece Membership in Match.
%-------------------------------------------------------------------------------------------%

creates_match(State, Row, Col, AllMatches) :-
    check_row(State, Row, 0, [], AllMatches),
    piece_in_match(Row, Col, AllMatches).