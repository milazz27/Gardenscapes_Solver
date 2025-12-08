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

ctr_increment(Ctr, NewCtr) :-
    NewCtr is Ctr + 1.

% base case:
horizontal_match_check(State, Row, Col, Ctr, Matches) :-
    Ctr >= 3,
    member((Row, Col), Matches).

% add another piece to matches -- processes after a step is taken
check_move_right(State, GoalType, Row, Col, Ctr, OGMatches, Matches) :-
    identify_piece(State, Row, Col, Type),
    Type = GoalType,
    append([(Row, Col)], OGMatches, Matches)
    ctr_increment(Ctr, NewCtr),
    Ctr is NewCtr.

%checks if run is over (hit a different piece) and but valid match found
check_move_right(State, GoalType, Row, Col, Ctr, OGMatches, Matches) :-
    identify_piece(State, Row, Col, Type),
    +\ Type = GoalType,
    horizontal_match_check(State, Row, Col, Ctr, Matches).

%checks if run is over (hit a different piece) and but valid match found
check_move_right(State, GoalType, Row, Col, Ctr, OGMatches, Matches) :-
    identify_piece(State, Row, Col, Type),
    +\ Type = GoalType,
    horizontal_match_check(State, Row, Col, Ctr, OGMatches).

% checks if run is over (at the bound)
check_move_right(State, GoalType, Row, Col, Ctr, OGMatches, Matches) :-
    col_bound(Bound),
    Col =:= Bound,
    horizontal_match_check(State, Row, Col, Ctr, OGMatches).

creates_horizontal_match(State, Type, Row, Col) :-

