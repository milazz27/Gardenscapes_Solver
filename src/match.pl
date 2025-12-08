%-------------------------------------------------------------------------------------------%
% Grid Traversal Methods:
%-------------------------------------------------------------------------------------------%

% Move Right
right_step(Row, Col) :-
    NewCol is Col + 1,
    Col is NewCol,
    in_bounds(Row, Col).

step_down(Row, Col) :-
    NewRow is Row + 1,
    Row is NewRow,
    in_bounds(Row, Col).

%-------------------------------------------------------------------------------------------%
% Verifying Horizontal Matches:
%-------------------------------------------------------------------------------------------%

matching_run()
creates_match(State, Type, Row, Col) :-
