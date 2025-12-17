% match.pl -- pattern-match checking methods + tests for cell membership in match %

%===============================================================================================%
%   Check for Match Patterns:                                                                   %
%===============================================================================================%

% Matching pieces (of obstacle type): add to a run.
check_run(PrevType, Type, Pos, Visited, NewVisited, All, All) :-
    obstacle(PrevType),
    obstacle(Type),
    PrevType == Type,
    append(Visited, [Pos], NewVisited).

% Pieces not matching, but complete run can be cached.
check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, UpdatedAllRuns) :-
    PrevType \= Type,
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], UpdatedAllRuns).

% Pieces not matching, but no complete run to be cached.
check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, AllRuns) :-
    PrevType \= Type,
    length(Visited, L),
    L < 3.

% Piece of invalid type, but prev complete run can be cached.
check_run(_, Type, Pos, Visited, [Pos], AllRuns, UpdatedAllRuns):-
    \+ obstacle(Type),
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], UpdatedAllRuns).

% Piece of invalid type, but no complete run to be cached.
check_run(_, Type, Pos, Visited, [Pos], AllRuns, AllRuns) :-
    \+ obstacle(Type),
    length(Visited, L),
    L < 3.

%===============================================================================================%
%   Final Pattern-Match Check:                                                                  %
%===============================================================================================%

% Add any final, complete runs
finalize_runs(Visited, AllRuns, FinalRuns) :-
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], FinalRuns).

% No remaining complete runs to be added 
finalize_runs(Visited, AllRuns, AllRuns) :-
    length(Visited, L),
    L < 3.

%===============================================================================================%
%   Grid-Traversal Logic:                                                                       %
%===============================================================================================%

% Base-Case: used to find matches in a line
traverse_for_matches(_, _, [], Visited, AllRuns, Final) :-
    finalize_runs(Visited, AllRuns, Final).

% Recursive Case: used to find matches in a line
traverse_for_matches(State, PrevType, [CurrentPos | T], Visited, AllRuns, Final) :-
    piece_at(State, CurrentPos, Type),
    check_run(PrevType, Type, CurrentPos, Visited, NewV, AllRuns, NewR),
    traverse_for_matches(State, Type, T, NewV, NewR, Final).

%===============================================================================================%
%   Check for Piece in Match:                                                                   %
%===============================================================================================%

% Use to Check if piece is part of a match in a row
piece_in_match(State, Pos) :-
    rows(Rows),
    get_row(Pos, R),
    nth0(R, Rows, Row),
    piece_at(State, Pos, Type),
    traverse_for_matches(State, Type, Row, [], [], Final),
    member_sublist(Pos, Final).

% Use to check if the piece is part of a Col match
piece_in_match(State, Pos) :-
    cols(Cols),
    get_col(Pos, C),
    nth0(C, Cols, Col),
    piece_at(State, Pos, Type),
    traverse_for_matches(State, Type, Col, [], [], Final2),
    member_sublist(Pos, Final2).