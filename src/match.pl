% match.pl -- pattern-match checking methods + tests for cell membership in match %

%===============================================================================================%
%   Check for Match Patterns:                                                                   %
%===============================================================================================%

% Matching pieces (of obstacle type): add to a run.
check_run(PrevType, Type, Pos, Visited, NewVisited, All, All) :-
    is_obstacle(PrevType),
    is_obstacle(Type),
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
    \+ is_obstacle(Type),
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], UpdatedAllRuns).

% Piece of invalid type, but no complete run to be cached.
check_run(_, Type, Pos, Visited, [Pos], AllRuns, AllRuns) :-
    \+ is_obstacle(Type),
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
traverse(_, _, [], Visited, AllRuns, Final) :-
    finalize_runs(Visited, AllRuns, Final).

% Recursive Case: used to find matches in a line
traverse(State, PrevType, [CurrentPos | T], Visited, AllRuns, Final) :-
    piece_at(State, CurrentPos, Type),
    check_run(PrevType, Type, CurrentPos, Visited, NewV, AllRuns, NewR),
    traverse(State, Type, T, NewV, NewR, Final).

%===============================================================================================%
%   Public Methods:                                                                             %
%===============================================================================================%

% Check for match in row
public_method_check_move(State, Pos) :-
    rows(Rows),
    get_row(Pos, R),
    nth0(R, Rows, [_|T1]),
    piece_at(State, Pos, Type),
    traverse(State, Type, T1, [Pos], [], Final),
    member_sublist(Pos, Final).

% check for match in col
public_method_check_move(State, Pos) :-
    cols(Cols),
    get_col(Pos, C),
    nth0(C, Cols, [_|T2]),
    piece_at(State, Pos, Type),
    traverse(State, Type, T2, [Pos], [], Final2),
    member_sublist(Pos, Final2).