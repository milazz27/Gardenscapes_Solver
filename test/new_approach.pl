:- consult('../src/grid.pl').
:- consult('testStates2.pl').

%===============================================================================================%
%   Misc Helper Methods:                                                                        %
%===============================================================================================%

member_sublist(Pos, List) :-
    member(Sub, List),
    member(Pos, Sub).

set_empty(State, Pos, NewState) :-
    select(cell(_, Pos), State, Temp),
    NewState = [cell(none(empty), Pos) | Temp].

%===============================================================================================%
%   Piece-Type Lookup Methods:                                                                  %
%===============================================================================================%

% piece is an obstacle
piece_at(State, Pos, Type) :-
    member(cell(obstacle(Type), Pos), State).

% piece is an object
piece_at(State, Pos, Type) :-
    member(cell(object(Type), Pos), State).

% empty cell at position
piece_at(State, Pos, Type) :-
    member(cell(none(Type), Pos), State).

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
check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, UpdatedAllRuns):-
    \+ is_obstacle(Type),
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], UpdatedAllRuns).

% Piece of invalid type, but no complete run to be cached.
check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, AllRuns) :-
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

% Base-Case:
traverse(_, _, [], Visited, AllRuns, Final) :-
    finalize_runs(Visited, AllRuns, Final).

% Recursive Case:
traverse(State, PrevType, [CurrentPos | T], Visited, AllRuns, Final) :-
    piece_at(State, CurrentPos, Type),
    check_run(PrevType, Type, CurrentPos, Visited, NewV, AllRuns, NewR),
    traverse(State, Type, T, NewV, NewR, Final).

%===============================================================================================%
%   Find All Matches on Grid:                                                                   %
%===============================================================================================%

parse(State, [], AllMatches, AllMatches).

parse(State, [Head | Tail], AllMatches, Final) :-
    piece_at(State, Head, Type),
    traverse(State, Type, Tail, [Head], [], Matches),
    append(AllMatches, Matches, Update),
    parse(State, Tail, Update, Final).

% returns a 1D list of positions
find_all_matches(State, All) :-
    rows(Rows),
    cols(Cols),
    findall(Run, (member(Row, Rows), parse(State, Row, [], Run)), RowRuns),
    flatten(RowRuns, FlatRowRuns),
    findall(Run2, (member(Col, Cols), parse(State, Col, [], Run2)), ColRuns),
    flatten(ColRuns, FlatColRuns),
    append(FlatRowRuns, FlatColRuns, All).

%===============================================================================================%
%   Remove Matches Methods:                                                                     %
%===============================================================================================%

% Set all pieces that were part of matching groups to empty
remove_matches([], NewState, NewState).

remove_matches([H|T], State, NewState) :-
    set_empty(State, H, New),
    remove_matches(T, New, NewState).

% 2. Carry objects down to fill empty spots (prob go column by column to do this)

apply_gravity(State, Final) :-
    cols(Cols),
    gravity_columns(State, Cols, Final).

gravity_columns(State, [], State).

gravity_columns(State, [ColPositions | Rest], Final) :-
    gravity_column(State, ColPositions, UpdatedState),
    gravity_columns(UpdatedState, Rest, Final).

%===============================================================================================%
%   Public Methods:                                                                             %
%===============================================================================================%

public_method_test(State, [H|T], Final) :-
    piece_at(State, H, Type),
    traverse(State, Type, T, [H], [], Final).

% Check for match in row
public_method_check_move(State, Pos) :-
    rows(Rows),
    get_row(Pos, R),
    nth0(R, Rows, [H1|T1]),
    piece_at(State, Pos, Type),
    traverse(State, Type, T1, [Pos], [], Final),
    member_sublist(Pos, Final).

% check for match in col
public_method_check_move(State, Pos) :-
    cols(Cols),
    get_col(Pos, C),
    nth0(C, Cols, [H2|T2]),
    piece_at(State, Pos, Type),
    traverse(State, Type, T2, [Pos], [], Final2),
    member_sublist(Pos, Final2).



