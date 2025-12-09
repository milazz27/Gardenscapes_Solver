:- consult('../src/grid.pl').
:- consult('testState3.pl').
:- consult('../src/export.pl').

%===============================================================================================%
%   Misc Helper Methods:                                                                        %
%===============================================================================================%

% checks if pos is a member of any sublist
member_sublist(Pos, List) :-
    member(Sub, List),
    member(Pos, Sub).

% extracts full type details for a cell at pos
full_cell_at(State, Pos, Kind) :-
    member(cell(Kind, Pos), State).

% Change a cell at pos to be empty and update state
set_empty(State, Pos, NewState) :-
    select(cell(_, Pos), State, Temp),
    NewState = [cell(none(empty), Pos) | Temp].

% Change cell at pos to a new type and update state
update_cell(State, Pos, NewKind, NewState) :-
    select(cell(_, Pos), State, Temp),
    NewState = [cell(NewKind, Pos) | Temp].

% Base Case: get full values of cells
get_column_vals(_, [], []).

% Recursive Case: get full values of cells
get_column_vals(State, [Pos | Rest], [Kind | Tail]) :-
    full_cell_at(State, Pos, Kind),
    get_column_vals(State, Rest, Tail).

% Base Case: Splitting list into empty and full cells
split_empty([], [], []).

% Recursive Case: Splitting list into empty and full cells
split_empty([none(empty) | T], [none(empty) | E], N) :-
    split_empty(T, E, N).

% Recursive Case: Splitting list into empty and full cells
split_empty([X | T], E, [X | N]) :-
    X \= none(empty),
    split_empty(T, E, N).

% Base Case: updating state vals
apply_column_updates(State, [], [], State).

% Recursive Case: updating state vals
apply_column_updates(State, [Pos|Ptail], [Kind|Ktail], FinalState) :-
    update_cell(State, Pos, Kind, TempState),
    apply_column_updates(TempState, Ptail, Ktail, FinalState).

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

% Base-Case: used to find matches in a line
traverse(_, _, [], Visited, AllRuns, Final) :-
    finalize_runs(Visited, AllRuns, Final).

% Recursive Case: used to find matches in a line
traverse(State, PrevType, [CurrentPos | T], Visited, AllRuns, Final) :-
    piece_at(State, CurrentPos, Type),
    check_run(PrevType, Type, CurrentPos, Visited, NewV, AllRuns, NewR),
    traverse(State, Type, T, NewV, NewR, Final).

%===============================================================================================%
%   Find All Matches on Grid:                                                                   %
%===============================================================================================%

% Base Case: Parse line for matches
parse(State, [], AllMatches, AllMatches).

% Recursive Case: Parse line for matches
parse(State, [Head | Tail], AllMatches, Final) :-
    piece_at(State, Head, Type),
    traverse(State, Type, Tail, [Head], [], Matches),
    append(AllMatches, Matches, Update),
    parse(State, Tail, Update, Final).

% returns a 1D list of positions that are members of matching groups
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

% Base Case: Set all pieces that were part of matching groups to empty
remove_matches([], NewState, NewState).

% Recursive Case: Set all pieces that were part of matching groups to empty
remove_matches([H|T], State, NewState) :-
    set_empty(State, H, New),
    remove_matches(T, New, NewState).

%===============================================================================================%
%   Gravity Application:                                                                        %
%===============================================================================================%

% Rearrange columns so that objects pushed to the bottom
gravity_column(State, ColumnPositions, NewState) :-
    get_column_vals(State, ColumnPositions, Values),
    split_empty(Values, Empties, NonEmpties),
    append(Empties, NonEmpties, NewValues),
    apply_column_updates(State, ColumnPositions, NewValues, NewState).

% Base Case: applying gravity to columns
gravity_columns(State, [], State).

% Recursive Case: applying gravity to the columns
gravity_columns(State, [ColPositions | Rest], Final) :-
    gravity_column(State, ColPositions, UpdatedState),
    gravity_columns(UpdatedState, Rest, Final).

% Apply Gravity to the entire State instance
apply_gravity(State, Final) :-
    cols(Cols),
    gravity_columns(State, Cols, Final).

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

% state(State), test_empty_cells(State, 'out.json').
test_empty_cells(State, Filename1, Filename2) :-
    export_state_to_json(State, Filename1),
    find_all_matches(State, All),
    remove_matches(All, State, NewState),
    export_state_to_json(NewState, Filename2).

% state3(State), test_gravity(State, 'p1.json', 'p2.json', 'p3.json').
test_gravity(State, File1, File2, File3) :-
    export_state_to_json(State, File1),
    find_all_matches(State, All),
    remove_matches(All, State, NewState),
    export_state_to_json(NewState, File2),
    apply_gravity(NewState, Final),
    export_state_to_json(Final, File3).



