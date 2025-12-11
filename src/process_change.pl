% process_change.pl -- applys changes for matches to the grid %

%===============================================================================================%
%   File Inclusions:                                                                            %
%===============================================================================================%

:- consult('grid.pl').
:- consult('match.pl').
:- consult('export.pl').
:- consult('../test/testState3.pl').

%===============================================================================================%
%   Misc Helper Methods:                                                                        %
%===============================================================================================%

% checks if pos is a member of any sublist
member_sublist(Pos, List) :-
    member(Sub, List),
    member(Pos, Sub).

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
%   Find All Matches on Grid:                                                                   %
%===============================================================================================%

% Base Case: Parse line for matches
parse(_, [], AllMatches, AllMatches).

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



