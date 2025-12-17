% solver.pl -- this is where solving functionality is implemented

%===============================================================================================%
%   File Inclusions:                                                                            %
%===============================================================================================%

:- ['grid_ops.pl'].
:- ['grid.pl'].
:- ['match.pl'].
:- ['export.pl'].
:- ['process_change.pl'].
:- ['../test/6x6_1.pl'].

%===============================================================================================%
%   Discover All Viable Swaps                                                                   %
%===============================================================================================%

%===============================================================================================%
%   All Viable Swaps @ Cell level:                                                              %
%===============================================================================================%

% finds all neighboring cells (no bounds checking)
all_adjacent(pos(R,C), [
                pos(R, C1),
                pos(R, C2),
                pos(R1, C),
                pos(R2, C)]
    ) :-
    C1 is C + 1,
    C2 is C - 1,
    R1 is R + 1,
    R2 is R - 1.

% filters all adjacent pairings by whether they generate a match
valid_swaps_by_cell(State, Pos, Valid) :-
    all_adjacent(Pos, Potential),
    findall(
        [Pos, I],
        (
            member(I, Potential),
            valid_swap(State, Pos, I)
        ),
        Valid
    ).

% test
test_cell(Pos, Out) :-
    state44(State),
    valid_swaps_by_cell(State, Pos, Out).

%===============================================================================================%
%   All Viable Swaps @ Grid Level:                                                              %
%===============================================================================================%

ordered_pair(pos(R1,C1), pos(R2,C2), [pos(R1,C1), pos(R2,C2)]) :-
    (R1 < R2 ; (R1 = R2, C1 =< C2)), !.

ordered_pair(A, B, [B, A]).

% finds all swaps on grid and ensures no duplicates and returns ordered version
all_swaps_on_grid(State, All) :-
    rows(Rows),
    findall(
        Pair,
        (
            member(Row, Rows),
            member(Pos, Row),
            valid_swaps_by_cell(State, Pos, Swaps),
            member([Pos, A], Swaps),
            ordered_pair(Pos, A, Pair)
        ),
        Raw
    ),
    sort(Raw, All).

test_grid(All) :-
    state44(State),
    all_swaps_on_grid(State, All).

%===============================================================================================%
%   Evaluation Driver:                                                                          %
%===============================================================================================%

apply_swap(State, [Pos1, Pos2], NewState) :-
    full_cell_at(State, Pos1, Type1),
    full_cell_at(State, Pos2, Type2),
    swap_op(State, Type1, Pos1, Type2, Pos2, NewState).

% solve(State, PotentialSwaps, Progress, NewState)
solve(State, _, Plan, Plan, Progress, Res) :-
    goal_complete(State),
    reverse([State | Progress], Res).

solve(State, Swaps, Plan, FullPlan, Progress, Res) :-
    member(Swap, Swaps),   
    apply_swap(State, Swap, Swapped),    
    process(Swapped, NewState),          
    \+ member(Swapped, Progress),       
    \+ member(NewState, Progress),       
    append(Progress, [Swapped, NewState], NewProgress),
    append(Plan, [Swap], UpdatedPlan),
    all_swaps_on_grid(NewState, NewSwaps),
    solve(NewState, NewSwaps, UpdatedPlan, FullPlan, NewProgress, Res).

solver(Sol) :-
    state(State),
    all_swaps_on_grid(State, AllSwaps),
    solve(State, AllSwaps,[], Sol, [State], Res),
    export_solution_to_json(Res, '../display/solution.json').