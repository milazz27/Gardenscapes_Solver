:- consult('../src/grid.pl').
% test : public_method(S, [pos(0,0), pos(0,1), pos(0,2), pos(0,3)], F).

state([
    cell(obstacle(flower), pos(0,0)),
    cell(obstacle(flower), pos(0,1)),
    cell(obstacle(flower), pos(0,2)),
    cell(obstacle(flower), pos(0,3))
]).

state1([
    cell(obstacle(flower), pos(0,0)),
    cell(obstacle(flower), pos(0,1)),
    cell(obstacle(flower), pos(0,2)),
    cell(obstacle(water), pos(0,3))
]).

state2([
    cell(obstacle(flower), pos(0,0)),
    cell(obstacle(berry), pos(0,1)),
    cell(obstacle(flower), pos(0,2)),
    cell(obstacle(water), pos(0,3))
]).

state3([
    cell(obstacle(leaf), pos(0,0)),
    cell(obstacle(flower), pos(0,1)),
    cell(obstacle(flower), pos(0,2)),
    cell(obstacle(flower), pos(0,3))
]).

% state5(S), public_method(S, [pos(0,0), pos(0,1), pos(0,2), pos(0,3), pos(0,4)], F).
state4([
    cell(obstacle(leaf), pos(0,0)),
    cell(obstacle(flower), pos(0,1)),
    cell(obstacle(flower), pos(0,2)),
    cell(obstacle(flower), pos(0,3)),
    cell(obstacle(water), pos(0,4))
]).

state5([
    cell(object(lemonade), pos(0,0)),
    cell(object(lemonade), pos(0,1)),
    cell(object(lemonade), pos(0,2)),
    cell(object(lemonade), pos(0,3)),
    cell(object(lemonade), pos(0,4))
]).

testState([
    cell(obstacle(flower), pos(0,0)),
    cell(object(lemonade), pos(0,1)),
    cell(none(empty), pos(0,2))
]).

rows([
    [pos(0,0), pos(0,1), pos(0,2), pos(0,3)]
]).

cols([
    [pos(0, 0)],
    [pos(0, 1)],
    [pos(0, 2)],
    [pos(0, 3)]
]).

% Grid dimensions
grid_width(4). % Number of Cols
grid_height(1). % Number of Rows

step_right(Col, NewCol) :-
    grid_width(Bound),
    Col < Bound - 1,
    NewCol is Col + 1.

% piece type lookup:

% piece is an obstacle
piece_at(State, Pos, Type) :-
    member(cell(obstacle(Type), Pos), State).

% piece is an object
piece_at(State, Pos, Type) :-
    member(cell(object(Type), Pos), State).

% empty cell at position
piece_at(State, Pos, Type) :-
    member(cell(none(Type), Pos), State).


% identifying runs:

% we can add to a run
check_run(PrevType, Type, Pos, Visited, NewVisited, All, All) :-
    is_obstacle(PrevType),
    is_obstacle(Type),
    PrevType == Type,
    append(Visited, [Pos], NewVisited).

check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, UpdatedAllRuns) :-
    PrevType \= Type,
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], UpdatedAllRuns).

check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, AllRuns) :-
    PrevType \= Type,
    length(Visited, L),
    L < 3.

check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, UpdatedAllRuns):-
    \+ is_obstacle(Type),
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], UpdatedAllRuns).

check_run(PrevType, Type, Pos, Visited, [Pos], AllRuns, AllRuns) :-
    \+ is_obstacle(Type),
    length(Visited, L),
    L < 3.

% to be used when we reach end of row
finalize_runs(Visited, AllRuns, FinalRuns) :-
    length(Visited, L),
    L >= 3,
    append(AllRuns, [Visited], FinalRuns).

% finalize case where no more runs need to be added
finalize_runs(Visited, AllRuns, AllRuns) :-
    length(Visited, L),
    L < 3.

traverse(_, _, [], Visited, AllRuns, Final) :-
    finalize_runs(Visited, AllRuns, Final).

traverse(State, PrevType, [CurrentPos | T], Visited, AllRuns, Final) :-
    piece_at(State, CurrentPos, Type),
    check_run(PrevType, Type, CurrentPos, Visited, NewV, AllRuns, NewR),
    traverse(State, Type, T, NewV, NewR, Final).


public_method(State, [H|T], Final) :-
    piece_at(State, H, Type),
    traverse(State, Type, T, [H], [], Final).
