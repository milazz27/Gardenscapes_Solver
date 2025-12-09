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