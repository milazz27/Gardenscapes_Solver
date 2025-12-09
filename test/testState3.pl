state3([
    cell(obstacle(flower), pos(0,0)),
    cell(obstacle(water), pos(0,1)),
    cell(obstacle(flower), pos(0,2)),
    cell(obstacle(berry), pos(0,3)),

    cell(obstacle(water), pos(1,0)),
    cell(obstacle(leaf), pos(1,1)),
    cell(obstacle(flower), pos(1,2)),
    cell(obstacle(flower), pos(1,3)),

    cell(obstacle(flower), pos(2,0)),
    cell(obstacle(flower), pos(2,1)),
    cell(obstacle(flower), pos(2,2)),
    cell(obstacle(flower), pos(2,3))
]).

rows([
    [pos(0,0), pos(0,1), pos(0,2), pos(0,3)],
    [pos(1,0), pos(1,1), pos(1,2), pos(1,3)],
    [pos(2,0), pos(2,1), pos(2,2), pos(2,3)]
]).

cols([
    [pos(0, 0), pos(1,0), pos(2,0)],
    [pos(0, 1), pos(1,1), pos(2,1)],
    [pos(0, 2), pos(1,2), pos(2,2)],
    [pos(0, 3), pos(1,3), pos(2,3)]
]).

width(4).
height(3).