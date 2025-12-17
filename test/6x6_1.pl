width(6).
height(6).

total_objects(2).

rows([
    [pos(0,0), pos(0,1), pos(0,2), pos(0,3), pos(0,4), pos(0,5)],
    [pos(1,0), pos(1,1), pos(1,2), pos(1,3), pos(1,4), pos(1,5)],
    [pos(2,0), pos(2,1), pos(2,2), pos(2,3), pos(2,4), pos(2,5)],
    [pos(3,0), pos(3,1), pos(3,2), pos(3,3), pos(3,4), pos(3,5)],
    [pos(4,0), pos(4,1), pos(4,2), pos(4,3), pos(4,4), pos(4,5)],
    [pos(5,0), pos(5,1), pos(5,2), pos(5,3), pos(5,4), pos(5,5)]
]).

cols([
    [pos(0,0), pos(1,0), pos(2,0), pos(3,0), pos(4,0), pos(5,0)],
    [pos(0,1), pos(1,1), pos(2,1), pos(3,1), pos(4,1), pos(5,1)],
    [pos(0,2), pos(1,2), pos(2,2), pos(3,2), pos(4,2), pos(5,2)],
    [pos(0,3), pos(1,3), pos(2,3), pos(3,3), pos(4,3), pos(5,3)],
    [pos(0,4), pos(1,4), pos(2,4), pos(3,4), pos(4,4), pos(5,4)],
    [pos(0,5), pos(1,5), pos(2,5), pos(3,5), pos(4,5), pos(5,5)]
]).

state([
    cell(object(lemonade), pos(0,0)),
    cell(obstacle(water), pos(0,1)),
    cell(obstacle(berry), pos(0,2)),
    cell(obstacle(water), pos(0,3)),
    cell(obstacle(leaf), pos(0,4)),
    cell(obstacle(apple), pos(0,5)),

    cell(obstacle(apple), pos(1,0)),
    cell(obstacle(leaf), pos(1,1)),
    cell(obstacle(apple), pos(1,2)),
    cell(obstacle(flower), pos(1,3)),
    cell(obstacle(berry), pos(1,4)),
    cell(obstacle(flower), pos(1,5)),

    cell(obstacle(flower), pos(2,0)),
    cell(obstacle(apple), pos(2,1)),
    cell(obstacle(flower), pos(2,2)),
    cell(obstacle(leaf), pos(2,3)),
    cell(obstacle(apple), pos(2,4)),
    cell(obstacle(leaf), pos(2,5)),

    cell(obstacle(leaf), pos(3,0)),
    cell(obstacle(flower), pos(3,1)),
    cell(obstacle(leaf), pos(3,2)),
    cell(obstacle(apple), pos(3,3)),
    cell(obstacle(flower), pos(3,4)),
    cell(obstacle(berry), pos(3,5)),

    cell(obstacle(berry), pos(4,0)),
    cell(obstacle(leaf), pos(4,1)),
    cell(obstacle(apple), pos(4,2)),
    cell(obstacle(flower), pos(4,3)),
    cell(obstacle(leaf), pos(4,4)),
    cell(obstacle(apple), pos(4,5)),

    cell(obstacle(apple), pos(5,0)),
    cell(object(lemonade), pos(5,1)),
    cell(obstacle(water), pos(5,2)),
    cell(obstacle(leaf), pos(5,3)),
    cell(obstacle(apple), pos(5,4)),
    cell(obstacle(flower), pos(5,5))

]).
