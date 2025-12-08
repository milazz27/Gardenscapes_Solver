% Define pieces with their initial positions via facts
% (piece_type, Row, Col)
piece(obstacle(water), 0, 0).
piece(object(lemonade), 0, 1).
piece(obstacle(apple), 0, 2).
piece(object(lemonade), 0, 3).

piece(obstacle(leaf), 1, 0).
piece(obstacle(water), 1, 1).
piece(obstacle(apple), 1, 2).
piece(obstacle(apple),1, 3).

piece(obstacle(flower),2, 0).
piece(obstacle(berry),2, 1).
piece(obstacle(flower),2, 2).
piece(obstacle(flower),2, 3).

piece(obstacle(leaf), 3, 0).
piece(obstacle(leaf), 3, 1).
piece(obstacle(water), 3, 2).
piece(obstacle(water), 3, 3).

piece(obstacle(leaf), 4, 0).
piece(obstacle(flower), 4, 1).
piece(obstacle(flower), 4, 2).
piece(obstacle(leaf), 4, 3).

% Grid dimensions
grid_width(4).
grid_height(5).

num_objects(2).