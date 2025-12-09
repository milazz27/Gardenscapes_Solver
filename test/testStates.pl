% Tests for Horizontal Matching:

state_no_matches([
    piece(obstacle(leaf),   1, 0),
    piece(obstacle(water),  1, 1),
    piece(obstacle(apple),  1, 2),
    piece(obstacle(berry),  1, 3),

    piece(obstacle(flower), 2, 0),
    piece(obstacle(berry),  2, 1),
    piece(obstacle(flower), 2, 2),
    piece(obstacle(water),  2, 3),

    piece(obstacle(leaf),   3, 0),
    piece(obstacle(water),  3, 1),
    piece(obstacle(apple),  3, 2),
    piece(obstacle(flower), 3, 3)
]).

state_single_match([
    piece(obstacle(leaf),   1, 0),
    piece(obstacle(apple),  1, 1),
    piece(obstacle(apple),  1, 2),
    piece(obstacle(apple),  1, 3),

    piece(obstacle(flower), 2, 0),
    piece(obstacle(berry),  2, 1),
    piece(obstacle(flower), 2, 2),
    piece(obstacle(water),  2, 3),

    piece(obstacle(leaf),   3, 0),
    piece(obstacle(water),  3, 1),
    piece(obstacle(apple),  3, 2),
    piece(obstacle(flower), 3, 3)
]).

state_four_match([
    piece(obstacle(leaf),    1, 0),
    piece(obstacle(water),   1, 1),
    piece(obstacle(apple),   1, 2),
    piece(obstacle(berry),   1, 3),

    piece(obstacle(flower),  2, 0),
    piece(obstacle(flower),  2, 1),
    piece(obstacle(flower),  2, 2),
    piece(obstacle(flower),  2, 3),

    piece(obstacle(leaf),    3, 0),
    piece(obstacle(water),   3, 1),
    piece(obstacle(apple),   3, 2),
    piece(obstacle(flower),  3, 3)
]).

state_two_matches([
    piece(obstacle(leaf),   3, 0),
    piece(obstacle(leaf),   3, 1),
    piece(obstacle(leaf),   3, 2),

    piece(obstacle(water),  3, 3),
    piece(obstacle(water),  3, 4),
    piece(obstacle(water),  3, 5)
]).

state_middle_match([
    piece(obstacle(leaf),    4, 0),
    piece(obstacle(apple),   4, 1),
    piece(obstacle(apple),   4, 2),
    piece(obstacle(apple),   4, 3),
    piece(obstacle(berry),   4, 4)
]).