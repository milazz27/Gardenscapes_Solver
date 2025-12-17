## Intro to Logic-Based AI Final Project (Fall 2025)
**Milena (Mila) Zlatkovic**

![Solver Functionality](solution1.gif)

## Overview:

For this project, my goal was to begin to formalize an approach to solve the pattern-matching game, *Gardenscapes.* To do this, began to build a solver in Prolog. This programming language naturally allows for the mechanics of the game due to its logic-based design. The programmer is able to set up a series of definitions and rules (by way of horn clauses) and then query over those to discover valid applications. In this project, rules and definitions are set up for how the grid is structured and what conditions any alterations must satisfy. 


## Defining States and Goal

In this project I defined the grid as being made up of cells. Each cell has a type and a position. For example:

``` Prolog
% defining a cell holding type object at Pos(0,0)
cell(object(lemonade), pos(0,0)).
```
Types represent **obstacles** (these are the pieces that can be matched and removed to advance the grid further), **objects** (represent the piece that should reach the ground in the goal condition), or **none**. They are defined formally as follows:

![](type_definitions.png)

*Subtypes are defined this way to enable additional features to be added to the program more smoothly.*

The initial state of a grid must be defined (see example below) in a prolog file and be loaded for [*solver.pl*](../src/solver.pl) before execution. 

In addition to a list of cells *(types and positions)*, this file sets definitions on dimensions, number of goal objects, and positions by rows and columns for cleaner traversal.

```Prolog
width(4).
height(4).
total_objects(2).

% list of cell definitions for a given state
state44([
    cell(object(lemonade), pos(0,0)),
    cell(obstacle(water), pos(0,1)),
    cell(object(lemonade), pos(0,2)),
    cell(obstacle(water), pos(0,3)),

    cell(obstacle(apple), pos(1,0)),
    cell(obstacle(leaf), pos(1,1)),
    cell(obstacle(apple), pos(1,2)),
    cell(obstacle(flower), pos(1,3)),

    cell(obstacle(flower), pos(2,0)),
    cell(obstacle(apple), pos(2,1)),
    cell(obstacle(flower), pos(2,2)),
    cell(obstacle(leaf), pos(2,3)),

    cell(obstacle(leaf), pos(3,0)),
    cell(obstacle(flower), pos(3,1)),
    cell(obstacle(leaf), pos(3,2)),
    cell(obstacle(apple), pos(3,3))
]).

% list of row positions
rows([
    [pos(0,0), pos(0,1), pos(0,2), pos(0,3)],
    [pos(1,0), pos(1,1), pos(1,2), pos(1,3)],
    [pos(2,0), pos(2,1), pos(2,2), pos(2,3)],
    [pos(3,0), pos(3,1), pos(3,2), pos(3,3)]
]).

% list of column positions
cols([
    [pos(0,0), pos(1,0), pos(2,0), pos(3,0)],
    [pos(0,1), pos(1,1), pos(2,1), pos(3,1)],
    [pos(0,2), pos(1,2), pos(2,2), pos(3,2)],
    [pos(0,3), pos(1,3), pos(2,3), pos(3,3)]
]).
```

## Mechanics of This Approach

At its core, this solver operates by attempting to resolve a proposed move by *querying* the defined set of rules. A move is deemed valid if there is a path through the clauses that evaluates to true. Another core element of the solver is the inherent *backtracking* done by Prolog. This is integral to this application as I am only interested in valid swaps that will lead to the goal condition. If any swap leads to a dead-end (no further swaps possible and the grid has not reached its goal condition) the program continually goes back to previous options until all are exhausted or a valid plan is found. 

**The current algorithm runs as follows:**
- Given a current state for the grid, identify cells that are swappable.
  - To be swappable, types must be compatible, adjacent, and must generate a match. This is defined in the program as follows:
  
```Prolog
% Cannot swap 2 empty cells
unswappable(State, Pos1, Pos2) :- 
    cell_holds_empty(State, Pos1),
    cell_holds_empty(State, Pos2),
    !.

% Cannot swap 2 objects
unswappable(State, Pos1, Pos2) :- 
    cell_holds_object(State, Pos1),
    cell_holds_object(State, Pos2).

% Definitions for bounds on column index
col_index_in_bounds(Pos1, Pos2) :-
    width(W),
    get_col(Pos1, C1),
    get_col(Pos2, C2),
    WBound is W - 1,
    C1 =< WBound, C1 >= 0,
    C2 =< WBound, C2 >= 0.

% Definitions for bounds on row index
row_index_in_bounds(Pos1, Pos2) :-
    height(H),
    get_row(Pos1, R1),
    get_row(Pos2, R2),
    HBound is H - 1,
    R1 =< HBound, R1 >= 0,
    R2 =< HBound, R2 >= 0.

% Defines a valid Swap: pieces must not be both empty or both objects
% pieces must be adjacent and in-bounds to swap them.
valid_swap(State, Pos1, Pos2) :-
    col_index_in_bounds(Pos1, Pos2),
    row_index_in_bounds(Pos1, Pos2),
    \+ unswappable(State, Pos1, Pos2),
    adjacent(Pos1, Pos2),
    generates_match(Pos1, Pos2, State).
```
- A valid swap is then applied (tracked in a **copy** of the new state) and the grid processes the change by removing all matches and applying the *gravity* functionality to move pieces from above down.**
  
![Gravity Functionality](match+grav_ex.gif)

- The grid is then checked for the goal condition.
  - If the goal is met, the program is able to export a list of the intermediate states that led to the result to a file for display formatting. Additionally, a list of the swaps made will be displayed in the terminal.
  - If the goal is not met and there are further swaps to be applied, it continues.
  - If the goal is not met and no more swaps can be applied it **backtracks** and attempts to find a different plan to reach the goal.
- In the case where there is no solution the program will return false.


## Demo

The following animation is put together from the output of intermediate states found by the solver. They have been exported to a json file and interpreted visually by python scripts in [the display directory](../display/).

![6x6 grid example](less_turbulent.gif)

**Process to Obtain the Result Above:**

[Sample Program Run](walkthrough.mp4)


## Further Steps

I hope to extend my work on this project further, particularly in the approach to selecting the next move to attempt. Currently, the algorithm takes a naive approach and must rely heavily on backtracking (becomes more difficult on larger grids!). There are additional conclusions that solver can make by examining the states in different ways that could aid in making its choices better informed. I would also like to introduce additional features to the program including:
- New piece generation at the top of the grid to replace empty cells.
- Power-ups (similar to those implemented in the original game) that clear a larger segment of the grid.

## References

- Ivan Bratko. *Prolog Programming for Artificial Intelligence-- Fourth Edition*. Pearson, 2012.
- Gardenscapes Game: https://play.google.com/store/apps/details?id=com.playrix.gardenscapes
- GIF Creation Tool: https://ezgif.com/