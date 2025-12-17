## Intro to Logic-Based AI Final Project (Fall 2025)
**Milena (Mila) Zlatkovic**

### Overview:

For this project, my goal was to begin to formalize an approach to the pattern-matching game, *Gardenscapes.* To do this, I approached the problem through programming in Prolog; the fundamental piece of this approach being the interaction between a set of rules (horn clauses) and queries accross these.

Another fundamental feature of Prolog that is central to this application is backtracking. This is integral to how solutions are generated. For this game, a *correct* solution is one that allows for the goal condition to follow. So, while a move may be valid, it does not guarantee that it will bring the state a step closer to the goal.

### Defining States and Goal

In this project I defined the grid as being made up of cells. Each cell has a type and a position.

``` Prolog
% defining a cell holding type object at Pos(0,0)
cell(object(lemonade), pos(0,0)).
```
Types represent obstacles (these are the pieces that can be matched), objects (represent the piece that should reach the ground in the goal condition), or none. These are defined formally as follows:

![definitions](https://latex.codecogs.com/svg.image?
\begin{aligned}
\text{obstacle}(X) &\rightarrow X \in \{\text{leaf},\text{apple},\text{berry},\text{water},\text{flower}\} \\
\\
\text{object}(X) &\rightarrow X \in \{\text{lemonade}\} \\
\\
\text{none}(X) &\rightarrow X \in \{\text{empty}\}
\end{aligned})




### Core Solver Approach

### Program Flow


### Demo

![Solver Functionality](solution1.gif)

### File Organization and Running

## Further Steps

![Gravity Functionality](match+grav_ex.gif)

![Solver Functionality](solution1.gif)

This project will be aimed at building an AI Agent to solve (my favorite mobile game) 'Gardenscapes.' To do this I will employ a planning approach in Prolog. 

To be specific, I will be abstacting levels of the game where the goal is to match blocks to clear the way for objects at the top of the grid to reach the 'ground.'

lecture 5 has some applicable stuff.

## References

- Ivan Bratko. *Prolog Programming for Artificial Intelligence-- Fourth Edition*. Pearson, 2012.
- Gardenscapes Game: https://play.google.com/store/apps/details?id=com.playrix.gardenscapes
- GIF Creation Tool: https://ezgif.com/