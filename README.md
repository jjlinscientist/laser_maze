======================================================================

laser_maze.R

2018-08-11 Justin J Lin

Coding Challenge

======================================================================

usage: 
```
laser_maze.R <input_file> <output_file> [--generate <X> <Y> <N>]

--generate :	Create a maze of <X> by <Y> squares, with <N> mirrors.
		If no parameters are specified, then a maze of up to 
		1000 x 1000 squares will be generated at random, with
		up to 1000 mirrors.
```
e.g.
```
./laser_maze.R input_maze.txt output_maze.txt --generate 10 10 99
```

======================================================================

dependencies:
```
library(data.table)
source("laser_maze_functions.r")
```
======================================================================

testing resources:

Testing was perfomed using the files below, as well as several
randomly generated mazes, using the --generate flag.
```
input/
├── input_maze.txt . . . . . . . . . . . . . . ### From the prompt ###
└── other_mazes/
    ├── example_generated_maze.txt
    ├── example_infinite_maze.txt
    └── improperly_formatted_mazes/
        ├── duplicate_mirrors_maze.txt
        ├── multiple_starting_position_maze.txt
        ├── object_collision_maze.txt
        └── out-of-bounds_maze.txt
```
