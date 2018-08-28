#!/usr/bin/env Rscript

source("laser_maze_functions.r")

usage_instructions <- c("usage: laser_maze.R <maze_file> <output_file> [--generate <X> <Y> <N>]\n",
			"       --generate: create a maze <X> by <Y> squares, with <N> mirrors")

args <- commandArgs(trailingOnly = TRUE)
maze_file <- args[1]
output_file <- args[2]

if(length(args) < 2 | length(args) > 6) {
	stop(usage_instructions)
}

if(sum(grepl("--generate", args)) == 1) {
	if(grep("--generate", args) == length(args)) {
		maze_table <- generate_maze()
	} else {
		x_max <- as.numeric(args[grep("--generate", args) + 1])
		y_max <- as.numeric(args[grep("--generate", args) + 2])
		mirrors <- as.numeric(args[grep("--generate", args) + 3])
		maze_table <- generate_maze(x_max, y_max, mirrors)
	}
} else {
	maze_table <- read_maze(maze_file)
}

player <- create_player(maze_table)
check_maze(maze_table)
write_maze(maze_table, maze_file)

if(nrow(get_mirrors(maze_table)) > 0) {
	max_moves <- nrow(get_mirrors(maze_table)) * 2
	while(player$beams < max_moves) {
		player <- fire_beam(player,
				    maze_table,
				    output_file)
	}
	recursion_reached(output_file)
} else {
	fire_beam(player,
		  maze_table,
		  output_file)
}
