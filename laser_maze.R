#!/usr/bin/env Rscript

source("laser_maze_functions.r")

usage_instructions <- c("usage: laser_maze.R <maze_file> <output_file> [--generate <X> <Y> <N>]\n",
			"       --generate: create a maze <X> by <Y> squares, with <N> mirrors")

# Collect command line arguments =================================================================

args <- commandArgs(trailingOnly = TRUE)
maze_file <- args[1]
output_file <- args[2]

# Generate maze if necessary =====================================================================

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
	check_maze(maze_table)
	write_maze(maze_table, maze_file)
} else {
	maze_table <- read_maze(maze_file)
}

# Move through the maze ==========================================================================

player <- create_player(maze_table)

if(nrow(get_mirrors(maze_table)) > 0) {
	while(1 > 0) {
		check_history(player,
			      output_file)
		player <- fire_beam(player,
				    maze_table,
				    output_file)
	}
} else {
	fire_beam(player,
		  maze_table,
		  output_file)
}
