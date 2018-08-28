library(data.table)

# MAZE ==========================================================

generate_maze <- function(x_max = sample(c(1:1000), size = 1),
			  y_max = sample(c(1:1000), size = 1),
			  mirrors = sample(c(0:1000), size = 1)) {
	squared <- x_max * y_max
	if(squared == 0) {
		print("Too few tiles. Creating maze with at least 1 tile.")
		x_max <- 1
		y_max <- 1
		squared <- x_max * y_max
	}
	if(mirrors >= squared) {
		print("Too many mirrors. Reducing to number of tiles - 1.")
		mirrors <- squared - 1
	}
	maze_coords <- matrix(1:squared,
			      nrow = y_max)
	directions <- c("N", "S", "E", "W")
	start_direction <- sample(directions, size = 1)
	maze_size <- data.table(row = y_max,
				col = x_max,
				object = "size")
	maze_min <- data.table(row = -1,
			       col = -1,
			       object = "min")
	if(mirrors > 0) {
		mirror_index <- sample(maze_coords, size = mirrors, replace = FALSE)
		mirror_coord_list <- lapply(mirror_index,
    					    function(index) {
    						    as.data.table(which(maze_coords == index, arr.ind = TRUE))
		    			    })
		mirror_coords <- rbindlist(mirror_coord_list) - 1
		mirror_types <- c("/", "\\")
		mirror_coords$object <- sample(mirror_types, size = mirrors, replace = TRUE)
		non_mirror_index <- maze_coords[!maze_coords %in% mirror_index]
		start_index <- non_mirror_index[sample(1:length(non_mirror_index), size = 1)]
		start_coord <- as.data.table(which(maze_coords == start_index, arr.ind = TRUE)) - 1
		start_coord$object <- start_direction
	} else {
		mirror_coords <- NULL
		start_index <- maze_coords[sample(1:length(maze_coords), size = 1)]
		start_coord <- as.data.table(which(maze_coords == start_index, arr.ind = TRUE)) - 1
		start_coord$object <- start_direction
	}
	if(squared == 1) {
		start_coord <- data.table(row = 0,
 					  col = 0,
 					  object = start_direction)
	} 
	maze_table <- rbind(maze_min,
			    maze_size,
			    start_coord,
			    mirror_coords)
	return(maze_table)
}

read_maze <- function(file) {
	maze_table <- fread(file,
			    sep = " ",
			    header = FALSE, 
			    fill = TRUE)
	maze_table[1, ][[3]] <- "size"
	names(maze_table) <- c("row",
			       "col",
			       "object")
	maze_min <- data.table(row = -1,
			       col = -1,
			       object = "min")
	maze_table <- rbind(maze_min,
			    maze_table)
	return(maze_table)
}

write_maze <- function(maze_table,
		       maze_file) {
	maze_table <- maze_table[!maze_table$object %in% "min"]
	maze_table[maze_table$object %in% "size"]$object <- ""
	write.table(maze_table,
		    file = maze_file,
		    row.names = FALSE,
		    quote = FALSE)
}

get_size <- function(maze_table) {
	maze_size <- maze_table[maze_table$object == "size"]
	return(maze_size)
}

get_min <- function(maze_table) {
	maze_min <- maze_table[maze_table$object == "min"]
	return(maze_min)
}

get_start <- function(maze_table) {
	directions <- c("N", "S", "E", "W")
	maze_start <- maze_table[maze_table$object %in% directions]
	return(maze_start)
}

get_mirrors <- function(maze_table) {
	mirror_types <- c("/", "\\")
	maze_mirrors <- maze_table[maze_table$object %in% mirror_types]
	return(maze_mirrors)
}

check_maze <- function(maze_table) {
	maze_min <- get_min(maze_table)
	maze_size <- get_size(maze_table)
	maze_start <- get_start(maze_table)
	maze_mirrors <- get_mirrors(maze_table)
	coords <- c("row", "col")
	maze_start_coords <- paste(maze_start[, ..coords], collapse = " ")
	maze_mirrors_coords <- sapply(1:nrow(maze_mirrors),
				      function(x) {
					      paste(maze_mirrors[x, ][, ..coords], collapse = " ")
				      })
	if(nrow(maze_mirrors) == 0) {
		maze_mirrors <- data.table(row = 0,
					   col = 0,
					   object = NA)
		maze_mirrors_coords <- ""
	}
	maze_object_coords <- c(maze_start_coords, maze_mirrors_coords)
	stopifnot(maze_size$row < 1000,
		  maze_size$col < 1000,
		  maze_start$row < maze_size$row,
		  maze_start$col < maze_size$col,
		  maze_start$row > maze_min$row,
		  maze_start$col > maze_min$col,
		  max(maze_mirrors$row) < maze_size$row,
		  max(maze_mirrors$col) < maze_size$col,
		  min(maze_mirrors$row) > maze_min$row,
		  min(maze_mirrors$col) > maze_min$col,
		  sum(duplicated(maze_object_coords)) == 0)
	print("Maze validated.")
}

# MOVEMENT ==========================================================

create_player <- function(maze_table) {
	position <- get_start(maze_table)
	beams = 0
	player <- list(position = position,
		       beams = beams)
	return(player)
}

change_direction <- function(direction,
			     mirror_type) {
	mirror_types <- c("/", "\\")
	directions <- c("N", "S", "E", "W")
	key <- data.table(mirror = mirror_types,
			  N = c("E", "W"),
			  S = c("W", "E"),
			  E = c("N", "S"),
			  W = c("S", "N"))
	mirror_selection <- key[key$mirror %in% mirror_type]
	new_direction <- mirror_selection[[direction]]
	return(new_direction)
}

get_col_objects <- function(position,
			    maze_table) {
	maze_mirrors <- get_mirrors(maze_table)
	col_mirrors <- maze_mirrors[maze_mirrors$row == position$row]
	maze_min <- get_min(maze_table)
	maze_size <- get_size(maze_table)
	col_objects <- rbind(maze_min, maze_size, col_mirrors)
	return(col_objects)
}

get_row_objects <- function(position,
			    maze_table) {
	maze_mirrors <- get_mirrors(maze_table)
	row_mirrors <- maze_mirrors[maze_mirrors$col == position$col]
	maze_min <- get_min(maze_table)
	maze_size <- get_size(maze_table)
	row_objects <- rbind(maze_min, maze_size, row_mirrors)
	return(row_objects)
}

fire_beam <- function(player,
		      maze_table,
		      output_file) {
	beams <- player$beams + 1
	position <- player$position
	direction <- position$object
	col_objects <- get_col_objects(position, maze_table)
	row_objects <- get_row_objects(position, maze_table)
	if(direction == "N") {
		col_objects <- col_objects[col_objects$col > position$col]
		nearest_object <- col_objects[which.min(col_objects$col)]
		wall_position <- position
		wall_position$col <- nearest_object$col - 1
	} else 
	if(direction == "S") {
		col_objects <- col_objects[col_objects$col < position$col]
		nearest_object <- col_objects[which.max(col_objects$col)]
		wall_position <- position
		wall_position$col <- nearest_object$col + 1
	} else
	if(direction == "E") {
		row_objects <- row_objects[row_objects$row > position$row]
		nearest_object <- row_objects[which.min(row_objects$row)]
		wall_position <- position
		wall_position$row <- nearest_object$row - 1
	} else
	if(direction == "W") {
		row_objects <- row_objects[row_objects$row < position$row]
		nearest_object <- row_objects[which.max(row_objects$row)]
		wall_position <- position
		wall_position$row <- nearest_object$row + 1
	}
	maze_min <- get_min(maze_table)
	maze_size <- get_size(maze_table)
	boundaries <- c(maze_min$object, maze_size$object)
	if(nearest_object$object %in% boundaries) {
		wall_position$object <- "wall"
		coords <- c("row", "col")
		write_data <- c(beams,
				paste(wall_position[, ..coords], collapse = " "))
		print("Boundary detected.")
		print(paste0("Results written to ", output_file))
		write(write_data, file = output_file, sep = "\n")
		write(write_data, "", sep = "\n")
		quit(save = "no")
	} else {
		nearest_object$object <- change_direction(direction = direction,
				  			  mirror_type = nearest_object$object)
	}
	new_player <- list(position = nearest_object,
			   beams = beams)
	return(new_player)
}

recursion_reached <- function(output_file) {
	print("The maximum escape path-length has been exceeded:")
	print("Mirror loop implied.")
	print(paste0("Results written to ", output_file))
	write(-1, file = output_file)
	write(-1, "")
	quit(save = "no")
}
