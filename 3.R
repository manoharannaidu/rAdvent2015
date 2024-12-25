# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/3input.txt"

# Helper function to process moves
process_moves <- function(directions, movers = 1) {
  # Define movements for each direction
  moves <- list(
    "<" = c(0, -1),
    ">" = c(0, 1),
    "^" = c(-1, 0),
    "v" = c(1, 0)
  )
  
  # Initialize positions for each mover
  positions <- matrix(0, nrow = movers, ncol = 2)
  
  # Pre-allocate visited positions (worst-case size)
  visited <- matrix(NA, nrow = length(directions) + movers, ncol = 2)
  visited[1:movers, ] <- positions
  
  # Process directions
  for (idx in seq_along(directions)) {
    mover <- (idx - 1) %% movers + 1  # Alternate movers if > 1
    positions[mover, ] <- positions[mover, ] + moves[[directions[idx]]]
    visited[idx + movers, ] <- positions[mover, ]
  }
  
  # Count unique visited positions
  nrow(unique(visited, MARGIN = 1))
}

# Part 1: Single mover (Santa)
part1 <- function(file_path) {
  directions <- strsplit(readLines(file_path), "")[[1]]
  process_moves(directions, movers = 1)
}

# Part 2: Two movers (Santa and Robo-Santa)
part2 <- function(file_path) {
  directions <- strsplit(readLines(file_path), "")[[1]]
  process_moves(directions, movers = 2)
}

# Run
cat("Part 1 result:", part1(file_path), "\n")
cat("Part 2 result:", part2(file_path), "\n")

# Benchmarking
microbenchmark(
  part1 = part1(file_path),
  part2 = part2(file_path),
  times = 10
)