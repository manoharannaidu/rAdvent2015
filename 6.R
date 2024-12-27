# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/6input.txt"

# Constants
data <- readLines(file_path)
grid <- matrix(0, 1000, 1000)

# Helpers
get_coords <- function(line) {
  words <- strsplit(line, " ")[[1]]
  coord_idx <- grep("[0-9]", words)
  coords <- as.integer(unlist(strsplit(words[coord_idx], ","))) + 1
  return(coords)
}

# Part 1
process_instructions <- function(line, grid) {
  coords <- get_coords(line)
  startX <- coords[1]
  startY <- coords[2]
  endX <- coords[3]
  endY <- coords[4]
  if (grepl("turn off", line, fixed = T)) {
    grid[startX:endX, startY:endY] <- 0
  } else if (grepl("turn on", line, fixed = T)) {
    grid[startX:endX, startY:endY] <- 1
  } else {
    grid[startX:endX, startY:endY] <- !grid[startX:endX, startY:endY]
  }
  grid
}

part1 <- function(data, G) {
  for (line in data) {
    G <- process_instructions(line, G)
  }
  sum(G)
}

# Part 2
process_instructions_v2 <- function(line, grid) {
  coords <- get_coords(line)
  startX <- coords[1]
  startY <- coords[2]
  endX <- coords[3]
  endY <- coords[4]
  if (grepl("turn off", line, fixed = T)) {
    grid[startX:endX, startY:endY] <- pmax(grid[startX:endX, startY:endY] - 1, 0)
  } else if (grepl("turn on", line, fixed = T)) {
    grid[startX:endX, startY:endY] <- grid[startX:endX, startY:endY] + 1
  } else {
    grid[startX:endX, startY:endY] <- grid[startX:endX, startY:endY] + 2
  }
  grid
}

part2 <- function(data, G) {
  for (line in data) {
    G <- process_instructions_v2(line, G)
  }
  sum(G)
}


# Run
part1(data, grid)
part2(data, grid)

# Benchmarking
microbenchmark(
  part1 = part1(data, grid),
  part2 = part2(data, grid),
  times = 5
)
