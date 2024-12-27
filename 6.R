# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/6input.txt"

# Constants
data <- readLines(file_path)
grid <- matrix(0, 1000, 1000)

# Helpers
preprocess_instructions <- function(data) {
  coord_matches <- regmatches(data, gregexpr("\\d+,\\d+", data))
  coords <- do.call(rbind, lapply(coord_matches, function(x)
    as.integer(unlist(strsplit(
      x, ","
    )))))
  
  actions <- sub(" .*", "", data)
  
  list(
    actions = actions,
    startX = coords[, 1] + 1,
    startY = coords[, 2] + 1,
    endX = coords[, 3] + 1,
    endY = coords[, 4] + 1
  )
}

instructions <- preprocess_instructions(data)

# Part 1
apply_instructions <- function(data, grid, instructions) {
  actions <- instructions$actions
  startX <- instructions$startX
  startY <- instructions$startY
  endX <- instructions$endX
  endY <- instructions$endY
  
  for (i in seq_along(actions)) {
    subgrid <- grid[startX[i]:endX[i], startY[i]:endY[i]]
    if (actions[i] == "turn") {
      if (grepl("off", data[i])) {
        grid[startX[i]:endX[i], startY[i]:endY[i]] <- 0
      } else {
        grid[startX[i]:endX[i], startY[i]:endY[i]] <- 1
      }
    } else if (actions[i] == "toggle") {
      grid[startX[i]:endX[i], startY[i]:endY[i]] <- !subgrid
    }
  }
  grid
}

part1 <- function(data, G, instructions) {
  G <- apply_instructions(data, G, instructions)
  sum(G)
}

# Part 2
apply_instructions_v2 <- function(data, grid, instructions) {
  actions <- instructions$actions
  startX <- instructions$startX
  startY <- instructions$startY
  endX <- instructions$endX
  endY <- instructions$endY
  
  for (i in seq_along(actions)) {
    subgrid <- grid[startX[i]:endX[i], startY[i]:endY[i]]
    if (actions[i] == "turn") {
      if (grepl("off", data[i])) {
        grid[startX[i]:endX[i], startY[i]:endY[i]] <- pmax(subgrid - 1, 0)
      } else {
        grid[startX[i]:endX[i], startY[i]:endY[i]] <- subgrid + 1
      }
    } else if (actions[i] == "toggle") {
      grid[startX[i]:endX[i], startY[i]:endY[i]] <- subgrid + 2
    }
  }
  grid
}

part2 <- function(data, grid, instructions) {
  grid <- apply_instructions_v2(data, grid, instructions)
  
  sum(grid)
}

# Run
part1(data, grid, instructions)
part2(data, grid, instructions)

# Benchmarking
microbenchmark(
  part1 = part1(data, grid, instructions),
  part2 = part2(data, grid, instructions),
  times = 100
)
