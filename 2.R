# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/2input.txt"

# Helpers
wrapping_paper <- function(line) {
  sides <- as.integer(unlist(strsplit(line, "x")))
  sides_sorted <- sort(sides)
  sum(2 * combn(sides_sorted, m = 2, FUN = prod)) + (sides_sorted[1] * sides_sorted[2])
}

ribbon <- function(line) {
  sides <- as.integer(unlist(strsplit(line, "x")))
  sides_sorted <- sort(sides)
  prod(sides_sorted) + (2 * (sides_sorted[1] + sides_sorted[2]))
}

# Part 1
part1 <- function(file_path) {
  
  input <- readLines(file_path)
  
  sum(vapply(input, wrapping_paper, numeric(1)))
  
}

# Part 2
part2 <- function(file_path) {
  
  input <- readLines(file_path)
  
  sum(vapply(input, ribbon, numeric(1)))
}

# Run
part1(file_path = file_path)
part2(file_path = file_path)

# Benchmarking
microbenchmark(
  part1 = part1(file_path),
  part2 = part2(file_path),
  times = 100
)