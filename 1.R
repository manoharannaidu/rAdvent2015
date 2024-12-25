# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/1input.txt"

# Part 1
part1 <- function(file_path) {
  
  input <- readLines(file_path)
  
  sum(charToRaw(input) == charToRaw("(")) - sum(charToRaw(input) == charToRaw(")"))
  
}

# Part 2
part2 <- function(file_path) {
  
  input <- readLines(file_path)
  
  floor <- 0
  position <- 0
  
  for (char in utf8ToInt(input)) {
    position <- position + 1
    floor <- floor + ifelse(char == 40, 1, -1)
    if (floor < 0) break
  }
  
  return(position)
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
