# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/8input.txt"

# Constants
data <- readLines(file_path)

# Helpers
evaluate_lines <- function(line) {
  length(charToRaw(eval(parse(text = line))))
}

evaluate_lines_v2 <- function(line) {
  length(charToRaw(deparse(line)))
}

# Part 1
part1 <- function(data) {
  ## Count total characters in the string
  code_characters <- vapply(data, nchar, integer(1))
  
  ## Count length of evaluated string
  mem_characters <- sapply(data, evaluate_lines)
  
  sum(code_characters) - sum(mem_characters)
}

# Part 2
part2 <- function(data) {
  ## Count total characters in the string
  code_characters <- vapply(data, nchar, integer(1))
  
  ## Count length of evaluated string
  mem_characters <- sapply(data, evaluate_lines_v2)
  
  sum(mem_characters) - sum(code_characters)
}

# Run
cat("The answer for Part 1 is: ", part1(data))
cat("The answer for Part 2 is: ", part2(data))

# Benchmarking
microbenchmark(part1 = part1(data),
               part2 = part2(data),
               times = 100)