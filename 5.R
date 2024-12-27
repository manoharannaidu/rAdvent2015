# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/5input.txt"

# Constants
data <- readLines(file_path)

# Part 1
naughty_nice <- function(line) {
  if (sum(grepl('[aeiou]', strsplit(line, "")[[1]])) < 3) {
    return(F)
  } else if (!grepl("([a-z])\\1", line)) {
    return(F)
  } else if (grepl("(ab)|(cd)|(pq)|(xy)", line)) {
    return(F)
  } else {
    return(T)
  }
}

part1 <- function(data) {
  sum(vapply(data, naughty_nice, logical(1)))
}

# Part 2
naughty_nice_v2 <- function(line) {
  if (!grepl(".*?([a-z]{2}).*?\\1", line)) {
    return(F)
  } else if (!grepl(".*?([a-z])[a-z]\\1", line)) {
    return(F)
  } else {
    return(T)
  }
}

part2 <- function(data) {
  sum(vapply(data, naughty_nice_v2, logical(1)))
}

# Run
part1(data)
part2(data)

# # Benchmarking
microbenchmark(
  part1 = part1(data),
  part2 = part2(data),
  times = 100
)
