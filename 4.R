# Libraries
library(digest)
library(parallel)

# Input file path
file_path <- "./inputs/4input.txt"

# Constants
secret_key = readLines(file_path)

# Helpers
find_smallest_num_range <- function(start,
                                    end,
                                    secret_key,
                                    num_chars_find = 5,
                                    string_find = "00000") {
  for (i in start:end) {
    secret <- paste0(secret_key, i)
    hash <- digest(secret, algo = "md5", serialize = FALSE)
    if (substr(hash, 1, num_chars_find) == string_find) {
      return(i)
    }
  }
  return(NA)
}

parallelize <- function(secret_key,
                        num_cores = detectCores(logical = F),
                        batch_size = 10000,
                        num_chars_find = 5,
                        string_find = "00000") {
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("secret_key", "digest", "find_smallest_num_range"))
  
  result <- NULL
  found <- FALSE
  start <- 0
  
  while (!found) {
    ranges <- lapply(1:num_cores, function(core_id) {
      c(start + ((core_id - 1) * batch_size), start + (core_id * batch_size) - 1)
    })
    results <- parSapply(cl, ranges, function(range) {
      find_smallest_num_range(range[1], range[2], secret_key, num_chars_find, string_find)
    })
    results <- unlist(results)
    if (any(!is.na(results))) {
      found <- TRUE
      result <- min(results, na.rm = TRUE)
    } else {
      start <- start + num_cores * batch_size
    }
  }
  stopCluster(cl)
  return(result)
}

# Run
## Part 1
start_time <- Sys.time()
cat("The smallest number for part1 is:",
    parallelize(secret_key),
    "\n")
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken

## Part 2
start_time <- Sys.time()
cat(
  "The smallest number for part2 is:",
  parallelize(
    secret_key,
    batch_size = 100000,
    num_chars_find = 6,
    string_find = "000000"
  ),
  "\n"
)
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken
