# Libraries
library(openssl)
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
  if (start > end)
    return(NA) # Handle empty ranges
  
  secrets <- paste0(secret_key, start:end)
  hashes <- md5(secrets)
  
  matches <- substr(hashes, 1, num_chars_find) == string_find
  if (any(matches)) {
    return(start + which(matches == T)[1] - 1)
  } else {
    return(NA)
  }
}

parallelize <- function(secret_key,
                        num_cores = detectCores(logical = FALSE) - 1,
                        batch_size = 10000,
                        num_chars_find = 5,
                        string_find = "00000") {
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("secret_key", "find_smallest_num_range", "md5")) # Export openssl library
  
  result <- NULL
  found <- FALSE
  start <- 1
  
  while (!found) {
    ranges <- splitIndices(nx = num_cores * batch_size, ncl = num_cores)
    ranges <- lapply(ranges, function(x)
      c(start + min(x) - 1, start + max(x) - 1))
    
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
