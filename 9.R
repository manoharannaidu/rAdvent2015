# Libraries
library(microbenchmark)

# Input file path
file_path <- "./inputs/9input.txt"

# Helpers
## Empty Graph
make_graph <- function() {
  list(nodes = list())
}

## Add a node to the Graph
add_node <- function(graph, node) {
  if (!node %in% names(graph$nodes)) {
    graph$nodes[[node]] <- list()
  }
  graph
}

## Add an egde to the Graph
add_edge <- function(graph, node1, node2, weight = 0) {
  if (!node1 %in% names(graph$nodes)) {
    add_node(graph, node1)
  }
  if (!node2 %in% names(graph$nodes)) {
    add_node(graph, node2)
  }
  graph$nodes[[node1]][[node2]] <- weight
  graph$nodes[[node2]][[node1]] <- weight
  graph
}

## Find the closest node not in a given list
closest_node_notin_list <- function(graph, src, l) {
  dists <- sort(unlist(graph$nodes[[src]]))
  names(dists[min(which(!(names(dists) %in% names(l))))])
}

## Find the length of the shortest path covering all nodes in the graph starting from a given source
find_shortest_loop_from_src <- function(graph, src) {
  visited <- list()
  visited[[src]] <- T
  distances <- list()
  while (T) {
    if (sum(unname(unlist(visited))) == length(names(graph$nodes))) {
      break
    }
    closest_node <- closest_node_notin_list(graph, src, visited)
    if (!closest_node %in% visited) {
      visited[[closest_node]] <- T
      distances[[closest_node]] <- graph$nodes[[src]][[closest_node]]
      src <- closest_node
    }
  }
  sum(unlist(distances))
}

# Part 1
part1 <- function(file_path) {
  # Parse txt file to Graph
  G <- Reduce(function(g, e)
    add_edge(g, e[1], e[3], as.numeric(e[5])),
    strsplit(readLines(file_path, n = -1L), " "),
    init = make_graph())
  min(sapply(names(G$nodes), FUN = find_shortest_loop_from_src, graph =
               G))
}

# Part 2
part2 <- function(file_path) {
  # Parse txt file to Graph
  G <- Reduce(function(g, e)
    add_edge(g, e[1], e[3], -as.numeric(e[5])),
    strsplit(readLines(file_path, n = -1L), " "),
    init = make_graph())
  -min(sapply(names(G$nodes), FUN = find_shortest_loop_from_src, graph = G))
}

# Run
cat("The solution of Part 1 is: ", part1(file_path))
cat("The solution of Part 2 is: ", part2(file_path))

# Benchmarking
microbenchmark(
  part1 = part1(file_path),
  part2 = part2(file_path),
  times = 100
)
