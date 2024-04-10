#' Generate weighted adjacency matrix of a graph with given number of vertices
#'
#' @param n Number of vertices
#' @param num_zeros Number of zeros to add randomly
#' @return A weighted adjacency matrix representing the random graph
#' @export
generate_random_adjacency_matrix <- function(n, num_zeros = NULL) {
  symmetric_matrix <- matrix(sample(1:10, n * n, replace = TRUE), nrow = n)
  symmetric_matrix <- (symmetric_matrix + t(symmetric_matrix)) / 2
  diag(symmetric_matrix) <- Inf

  if (!is.null(num_zeros)) {
    for (i in 1:num_zeros) {
      row_index <- sample(1:n, 1)
      col_index <- sample(1:n, 1)
      symmetric_matrix[row_index, col_index] <- Inf
      symmetric_matrix[col_index, row_index] <- Inf
    }
  }

  return(round(symmetric_matrix))
}

#' Find the Minimum Spanning Tree using Prim's algorithm
#'
#' @param adj_matrix Weighted adjacency matrix
#' @return Weighted adjacency matrix of the Minimum Spanning Tree
#' @export
prim_mst <- function(adj_matrix) {
  n <- nrow(adj_matrix)
  parent <- rep(NA, n)
  key <- rep(Inf, n)
  mst_set <- rep(FALSE, n)

  key[1] <- 0

  for (count in 1:(n - 1)) {
    u <- min_key_vertex(key, mst_set)
    mst_set[u] <- TRUE

    for (v in 1:n) {
      if (adj_matrix[u, v] != 0 && !mst_set[v] && adj_matrix[u, v] < key[v]) {
        parent[v] <- u
        key[v] <- adj_matrix[u, v]
      }
    }
  }

  mst <- matrix(0, nrow = n, ncol = n)
  for (v in 2:n) {
    mst[parent[v], v] <- adj_matrix[parent[v], v]
    mst[v, parent[v]] <- adj_matrix[parent[v], v]
  }

  return(mst)
}

#' Find the vertex with minimum key value that is not yet in the MST
#'
#' @param key Key values for vertices
#' @param mst_set A logical vector indicating whether each vertex is in the MST
#' @return Index of the vertex with the minimum key value that is not yet in the MST
min_key_vertex <- function(key, mst_set) {
  min_key <- Inf
  min_index <- -1

  for (v in 1:length(key)) {
    if (!mst_set[v] && key[v] < min_key) {
      min_key <- key[v]
      min_index <- v
    }
  }

  return(min_index)
}

#' Find the Minimum Spanning Tree using Kruskal's algorithm
#' 
#' @param adj_matrix Weighted adjacency matrix
#' @return Weighted adjacency matrix of the Minimum Spanning Tree
#' @export
kruskal_mst <- function(adj_matrix) {
  num_vertices <- nrow(adj_matrix)
  
  # Find edges and their weights from the adjacency matrix
  edges <- list()
  for (i in 1:num_vertices) {
    for (j in 1:num_vertices) {
      if (adj_matrix[i, j] != 0 && i < j) {
        edges <- c(edges, list(c(i, j, adj_matrix[i, j])))
      }
    }
  }
  
  # Sort edges based on weight
  edges <- do.call(rbind, edges)
  edges <- edges[order(edges[,3]), ]
  
  # Initialize parent array
  parent <- c(1:num_vertices)
  
  # Function to find the root of a node
  find_root <- function(node) {
    while (parent[node] != node) {
      node <- parent[node]
    }
    return(node)
  }
  
  # Function to perform union of two sets
  union_sets <- function(x, y) {
    root_x <- find_root(x)
    root_y <- find_root(y)
    parent[root_x] <<- root_y
  }
  
  mst_edges <- matrix(0, nrow = num_vertices, ncol = num_vertices)
  mst_weight <- 0
  
  # Kruskal's Algorithm
  for (i in 1:(num_vertices - 1)) {
    while (TRUE) {
      edge <- edges[1, ]
      edges <- edges[-1, ]
      u <- edge[1]
      v <- edge[2]
      if (find_root(u) != find_root(v)) {
        mst_edges[u, v] <- edge[3]
        mst_edges[v, u] <- edge[3]
        mst_weight <- mst_weight + edge[3]
        union_sets(u, v)
        break
      }
    }
  }
  
  return(mst_edges)
}

#' Convert an adjacency matrix into a matrix of edges (source, destination, weight)
#'
#' @param adj_matrix Weighted adjacency matrix
#' @return Matrix of edges (source, destination, weight)
#' @export
adj_matrix_to_edges <- function(adj_matrix) {
  num_vertices <- nrow(adj_matrix)
  edges <- matrix(0, nrow = num_vertices^2, ncol = 3)
  count <- 1

  for (i in 1:num_vertices) {
    for (j in (i+1):num_vertices) {
      if (j <= num_vertices && adj_matrix[i, j] != 0) {
        edges[count, ] <- c(i, j, adj_matrix[i, j])
        count <- count + 1
      }
    }
  }

  edges <- edges[1:(count-1), ]
  return(edges)
}
