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
  edges <- adj_matrix_to_edges(adj_matrix)
  
  # Sorting edges by increasing weight
  edges <- edges[order(edges[, 3]), ]
  
  # Initialization of the Union-Find data structure
  parent <- 1:num_vertices  # Chaque nœud est son propre parent au début
  rank <- rep(0, num_vertices)  # Rang initial de chaque arbre
  
  # Find function with path compression
  find_root <- function(node) {
    if (node != parent[node]) {
      parent[node] <- find_root(parent[node])
    }
    return(parent[node])
  }
  
  # Union by rank function
  union_sets <- function(x, y) {
    root_x <- find_root(x)
    root_y <- find_root(y)
    if (root_x != root_y) {
      if (rank[root_x] < rank[root_y]) {
        parent[root_x] <- root_y
      } else if (rank[root_x] > rank[root_y]) {
        parent[root_y] <- root_x
      } else {
        parent[root_y] <- root_x
        rank[root_x] <- rank[root_x] + 1
      }
    }
  }
  
  mst_edges <- matrix(0, nrow = num_vertices-1, ncol = 3)
  edge_count <- 1
  
  for (edge in 1:nrow(edges)) {
    if (edge_count > num_vertices - 1) break
    
    u <- edges[edge, 1]
    v <- edges[edge, 2]
    w <- edges[edge, 3]
    if (find_root(u) != find_root(v)) {
      mst_edges[edge_count, ] <- c(u, v, w)
      edge_count <- edge_count + 1
      union_sets(u, v)
    }
  }
  
  # Convert MST edges to adjacency matrix
  mst_adj_matrix <- matrix(0, nrow = num_vertices, ncol = num_vertices)
  for (edge in 1:nrow(mst_edges)) {
    i <- mst_edges[edge, 1]
    j <- mst_edges[edge, 2]
    w <- mst_edges[edge, 3]
    mst_adj_matrix[i, j] <- w
    mst_adj_matrix[j, i] <- w
  }
  
  return(mst_adj_matrix)
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
