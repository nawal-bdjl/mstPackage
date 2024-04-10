#include <Rcpp.h>
using namespace Rcpp;

//' Find the vertex with minimum key value that is not yet in the MST
//'
//' @param key NumericVector containing the key values of vertices
//' @param mst_set LogicalVector indicating whether vertices are already in the MST
//' @return Index of the vertex with the minimum key value
//' @export
// [[Rcpp::export]]
int min_key_vertex(NumericVector key, LogicalVector mst_set) {
  int min_index = -1;
  // Initialize min_key to +inf
  double min_key = R_PosInf;

  // Loop over vertexes
  for (int v = 0; v < key.size(); v++) {
    // Check if the vertex is not yet in the MST and its key is smaller than the current minimum key
    if (!mst_set[v] && key[v] < min_key) {
      // Update the minimum key and the index of the corresponding vertex
      min_key = key[v];
      min_index = v;
    }
  }

  // Return the index of the vertex with the minimum key
  return min_index;
}

// Generate a random adjacency matrix
//'
//' @param n Integer specifying the number of vertices
//' @param num_zeros Nullable integer specifying the number of zero-weight edges to introduce
//' @return NumericMatrix representing the weighted adjacency matrix
//' @export
// [[Rcpp::export]]
NumericMatrix generate_random_adjacency_matrix(int n, Nullable<int> num_zeros = R_NilValue) {
  // Initialize a symmetric matrix
  NumericMatrix symmetric_matrix(n, n);
  // Fill the symm matrix with random numbers, keeping the symmetry
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      symmetric_matrix(i, j) = symmetric_matrix(j, i) = R::runif(1, 11);
    }
  }

  // Keep into account the "self-distance" of each node
  for (int i = 0; i < n; i++) {
    symmetric_matrix(i, i) = R_PosInf;
  }

  // This piece of code makes sure that the graph is not complete; we take some pairs of edges at
  // random and we set their distance to +inf, effectively "disconnetting them"
  if (num_zeros.isNotNull()) {
    int num_zeros_val = as<int>(num_zeros);
    for (int i = 0; i < num_zeros_val; i++) {
      int row_index = R::runif(0, n);
      int col_index = R::runif(0, n);
      symmetric_matrix(row_index, col_index) = symmetric_matrix(col_index, row_index) = R_PosInf;
    }
  }

  return symmetric_matrix;
}

//' Find the Minimum Spanning Tree using Prim's algorithm
//'
//' @param adj_matrix Weighted adjacency matrix
//' @return Weighted adjacency matrix of the Minimum Spanning Tree
// [[Rcpp::export]]
NumericMatrix prim_mst_rcpp(NumericMatrix adj_matrix) {
  int n = adj_matrix.nrow();

  bool is_connected = is_connected_bfs(adj_matrix);
  if (!is_connected) {
    Rcpp::warning("Warning: graph is not connected, there is no MST");
    return NumericMatrix(n, n); // Return an empty matrix if not connected
  }

  // Initialize "parent" array with NA values
  IntegerVector parent(n, NA_INTEGER);
  // Initialize "key" array with +inf
  NumericVector key(n, R_PosInf);
  // Initialize "mst_set" array with false values
  LogicalVector mst_set(n, false);
  key[0] = 0;

  // Loop over vertexes
  for (int count = 0; count < n - 1; count++) {
    // Find the vertex with the minimum key value and add it to the mst set
    int u = min_key_vertex(key, mst_set);
    mst_set[u] = true;

    // Update the key values of adjacent vertices if they are not yet in the MST and have smaller key values
    for (int v = 0; v < n; v++) {
      if (adj_matrix(u, v) != 0 && !mst_set[v] && adj_matrix(u, v) < key[v]) {
        parent[v] = u;
        key[v] = adj_matrix(u, v);
      }
    }
  }

  // Initialize the mst matrix
  NumericMatrix mst(n, n);
  // Fill the mst matrix with edges
  for (int v = 1; v < n; v++) {
    mst(parent[v], v) = adj_matrix(parent[v], v);
    mst(v, parent[v]) = adj_matrix(parent[v], v);
  }

  return mst;
}

//' Find the Minimum Spanning Tree using Kruskal's algorithm
//'
//' @param adj_matrix Weighted adjacency matrix
//' @return Weighted adjacency matrix of the Minimum Spanning Tree
// [[Rcpp::export]]
NumericMatrix kruskal_mst_rcpp(NumericMatrix adj_matrix) {
  int num_vertices = adj_matrix.nrow();

  bool is_connected = is_connected_bfs(adj_matrix);
  if (!is_connected) {
    Rcpp::warning("Warning: graph is not connected, there is no MST");
    return NumericMatrix(n, n); // Return an empty matrix if not connected
  }

  // Find edges and their weights from the adjacency matrix
  std::vector<std::vector<double>> edges;
  for (int i = 0; i < num_vertices; i++) {
    for (int j = i + 1; j < num_vertices; j++) {
      if (adj_matrix(i, j) != 0) {
        edges.push_back({(double)i, (double)j, adj_matrix(i, j)});
      }
    }
  }

  // Sort edges based on weight
  std::sort(edges.begin(), edges.end(), [](const std::vector<double>& a, const std::vector<double>& b) {
    return a[2] < b[2];
  });

  // Initialize parent array
  std::vector<int> parent(num_vertices);
  std::iota(parent.begin(), parent.end(), 0);

  // Function to find the root of a node
  auto find_root = [&](int node) {
    while (parent[node] != node) {
      node = parent[node];
    }
    return node;
  };

  // Function to perform union of two sets
  auto union_sets = [&](int x, int y) {
    int root_x = find_root(x);
    int root_y = find_root(y);
    parent[root_x] = root_y;
  };

  // Initialize mst matrix and weights
  NumericMatrix mst_edges(num_vertices, num_vertices);

  // Kruskal's Algorithm
  for (const auto& edge : edges) {
    int u = edge[0];
    int v = edge[1];
    // Condition to avoid that adding an edge will form a cycle
    if (find_root(u) != find_root(v)) {
      // Add the edge to the MST
      mst_edges(u, v) = edge[2];
      mst_edges(v, u) = edge[2];
      // Merge the sets
      union_sets(u, v);
    }
  }

  return mst_edges;
}

//' Convert an adjacency matrix into a list of edges (source, destination, weight)
//'
//' @param adj_matrix Weighted adjacency matrix
//' @return Matrix of edges (source, destination, weight)
// [[Rcpp::export]]
NumericMatrix adj_matrix_to_edges(NumericMatrix adj_matrix) {
  int num_vertices = adj_matrix.nrow();
  // Initialize the edges matrix
  NumericMatrix edges(num_vertices * num_vertices, 3);
  int count = 0;

  // Iterate over each pair of vertices
  for (int i = 0; i < num_vertices; i++) {
    for (int j = i + 1; j < num_vertices; j++) {
      // Check if there is an edge between the vertices
      if (adj_matrix(i, j) != 0) {
        // Set the source vertex
        edges(count, 0) = i + 1;
        // Set the destination vertex
        edges(count, 1) = j + 1;
        // Set the weight of the edge
        edges(count, 2) = adj_matrix(i, j);
        // Increment the count
        count++;
      }
    }
  }

  // Resize the edges matrix to remove unused rows
  edges = edges(Range(0, count - 1), _);
  return edges;
}
