# MST Package Vignette

### Nawal BENDJELLOUL, Rabab KHATIB, Gabriele PEDRONI

#### M2 Data Science, Université Paris Saclay

### April 11, 2024

> [Quick Start](#qs)

> [Minimum Spanning Trees Algorithms](#mst)

> [Example Usage](#usage)

<a id="qs"></a>

## Quick Start

The `mst` R package provides implementations of algorithms for finding Minimum Spanning Trees (MSTs) in graphs. It includes Prim's algorithm and Kruskal's algorithm, both capable of finding the MST in a weighted undirected graph. This package is intended for educational purposes to demonstrate the functionality and efficiency of these algorithms.

### Package Installation

To install the `mst` package, you can use the `devtools` package:

```r
devtools::install_github("nawal-bdjl/mstPackage")
library(mstPackage)
```

### Available Functions

- `prim_mst`: Implements Prim's algorithm to find the MST of a given graph.
- `kruskal_mst`: Implements Kruskal's algorithm to find the MST of a given graph.

<a id="mst"></a>

## Minimum Spanning Trees Algorithms

### Prim's Algorithm

Prim's algorithm grows a Minimum Spanning Tree from a starting vertex by adding the shortest edge that connects the tree to a new vertex in each step.

### Kruskal's Algorithm

Kruskal's algorithm builds the MST by progressively adding edges with the smallest weights, ensuring that no cycle is formed.

<a id="usage"></a>

## Example Usage

### Generating a Graph

First, let's generate a random weighted adjacency matrix representing a graph:

```r
# Set the number of vertices
n <- 10

# Generate a random weighted adjacency matrix
adj_matrix <- generate_random_adjacency_matrix(n)

# Print the adjacency matrix
print(adj_matrix)
```

### Finding MST using Prim's Algorithm

```r
# Find MST using Prim's algorithm
mst_prim <- prim_mst(adj_matrix)

# Print the MST
print("Minimum Spanning Tree using Prim's algorithm:")
print(mst_prim)
```

### Finding MST using Kruskal's Algorithm

```r
# Find MST using Kruskal's algorithm
mst_kruskal <- kruskal_mst(adj_matrix)

# Print the MST
print("Minimum Spanning Tree using Kruskal's algorithm:")
print(mst_kruskal)
```

This package provides efficient implementations of MST algorithms, allowing users to analyze graphs and extract essential structures efficiently.

[Back to Top](#top)
