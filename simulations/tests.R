################################################
############# A simple test ####################
################################################

library(mstPackage)
library(ggplot2)
library(microbenchmark)

n <- 10 #data size
s <- generate_random_adjacency_matrix(n) #generate data

# the 4 algorithms

prim_mst(s)
kruskal_mst(s)
prim_mst_rcpp(s)
kruskal_mst_rcpp(s)


################################################################################################
# We define the function one.simu which returns the execution time of a given algorithm

library(mstPackage)
library(ggplot2)
library(microbenchmark)
one.simu <- function(func = "prim_mst", ajd_matrix = NULL) {
  start_time <- NA
  end_time <- NA

  if (func == "prim_mst") {
    start_time <- Sys.time()
    mst <- prim_mst(adj_matrix)
    end_time <- Sys.time()

  } else if (func == "kruskal_mst") {
    start_time <- Sys.time()
    mst <- kruskal_mst(adj_matrix)
    end_time <- Sys.time()
  }
  else if (func == "kruskal_mst_rcpp") {
    start_time <- Sys.time()
    mst <- kruskal_mst_rcpp(adj_matrix)
    end_time <- Sys.time()
  }
  else if (func == "prim_mst_rcpp") {
    start_time <- Sys.time()
    mst <- kruskal_mst_rcpp(adj_matrix)
    end_time <- Sys.time()
  }

  return(unclass(end_time - start_time)[1])
}

n <- 1000
adj_matrix <- generate_random_adjacency_matrix(n)
one.simu(func = "kruskal_mst", ajd_matrix)


################################################################################################


###########################################################
############# One time complexity test ####################
###########################################################
#we evaluate the time with a given n for the 4 algorithms
n <- 1000
adj_matrix <- generate_random_adjacency_matrix(n)
one.simu(func = "prim_mst", adj_matrix)
one.simu(func = "kruskal_mst", adj_matrix)
one.simu(func = "prim_mst_rcpp",adj_matrix)
one.simu(func = "kruskal_mst_rcpp",adj_matrix)



#########################################################################
############# A short simulation study at fixed vector size #############
#########################################################################

#we compare the running time at a given length n with repeated executions (nbSimus times)
nbSimus <- 1000
adj_matrix <- generate_random_adjacency_matrix(n)
time1 <- 0
time2 <- 0
time3 <- 0
time4 <- 0
for(i in 1:nbSimus){time1 <- time1 + one.simu(func = "prim_mst", adj_matrix)}
for(i in 1:nbSimus){time2 <- time2 + one.simu(func = "kruskal_mst", adj_matrix)}
for(i in 1:nbSimus){time3 <- time3 + one.simu(func = "prim_mst_rcpp", adj_matrix)}
for(i in 1:nbSimus){time4 <- time4 + one.simu(func = "kruskal_mst_rcpp", adj_matrix)}

#gain R -> rcpp
time1/time3
time2/time4

#gain prim -> kruskal
time1/time2
time3/time4

#max gain
time1/time4 #à adapter


####### MY RESULT #######
#> #gain R -> rcpp
#  > time1/time3
#
#> time2/time4
#
#>
#  > #gain prim -> kruskal
#  > time1/time2
#
#> time3/time4
#
#>
#  > #max gain
#  > time1/time4
#


#HERE : R to rcpp => at least 150 times faster #à changer aussi
#HERE : insertion to heap => 10 times faster



##########################################
############# microbenchmark #############
##########################################

n <- 5000
adj_matrix <- generate_random_adjacency_matrix(n)
res <- microbenchmark(one.simu(func = "prim_mst_rcpp", ajd_matrix), one.simu(func = "kruskal_mst_rcpp"), times = 50, ajd_matrix)
autoplot(res)
res


##########################################
############# time complexity ############
##########################################

sizes <- c(3, 5)#, 10, 15, 20, 50, 100, 200, 500, 1000)

times <- vector("list", length(sizes))
names(times) <- as.character(sizes)

# Time measure 
for (size in sizes) {
  adj_matrix <- generate_random_adjacency_matrix(size)

  times[[as.character(size)]] <- c(
    prim_mst = one.simu("prim_mst", adj_matrix),
    kruskal_mst = one.simu("kruskal_mst", adj_matrix),
    prim_mst_rcpp = one.simu("prim_mst_rcpp", adj_matrix),
    kruskal_mst_rcpp = one.simu("kruskal_mst_rcpp", adj_matrix)
  )
}

results_list <- lapply(names(times), function(size) {
  data.frame(Size = rep(as.integer(size), 4),
             Time = as.numeric(times[[size]]),
             Algorithm = c("Prim_R", "Kruskal_R", "Prim_Rcpp", "Kruskal_Rcpp"))
})

results_df <- do.call(rbind, results_list)
results_df$logSize <- log(results_df$Size)
results_df$logTime <- log(results_df$Time)


# Plot results 
analyze_and_plot <- function(df, filter, title) {
  filtered_df <- df[df$Algorithm %in% filter, ]

  ggplot(filtered_df, aes(x = logSize, y = logTime, color = Algorithm)) +
    geom_point() +
    geom_line(aes(group = Algorithm)) +
    labs(title = title, x = "Log(Size)", y = "Log(Time)") +
    theme_minimal() -> p
  print(p)

  fit <- lm(logTime ~ logSize + Algorithm, data = filtered_df)
  print(summary(fit))

  slopes <- coef(fit)[grep("logSize", names(coef(fit)))]
  cat("\nSlopes for", title, ": ", slopes, "\n\n")
}

analyze_and_plot(results_df, c("Prim_R", "Kruskal_R"), "Comparaison des performances en R: Prim vs Kruskal")
analyze_and_plot(results_df, c("Prim_Rcpp", "Kruskal_Rcpp"), "Comparaison des performances en Rcpp: Prim vs Kruskal")
analyze_and_plot(results_df, c("Kruskal_R", "Kruskal_Rcpp"), "Comparaison des performances de Kruskal: R vs Rcpp")
analyze_and_plot(results_df, c("Prim_R", "Prim_Rcpp"), "Comparaison des performances de Prim: R vs Rcpp")
