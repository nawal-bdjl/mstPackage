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

one.simu <- function(n, func = "prim_mst") {
  adj_matrix <- generate_random_adjacency_matrix(n)

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


################################################################################################


###########################################################
############# One time complexity test ####################
###########################################################
#we evaluate the time with a given n for the 4 algorithms
n <- 1000
one.simu(n, func = "prim_mst")
one.simu(n, func = "kruskal_mst")
one.simu(n, func = "prim_mst_rcpp")
one.simu(n, func = "kruskal_mst_rcpp")



#########################################################################
############# A short simulation study at fixed vector size #############
#########################################################################

#we compare the running time at a given length n with repeated executions (nbSimus times)
nbSimus <- 1000
time1 <- 0
time2 <- 0
time3 <- 0
time4 <- 0
for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "prim_mst")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "kruskal_mst")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "prim_mst_rcpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "kruskal_mst_rcpp")}

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
res <- microbenchmark(one.simu(n, func = "prim_mst_rcpp"), one.simu(n, func = "kruskal_mst_rcpp"), times = 50)
autoplot(res)
res


##########################################
############# time complexity ############
##########################################


# Comparaison des deux algorithmes en R
sizes <- c(5, 7, 10, 15, 20, 50, 100, 200, 500, 1000)
times_prim <- sapply(sizes, function(n) one.simu(n, "prim_mst"))
times_kruskal <- sapply(sizes, function(n) one.simu(n, "kruskal_mst"))

data <- data.frame(
  Size = rep(sizes, times = 2),
  Time = c(times_prim, times_kruskal),
  Algorithm = rep(c("Prim", "Kruskal"), each = length(sizes))
)
data$logSize = log(data$Size)
data$logTime = log(data$Time)

graph = ggplot(data, aes(x = logSize, y = logTime, color = Algorithm)) +
  geom_point() +
  geom_line(aes(group = Algorithm)) +
  labs(title = "Comparaison des performances en R: Prim vs Kruskal",
       x = "Log(Size)", y = "Log(Time)") +
  theme_minimal()

print(graph)
summary(lm(logTime ~ logSize + Algorithm, data = data))


# Comparaison des deux algorithmes en rcpp
sizes <- c(5, 7, 10, 15, 20, 50, 100, 200, 500, 1000)
times_prim <- sapply(sizes, function(n) one.simu(n, "prim_mst_rcpp"))
times_kruskal <- sapply(sizes, function(n) one.simu(n, "kruskal_mst_rcpp"))

data <- data.frame(
  Size = rep(sizes, times = 2),
  Time = c(times_prim, times_kruskal),
  Algorithm = rep(c("Prim", "Kruskal"), each = length(sizes))
)
data$logSize = log(data$Size)
data$logTime = log(data$Time)

graph2 = ggplot(data, aes(x = logSize, y = logTime, color = Algorithm)) +
  geom_point() +
  geom_line(aes(group = Algorithm)) +
  labs(title = "Comparaison des performances en Rcpp: Prim vs Kruskal",
       x = "Log(Size)", y = "Log(Time)") +
  theme_minimal()

print(graph2)
summary(lm(logTime ~ logSize + Algorithm, data = data))




# Comparaison Kruskal R vs rcpp
sizes <- c(5, 7, 10, 15, 20, 50, 100, 200, 500, 1000)
times_kruskal <- sapply(sizes, function(n) one.simu(n, "kruskal_mst"))
times_kruskal_rcpp <- sapply(sizes, function(n) one.simu(n, "kruskal_mst_rcpp"))

data <- data.frame(
  Size = rep(sizes, times = 2),
  Time = c(times_kruskal, times_kruskal_rcpp),
  Algorithm = rep(c("Kruskal_R", "Kruskal_rcpp"), each = length(sizes))
)
data$logSize = log(data$Size)
data$logTime = log(data$Time)

graph3 = ggplot(data, aes(x = logSize, y = logTime, color = Algorithm)) +
  geom_point() +
  geom_line(aes(group = Algorithm)) +
  labs(title = "Comparaison des performances de Kruskal: R vs Rcpp",
       x = "Log(Size)", y = "Log(Time)") +
  theme_minimal()

print(graph3)
summary(lm(logTime ~ logSize + Algorithm, data = data))



# Comparaison Prim R vs Rcpp
sizes <- c(5, 7, 10, 15, 20, 50, 100, 200, 500, 1000, 5000)
times_prim <- sapply(sizes, function(n) one.simu(n, "prim_mst"))
times_prim_rcpp <- sapply(sizes, function(n) one.simu(n, "prim_mst_rcpp"))

data <- data.frame(
  Size = rep(sizes, times = 2),
  Time = c(times_prim, times_prim_rcpp),
  Algorithm = rep(c("Prim_R", "Prim_rcpp"), each = length(sizes))
)
data$logSize = log(data$Size)
data$logTime = log(data$Time)

graph4 = ggplot(data, aes(x = logSize, y = logTime, color = Algorithm)) +
  geom_point() +
  geom_line(aes(group = Algorithm)) +
  labs(title = "Comparaison des performances de Prim: R vs Rcpp",
       x = "Log(Size)", y = "Log(Time)") +
  theme_minimal()

print(graph4)
summary(lm(logTime ~ logSize + Algorithm, data = data))

