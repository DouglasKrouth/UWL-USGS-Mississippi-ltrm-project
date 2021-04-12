# IMPORTS 
library(dplyr)
library(TDAmapper)
library(igraph)
require(fastcluster)



# GLOBALS
NUM_INTERVALS = 10
NUM_BINS_WHEN_CLUSTERING = 8
PERCENT_OVERLAP = 35



# Load the data
df <- data.frame(Predict_summer_13_backwater)
# Select desired columns
# Water Depth, TP, TN, Suspended Solids, SECCHI, Turbidity, Chlorophyll
df_clean <- select(df, "WDP", "SECCHI", "TURB", "SS", "CHLcal", "TEMP", "PredictedTN", "PredictedTP")
dist_matrix <- dist(df_clean, method = "euclidean", diag = FALSE, upper = FALSE)



# install.packages("fastcluster") 



vertex.label=lapply(m1$points_in_vertex, FUN = ave_col_fun)
pca<-prcomp(dist_matrix, scale=FALSE, tol = 0.3, rank. = 3)




ave_col_fun <- function(a){
  mean_list<-c(round(mean(df_clean[a,]$SS),digits = 1),
               round(mean(df_clean[a,]$CHLcal), digits = 1),
               round(mean(df_clean[a,]$PredictedTP),digits=1))
  return(mean_list)
}



# fun_label <- function(a){
#   summation <- length(m2$points_in_vertex[[a]])
#   return(summation)
# }
# fun_label()



m1 <- mapper1D(
  distance_matrix = dist_matrix,
  filter_values = df_clean$PredictedTN,
  num_intervals = NUM_INTERVALS,
  percent_overlap = PERCENT_OVERLAP,
  num_bins_when_clustering = NUM_BINS_WHEN_CLUSTERING)



m2 <- mapper2D(
  distance_matrix = dist_matrix,
  filter_values = list(pca$rotation[,1],pca$rotation[,2]),
  num_intervals = c(7,7),
  percent_overlap = 35,
  num_bins_when_clustering = 10)



# temp <- table(m2$level_of_vertex)



# m1 <- mapper1D(
#   distance_matrix = dist_matrix,
#   filter_values = 2*cos(0.5*(1:100)),
#   num_intervals = NUM_INTERVALS,
#   percent_overlap = PERCENT_OVERLAP,
#   num_bins_when_clustering = NUM_BINS_WHEN_CLUSTERING)



g1 <- graph.adjacency(m1$adjacency, mode="undirected")
# V(g1)$vertex_size <- length(na.omit(points_in_vertex))
g2 <- graph.adjacency(m2$adjacency, mode="undirected")



temp_size <- c()
for (i in m2$points_in_vertex){
  append_value <- length(na.omit(i))
  temp_size <- append(temp_size, append_value)
}



# V(g1)$vertex_size <- temp_size
# V(g1)$vertex_size
# plot(g1, layout = layout.auto(g1), vertex.size = V(g1)$vertex_size)



fun_label <- function(a){
  summation <- c(length(df_clean[a]),0)
  # summation <- length(m2$points_in_vertex[[a]])
  return(summation)
}



m2 <- mapper2D(
  distance_matrix = dist_matrix,
  filter_values = list(pca$rotation[,1],pca$rotation[,2]),
  num_intervals = c(3,3),
  percent_overlap = 35,
  num_bins_when_clustering = 10)



g2 <- graph.adjacency(m2$adjacency, mode="undirected")




ave_col_fun <- function(a){
  mean_list<-c(round(mean(df_clean[a,]$SS),digits = 1),
               round(mean(df_clean[a,]$CHLcal), digits = 1),
               round(mean(df_clean[a,]$PredictedTP),digits=1))
  return(mean_list)
}



temp_size <- c()
for (i in m2$points_in_vertex){
  append_value <- length(na.omit(i))
  temp_size <- append(temp_size, append_value)
}



vertex.label <- lapply(m2$points_in_vertex, FUN = ave_col_fun)
plot(g2, layout = layout.auto(g2), vertex.label = vertex.label, vertex.size = 0.2 * temp_size)