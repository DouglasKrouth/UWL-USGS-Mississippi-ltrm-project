# This file is used for individual vertex analysis between
# 3 variable outputs and all variable outputs.

# A generalized function for this analysis has not
# been created yet

library(readxl)
library(TDAmapper)
library(ggplot2)
library(igraph)
library(mosaic)
# Creating the distance matrix on all 11 variables - cleaned
distmatrix <- dist(data_TDA)

# Create the pca object on all data 
pca <- prcomp(data_TDA,center = TRUE,scale.=TRUE,retx = TRUE)

# This mapper object has all 11 variables
allmap <- mapper2D(distance_matrix = distmatrix,
                   filter_values = list(pca$x[,1],pca$x[,2]),
                   num_intervals = c(6,6),
                   percent_overlap = 25,
                   num_bins_when_clustering = 8)


# Select the variables we want to compare to
curvars <- c("CHLcal","TURB","PredictedTP")
thisdata <- select(data_TDA,curvars)
distmatrix <- dist(thisdata)
pca <- prcomp(thisdata,center = TRUE,scale.= TRUE,retx = TRUE)


curmap <- mapper2D(distance_matrix = distmatrix,
                   filter_values = list(pca$x[,1],pca$x[,2]),
                   num_intervals = c(6,6),
                   percent_overlap = 25,
                   num_bins_when_clustering = 8)

# Optional display of cur output
vertex.label <- lapply(curmap$points_in_vertex, FUN = length)
Graph <- graph.adjacency(curmap$adjacency, mode="undirected")
plot(Graph, 
     layout = layout.auto(Graph),
     vertex.size = scale_vertex_by_points(curmap),
     vertex.label = vertex.label,
     vertex.label.family="Helvetica",
     vertex.label.color=c("black"),
     vertex.label.dist= -2,
     vertex.label.degree=20,
     edge.color="black",                           # Edge color
     edge.width=2,                                 # Edge width, defaults to 1
     edge.arrow.size=1,                            # Arrow size, defaults to 1
     edge.arrow.width=1,                           # Arrow width, defaults to 1
     edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
     edge.curved=0.3
)


# Check which vertices have the most points in all
lapply(allmap$points_in_vertex,FUN = length)
length(allmap$points_in_vertex[[18]])
length(allmap$points_in_vertex[[24]])
length(allmap$points_in_vertex[[19]])

# Check which vertices have the most points in cur variables
lapply(curmap$points_in_vertex,FUN = length)
# Group the smaller nodes into a set 
smaller_union_all <- curmap$points_in_vertex[[7]]

# Group the larger nodes into a set
temp_union <- union(allmap$points_in_vertex[[18]],allmap$points_in_vertex[[19]])
larger_union_all <- union(temp_union,allmap$points_in_vertex[[24]])

# Group all points into a big blob
unionALL <- union(smaller_union_all,larger_union_all)

# Group all points in cur into a big blob
unionTHIS <- union(curmap$points_in_vertex[[9]],curmap$points_in_vertex[[15]])
bigblob <- curmap$points_in_vertex[[15]]

# Individual node analysis
numcommonBLOB <- length(intersect(larger_union_all,smaller_union_all))
cat("The bigggest node in cur capture ",numcommonBLOB/length(unionALL)," percent of 3 biggest nodes in ALL")

numcommonsmall_9 <- length(intersect(smaller_union_all,bigblob))
cat("Biggest node in cur captures  ",numcommonsmall_9/length(smaller_union_all)," percent of 2 smallest nodes in ALL")

numcomlarge_15 <- length(intersect(larger_union_all,bigblob))
cat("Biggest node in cur captures  ",numcomlarge_15/length(larger_union_all)," percent of 2 largest nodes in ALL")

numcomsmall_15 <- length(intersect(smaller_union_all,curmap$points_in_vertex[[15]]))
cat("Vertex 15 of size 1302 captures  ",numcomsmall_15/length(smaller_union_all)," percent of 2 smallest nodes in ALL")

numcomlarge_9 <- length(intersect(larger_union_all,curmap$points_in_vertex[[9]]))
cat("Vertex 9 of size 907 captures ",numcomlarge_9/length(larger_union_all)," percent of 2 largest nodes in ALL")

# Big nodes are 10, 11, 18 19 ALL
# 15 and 9 for CUR
numcom10_15 <- length(intersect(allmap$points_in_vertex[[10]],bigblob))
cat("Biggest node in cur captures ",numcom10_15/length(allmap$points_in_vertex[[10]])," percent of vertex 10 of size 348 in ALL")

numcom10_9 <- length(intersect(allmap$points_in_vertex[[10]],bigblob))
cat("Biggest node in cur captures ",numcom10_9/length(allmap$points_in_vertex[[10]])," percent of vertex 10 of size 348 in ALL")

numcom11_15 <- length(intersect(allmap$points_in_vertex[[11]],bigblob))
cat("Biggest node in cur captures ",numcom11_15/length(allmap$points_in_vertex[[11]])," percent of vertex 11 of size 354 in ALL")

numcom11_9 <- length(intersect(allmap$points_in_vertex[[11]],bigblob))
cat("Biggest node in cur captures ",numcom11_9/length(allmap$points_in_vertex[[11]])," percent of vertex 11 of size 354 in ALL")

numcom18_15 <- length(intersect(allmap$points_in_vertex[[18]],bigblob))
cat("Biggest node in cur captures ",numcom18_15/length(allmap$points_in_vertex[[18]])," percent of vertex 18 of size 801 in ALL")

numcom18_9 <- length(intersect(allmap$points_in_vertex[[18]],curmap$points_in_vertex[[9]]))
cat("Vertex 9 of size 907 captures ",numcom18_9/length(allmap$points_in_vertex[[18]])," percent of vertex 18 of size 801 in ALL")

numcom19_15 <- length(intersect(allmap$points_in_vertex[[19]],bigblob))
cat("Biggest node in cur captures ",numcom18_15/length(allmap$points_in_vertex[[19]])," percent of vertex 19 of size 813 in ALL")

numcom19_9 <- length(intersect(allmap$points_in_vertex[[19]],curmap$points_in_vertex[[9]]))
cat("Vertex 9 of size 907 captures ",numcom18_9/length(allmap$points_in_vertex[[19]])," percent of vertex 19 of size 813 in ALL")


