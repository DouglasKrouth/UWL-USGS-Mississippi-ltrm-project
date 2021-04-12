# This file produces 7 number summaries for a desired number of the biggest vertices
# for desired variables for each output in the 6-6 8-8 num_interval outputs. Data is still
# split by time


library(readxl)
library(TDAmapper)
library(ggplot2)
library(igraph)
library(tibble)

# This function will return a table of the 7 number summary of vertex or component
get_summary <- function(map,vertex,vars,data,weird,component){
  # Are we getting the summary for a component or vertice?
  if(component){
    # Each variable will have a summary
    width <- length(vars)
    entries <- width*7
    # Initialize the table
    S <- matrix(rep(0,entries),ncol=width)
    colnames(S) <- vars
    rownames(S) <- c("min","1st","median","mean","3rd","max","std")
    S <- as.table(S)
    
    # The table is created, now need to get the points in this component
    curdata <- data[vertex,vars]
    for(var in colnames(S)){
      # get 5 number summary for this var
      varsum <- summary(curdata[,var])
      # get standard dev for this var
      #print(class(curdata[,var]))
      #print(curdata[,var])
      #print(varsum)
      if(!weird){
        std <- sd(curdata[,var])
        S["min",var] <- varsum[[1]]
        S["1st",var] <- varsum[[2]]
        S["median",var] <- varsum[[3]]
        S["mean",var] <- varsum[[4]]
        S["3rd",var] <- varsum[[5]]
        S["max",var] <- varsum[[6]]
        S["std",var] <- std

      }
      else{
        std <- sd(curdata[,var])
        S["min",var] <- as.numeric(strsplit(varsum[1],':')[[1]][2])
        S["1st",var] <- as.numeric(strsplit(varsum[2],':')[[1]][2])
        S["median",var] <- as.numeric(strsplit(varsum[3],':')[[1]][2])
        S["mean",var] <- as.numeric(strsplit(varsum[4],':')[[1]][2])
        S["3rd",var] <- as.numeric(strsplit(varsum[5],':')[[1]][2])
        S["max",var] <- as.numeric(strsplit(varsum[6],':')[[1]][2])
        S["std",var] <- std
      }
    }
  }
  else{
    # We are analyzing a single vertex
    width <- length(vars)
    entries <- width*8
    S <- matrix(rep(0,entries),ncol=width)
    colnames(S) <- vars
    rownames(S) <- c("min","1st","median","mean","3rd","max","std","vsize")
    S <- as.table(S)
    # The table is created, now need to get the points in vertex
    
    thesepoints <- map$points_in_vertex[[vertex]]
    size <- length(thesepoints)
    curdata <- data[thesepoints,vars]
    for(var in colnames(S)){
      # get 5 number summary for this var
      varsum <- summary(curdata[,var])
      # get standard dev for this var
      #print(class(curdata[,var]))
      #print(curdata[,var])
      #print(varsum)
      if(!weird){
        std <- sd(curdata[,var])
        S["min",var] <- varsum[[1]]
        S["1st",var] <- varsum[[2]]
        S["median",var] <- varsum[[3]]
        S["mean",var] <- varsum[[4]]
        S["3rd",var] <- varsum[[5]]
        S["max",var] <- varsum[[6]]
        S["std",var] <- std
        S["vsize",var] <- size
        
      }
      else{
        std <- sd(curdata[,var])
        S["min",var] <- as.numeric(strsplit(varsum[1],':')[[1]][2])
        S["1st",var] <- as.numeric(strsplit(varsum[2],':')[[1]][2])
        S["median",var] <- as.numeric(strsplit(varsum[3],':')[[1]][2])
        S["mean",var] <- as.numeric(strsplit(varsum[4],':')[[1]][2])
        S["3rd",var] <- as.numeric(strsplit(varsum[5],':')[[1]][2])
        S["max",var] <- as.numeric(strsplit(varsum[6],':')[[1]][2])
        S["std",var] <- std
        S["vsize",var] <- size
      }
    }
  }
  return(S)
}

# This function returns a list of the indices of the "numbiggest" biggest vertices
get_v_indices <- function(map,numbiggest){
  piv <- map$points_in_vertex
  lens <- as.numeric(lapply(piv, FUN = length))
  ordered <- order(lens,decreasing = T)[1:numbiggest]
  return(ordered)
}

# Returns a number summary for each var in vars for each connected component
# In the map
connected_comp_summary <- function(map,data,vars,weird){
  # Split up the data into connected components
  G <- graph.adjacency(map$adjacency, mode="undirected")
  components <- components(G)
  # The nested list of vertices in each connected component
  vertice_comps <- vector(mode = "list",length = components$no)
  for(comp in 1:components$no){
    vertice_comps[comp] <- list(which(components$membership %in% comp))
  }
  # Change vertice_comps to a nested list of the union of points
  points_in_comp <- vector(mode = "list",length = components$no)
  for(comp in 1:components$no){
    # Union all the vertices in this component
    points_in_comp[comp] <- list(unique(unlist(map$points_in_vertex[vertice_comps[[comp]]])))
  }
  
  # Create table of summaries
  comp_tables <- vector(mode = "list",length = components$no)
  for(i in 1:components$no){
    tab <- get_summary(map,points_in_comp[[i]],vars,data,weird,TRUE)
    #print(class(tab))
    #print(tab)
    #print(is.list(tab))
    comp_tables[i] <- list(tab)
  }
  return(comp_tables)
}

# Returns a list of tables representing 7 number summary for each output
largenode_summary <- function(map,vars,data,weird,num){
  indices <- get_v_indices(map,num)
  tables <- list()
  for(i in 1:num){
    tables[[i]] <- get_summary(map,indices[i],vars,data,weird,FALSE)
  }
  return(tables)
}


# This is the main function that produces the map object, vertex summaries,
# and connected component summaries
run_output <- function(data,path,interval,title,vars_sum,weird){
  distmatrix <- dist(data)
  pca <- prcomp(data,center = TRUE,scale.=TRUE,retx = TRUE)
  
  map <- mapper2D(distance_matrix = distmatrix,
                  filter_values = list(pca$x[,1],pca$x[,2]),
                  num_intervals = interval,
                  percent_overlap = 25,
                  num_bins_when_clustering = 8)
  
  # Create the summaries
  largenodes_summary <- largenode_summary(map,vars_sum,data,weird,20)
  component_summary <- connected_comp_summary(map,data,vars_sum,weird)
  
  # Save them to a file
  saveRDS(component_summary, file = paste0(path,"/",title,"comp_summary",".rds"))
  saveRDS(largenodes_summary, file = paste0(path,"/",title,"large_nodesummary",".rds"))
  saveRDS(map, file = paste0(path,"/",title,"map",".rds"))
  
  
  vertex.label <- lapply(map$points_in_vertex, FUN = length)
  Graph <- graph.adjacency(map$adjacency, mode="undirected")
  
  File <- paste0(path,"/",title,".png")
  
  png(File)
  
  par(bg="white")
  plot(Graph, 
       layout = layout.auto(Graph),
       vertex.size = scale_vertex_by_points(map),
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
       edge.curved=0.3,
       main = title
  )
  
  dev.off()
  
}

# Read and filter data - make sure data read in is in csv, for some reason xlsx produces a "weird" formatted 
# summary and should put weird= True for the run_output function
alldata <- read.csv("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/Cleaned data/Predict_summer_13_backwater.csv")
data_93_00 <- read.csv("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/Cleaned data/Predict_summer_13_backwater_timesplit/1993-2000.csv")
data_01_14 <- read.csv("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/Cleaned data/Predict_summer_13_backwater_timesplit/2001-2014.csv")
data_15_19 <- read.csv("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/Cleaned data/Predict_summer_13_backwater_timesplit/2015-2019.csv")

keep <- c("WDP","SECCHI","TEMP","DO","TURB","COND","VEL","SS",
          "PredictedTN","PredictedTP","CHLcal")

# Get data ready for tda
data_TDA_all <- alldata[,(names(alldata) %in% keep)]
data_TDA_93 <- data_93_00[,(names(data_93_00) %in% keep)]
data_TDA_01 <- data_01_14[,(names(data_01_14) %in% keep)]
data_TDA_15 <- data_15_19[,(names(data_15_19) %in% keep)]


# Create the directories for organizing the outputs
foldername <- "Vertex Summaries 6_6 8_8"
dir.create(foldername)
dir.create(paste0(foldername,"/6_6"))
dir.create(paste0(foldername,"/8_8"))


# Run outputs 1993
#keep <- c("TURB","CHLcal","PredictedTP","PredictedTN","WDP","SS","DO")
title <- "1993_2000_6_6"
interv1 <- c(6,6)
interv2 <- c(8,8)
path <- "./Vertex Summaries 6_6 8_8/6_6"
run_output(data_TDA_93,path,interv1,title,keep,FALSE)
Csums1 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums1 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map1 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map1$adjacency, mode="undirected")
components1 <- components(G)



title <- "1993_2000_8_8"
path <- "./Vertex Summaries 6_6 8_8/8_8"
run_output(data_TDA_93,path,interv2,title,keep,FALSE)
Csums2 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums2 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map2 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map2$adjacency, mode="undirected")
components2 <- components(G)

# Run outputs 2001
title <- "2001_2014_6_6"
interv1 <- c(6,6)
interv2 <- c(8,8)
path <- "./Vertex Summaries 6_6 8_8/6_6"
run_output(data_TDA_01,path,interv1,title,keep,FALSE)
Csums3 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums3 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map3 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map3$adjacency, mode="undirected")
components3 <- components(G)

title <- "2001_2014_8_8"
path <- "./Vertex Summaries 6_6 8_8/8_8"
run_output(data_TDA_01,path,interv2,title,keep,FALSE)
Csums4 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums4 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map4 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map4$adjacency, mode="undirected")
components4 <- components(G)

# Run outputs 2015
title <- "2015_2019_6_6"
interv1 <- c(6,6)
interv2 <- c(8,8)
path <- "./Vertex Summaries 6_6 8_8/6_6"
run_output(data_TDA_15,path,interv1,title,keep,FALSE)
Csums5 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums5 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map5 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map5$adjacency, mode="undirected")
components5 <- components(G)

title <- "2015_2019_8_8"
path <- "./Vertex Summaries 6_6 8_8/8_8"
run_output(data_TDA_15,path,interv2,title,keep,FALSE)
Csums6 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums6 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map6 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map6$adjacency, mode="undirected")
components6 <- components(G)

# Run outputs all
title <- "all_6_6"
interv1 <- c(6,6)
interv2 <- c(8,8)
path <- "./Vertex Summaries 6_6 8_8/6_6"
run_output(data_TDA_all,path,interv1,title,keep,FALSE)
Csums7 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums7 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map7 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map7$adjacency, mode="undirected")
components7 <- components(G)

title <- "all_8_8"
path <- "./Vertex Summaries 6_6 8_8/8_8"
run_output(data_TDA_all,path,interv2,title,keep,FALSE)
Csums8 <- readRDS(file = paste0(path,"/",title,"comp_summary",".rds"))
Vsums8 <- readRDS(file = paste0(path,"/",title,"large_nodesummary",".rds"))
map8 <- readRDS(file = paste0(path,"/",title,"map",".rds"))
G <- graph.adjacency(map8$adjacency, mode="undirected")
components8 <- components(G)







    
File <- paste0("./",foldername,"/6_6","/","1993_2000_6_6",".png")
    dir.create(dirname(File), showWarnings = FALSE)

