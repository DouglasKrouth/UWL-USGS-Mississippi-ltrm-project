# This file creates outputs changing the num_intervals
# parameter but using data previously split into different
# time periods

library(readxl)
library(TDAmapper)
library(ggplot2)
library(igraph)
library(mosaic)

# Read and filter data
alldata <- read_excel("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/Cleaned data/Predict_summer_13_backwater.xlsx")
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
create_dirs <- function(foldername,interval){
  dir.create(foldername)
  for(i in interval){
    for(j in interval){
      int <- paste0(i,'_',j)
      dir.create(paste0(foldername,"/",int))
    }
  }
}


# Create a function for interval analysis on TDA ready data
analyze_interval <- function(interval,dataset,time_name,foldername){
  
  # Create distance matrix and pca
  distmatrix <- dist(dataset)
  pca <- prcomp(dataset,center = TRUE,scale.=TRUE,retx = TRUE)
  for(i in interval){
    for(j in interval){
      
      title <- paste0(time_name,'_',i,'_',j)
      map <- mapper2D(distance_matrix = distmatrix,
                      filter_values = list(pca$x[,1],pca$x[,2]),
                      num_intervals = c(i,j),
                      percent_overlap = 25,
                      num_bins_when_clustering = 8)
      
      vertex.label <- lapply(map$points_in_vertex, FUN = length)
      Graph <- graph.adjacency(map$adjacency, mode="undirected")
      
      int <- paste0(i,'_',j)
      
      File <- paste0("./",foldername,"/",int,"/",title,".png")
      dir.create(dirname(File), showWarnings = FALSE)
      png(File)
      
      #plot(Graph, layout = layout.auto(Graph) , vertex.label = vertex.label,main = title )
      
      #vertex.label <- lapply(m2$points_in_vertex, FUN = ave_col_fun)
      # https://www.r-graph-gallery.com/248-igraph-plotting-parameters.html
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
      
  }
}
interv <- c(4,6,10)
# Create directories
folder <- "timesplit_interval_analysis"
create_dirs(folder,interv)


# Create plots for each time period
analyze_interval(interv,data_TDA_all,"All",folder)
analyze_interval(interv,data_TDA_93,"1993-2000",folder)
analyze_interval(interv,data_TDA_01,"2001-2014",folder)
analyze_interval(interv,data_TDA_15,"2015-2019",folder)





