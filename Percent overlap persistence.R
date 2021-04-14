# This file creates multiple outputs changing the 
# percent overlap parameter, used for checking for persistence of structure

library(readxl)
library(TDAmapper)
library(ggplot2)
library(igraph)
library(mosaic)

# Read and filter data
data <- read_excel("C:/Users/cashe/OneDrive/Desktop/Data Science/Mississippi River analysis/Cleaned data/Predict_summer_13_backwater.xlsx")
keep <- c("WDP","SECCHI","TEMP","DO","TURB","COND","VEL","SS",
          "PredictedTN","PredictedTP","CHLcal")
data_TDA <- data[,(names(data) %in% keep)]

# Create distance matrix and pca
distmatrix = dist(data_TDA)
pca <- prcomp(data_TDA,center = TRUE,scale.=TRUE,retx = TRUE)


# Create outputs with percent overlap equal to every value in this vector
for(i in c(25,35,45)){
    
    title <- paste0('Percent','_',i)
    
    map <- mapper2D(distance_matrix = distmatrix,
                    filter_values = list(pca$x[,1],pca$x[,2]),
                    num_intervals = c(5,5),
                    percent_overlap = i,
                    num_bins_when_clustering = 8)
    
    
    vertex.label <- lapply(map$points_in_vertex, FUN = ave_col_fun)
    Graph <- graph.adjacency(map$adjacency, mode="undirected")
    
    File <- paste0("./percent_overlap_analysis/",title,".png")
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
         vertex.label.dist= 0,
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