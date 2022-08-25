library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "W396"){
        data[i,j] <- "1"
      } else if(data[i,j] == "W515") {
        data[i,j] <- "2"
      } else if(data[i,j] == "W686") {
        data[i,j] <- "3"
      } else if(data[i,j] == "W717") {
        data[i,j] <- "4"
      } else if(data[i,j] == "W755") {
        data[i,j] <- "5"
      } else if(data[i,j] == "W756") {
        data[i,j] <- "6"
    }
    }
  }
  return(data)
}

# input - data frame, output - vector of characters
creating_edges <- function(data){
  edges_vec <- c()
  for (i in 1:nrow(data)) {
    if (data[i,3] == 1) {
      edges_vec <- append(edges_vec, data[i,1])
      edges_vec <- append(edges_vec, data[i,2])
    }
  }
  return(edges_vec)
}

#  small female df
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)
edges <- creating_edges(small_female)
small_female_graph <- graph(edges, directed = TRUE)
small_female_plot <- plot(small_female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="tomato", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 0.6,vertex.size = 25, edge.color = "black")

svg("figure5.svg")
plot(small_female_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="tomato", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 0.6,vertex.size = 25, edge.color = "black", main = "Figure 5")
dev.off()
