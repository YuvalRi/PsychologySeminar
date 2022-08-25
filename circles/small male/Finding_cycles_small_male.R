library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M567"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M597") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M607") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M614" | data[i,j] == "M614 ") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M615") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M632") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M633") {
        data[i,j] <- "7"
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
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
small_male <- name_to_number(small_male)
edges <- creating_edges(small_male)
small_male_graph <- graph(edges, directed = TRUE)
small_male_plot <- plot(small_male_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="royalblue1", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 0.8,vertex.size = 25, edge.color = "black")

svg("figure6.svg")
plot(small_male_graph, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="royalblue1", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 0.8,vertex.size = 25, edge.color = "black", main = "Figure 6")
dev.off()



