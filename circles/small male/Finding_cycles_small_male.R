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

# function which return a data frame that represent an undirected graph (removing duplicated edges)
# Q1: version == TRUE
# for Q1
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 17){
        {break}
      }
      if( df[i,1] == df[j,2] & df[i,2] == df[j,1] ){
        if (df[i,3] == 1 & df[j,3] == 1){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == 0 & df[j,3] == 0){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == version & df[j,3] == !version){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == !version & df[j,3] == version){
          df_undirected <- rbind(df_undirected, c(df[j,]))
        }
      }
    }
  }
  return(df_undirected)
}

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  return(unique(vec[!is.na(vec)]))
}


# check if edge is found in df (the df should include only edges that appear in the graph)

is_edge <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i,1] == edge[1] && df[i,2] == edge[2]){
      return(TRUE)
    }
  }
  return(FALSE)
}

#v2, v3 are neighbours of v1
is_circle <- function(v1, v2, v3, df){
  if (is_edge(edge = c(v1, v2), df) && is_edge(edge = c(v1, v3), df) && (is_edge(edge = c(v2, v3), df) | is_edge(edge = c(v3, v2), df))){
    return(TRUE)
  }
  return(FALSE)
}


# counting circles in the graph (directed graph)
count_circles <- function(df){
  count <- 0 
  vec <- c(1,3,5) # the vertices in the graph
  for (v in 1:3){ 
    for(w in 1:2){
      if (w == v){
        next
      }
      for(z in (w+1):3){
        if(z == v | z == w){
          next
        }
        if(is_circle(vec[v], vec[w], vec[z], df)){
          count <- count + 1
        }
      }
    }
  }
  return(count)
}


# for Q3
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 3){
        {break}
      }
      if( df[i,1] == df[j,2] & df[i,2] == df[j,1] ){
        if (df[i,3] == 1 & df[j,3] == 1){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == 0 & df[j,3] == 0){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == version & df[j,3] == !version){
          df_undirected <- rbind(df_undirected, c(df[i,]))
        }
        if (df[i,3] == !version & df[j,3] == version){
          df_undirected <- rbind(df_undirected, c(df[j,]))
        }
      }
    }
  }
  return(df_undirected)
}

#  small female df
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
small_male <- name_to_number(small_male)
edges <- creating_edges(small_male)
small_male_graph <- graph(edges, directed = TRUE)
small_male_plot <- plot(small_male_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black")

svg("figure6.svg")
plot(small_male_graph, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="gray63", vertex.label.color="black", vertex.frame.color="black", vertex.label.cex = 1,vertex.size = 25, edge.color = "black", main = "nodesGraphDataset6")
dev.off()


## Q1 - 1 circle
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
small_male <- name_to_number(small_male)
small_male <- directed_to_undirected_q3(small_male, TRUE)
edges <- creating_edges(small_male)
small_male_graph <- graph(edges, directed = FALSE)
graph.motifs(small_male_graph,size=3)[length(graph.motifs(small_male_graph,size=3))]


## Q2 - 0 circles
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
small_male <- name_to_number(small_male)
small_male <- small_male[-which(small_male[,1] == "4" | small_male[,2] == "4"),] # '4' participant has no neighbors, so it removed from analysis
small_male <- small_male[-which(small_male[,1] == "2" | small_male[,2] == "2"),] # '2' participant had only 1 neighbor, so it removed from analysis
small_male <- small_male[-which(small_male[,1] == "6" | small_male[,2] == "6"),] # '6' participant had only 1 neighbor, so it removed from analysis
small_male <- small_male[-which(small_male[,1] == "7" | small_male[,2] == "7"),] # '7' participant had only 1 neighbor, so it removed from analysis
# creating df with edges only
vec1 <- ifelse(small_male[,3] == "1", small_male[,1],"0")
vec1_nozero <- vec1[vec1 != "0"]
vec2 <- ifelse(small_male[,3] == "1", small_male[,2],"0")
vec2_nozero <- vec2[vec2 != "0"]
d <- data.frame(vec1_nozero, vec2_nozero)
count_circles(d)


## Q3 - 0 circles
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
small_male <- name_to_number(small_male)
small_male <- small_male[-which(small_male[,1] == "4" | small_male[,2] == "4"),] # '4' participant has no neighbors, so it removed from analysis
small_male <- small_male[-which(small_male[,1] == "2" | small_male[,2] == "2"),] # '2' participant had only 1 neighbor, so it removed from analysis
small_male <- small_male[-which(small_male[,1] == "6" | small_male[,2] == "6"),] # '6' participant had only 1 neighbor, so it removed from analysis
small_male <- small_male[-which(small_male[,1] == "7" | small_male[,2] == "7"),] # '7' participant had only 1 neighbor, so it removed from analysis
small_male <- directed_to_undirected_q3(small_male, FALSE)
edges <- creating_edges(small_male)
graph <- graph(edges, directed = F)
graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] 





