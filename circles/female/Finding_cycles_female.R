library(tidyverse)
library(dplyr)
library(igraph)

# function for counting all circles in Q2
count_circles <- function(df){
  count <- 0 
  vec <- c(1,2,3,4,5,7,8,9,10) # the vertices in the graph
  for (v in 1:9){ 
    for(w in 1:8){
      if (w == v){
        next
      }
      for(z in (w+1):9){
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

Clicks_origin_women <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
#sub df - relevant columns
Clicks_women <- Clicks_origin_women[,c(1,2,25)]
#Sorted Women df
Clicks_sorted_women <- arrange(Clicks_women, Subject)
#number of each participant: 1 - W396, 2- W515, 3- W617, 4 - W622, 5 - W623, 6 - W674, 7 - W682, 8 - W764, 9 - W776, 10 - W778
C1 <- c("1","2","3","4","5","6","7","8","9","10")
#mutual clicks
C2 <- c(0,2,1,2,5,1,4,5,5,3)
#subject's clicks
C3 <- c(3,4,1,5,6,1,6,6,7,6)
#partner's clicks with subject
C4 <- c(3,3,7,2,8,1,5,7,5,4)
#women table of all clicks 
data_women <- tibble( "subject" = C1, "mutual clicks" = C2, "subject's clicks" = C3, "partner's clicks with subject" = C4)
data_women
#women - directed graph
g_women <- graph(edges = c("1","3", "1","7", "1","8", "4","1", "5","1", "9","1", "2","8", "2","9", "2","3", "2","5", "8","2", "9","2", "10","2", "3","5", "5","3", "7","3", "8","3", "9","3", "10","3", "4","5", "4","7", "4","8", "4","10", "5","4", "7","4", "5","6", "5","8", "5","9", "6","5", "7","5", "8","5", "9","5", "10","5", "7","8", "7","9", "7","10", "8","7", "9","7", "10","7", "8","9", "8","10", "9","8", "10","8", "10","9", "9","10") ,directed = TRUE)
#graph plot with the required color - 'purple' 
set.seed(1234)
figure1 <- plot(g_women, layout = layout_with_graphopt, edge.arrow.size = 0.3, vertex.color="purple", vertex.label.color="black", vertex.frame.color="purple", vertex.label.cex = 0.8,vertex.size = 25, edge.color = "black", main = "Figure 1")

#Q1
#women - undirected graph
g_women_undirected <- graph(edges = c("1","3", "1","7", "1","8", "4","1", "5","1", "9","1", "2","8", "2","9", "2","3", "2","5", "8","2", "9","2", "10","2", "3","5", "5","3", "7","3", "8","3", "9","3", "10","3", "4","5", "4","7", "4","8", "4","10", "5","4", "7","4", "5","6", "5","8", "5","9", "6","5", "7","5", "8","5", "9","5", "10","5", "7","8", "7","9", "7","10", "8","7", "9","7", "10","7", "8","9", "8","10", "9","8", "10","8", "10","9", "9","10") ,directed = FALSE)
#number of cycles of 3 edegs 
graph.motifs(g_women_undirected,size=3)[length(graph.motifs(g_women_undirected,size=3))]


#Q2
# counting circles in the graph (directed graph)

#women - undirected graph with two sided edges only 
g_women_Q3 <- graph(edges = c("1","8", "8","2", "9","2", "5","3", "5","4", "7","4", "6","5", "7","5", "8","5", "9","5", "7","8", "7","9", "7","10", "9","8", "10","8", "9","10") ,directed = FALSE)
#number of cycles of 3 edegs 
graph.motifs(g_women_Q3,size=3)[length(graph.motifs(g_women_Q3,size=3))]


