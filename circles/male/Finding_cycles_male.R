library(tidyverse)
library(dplyr)
library(igraph)

#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42),c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, ï..Participant)

#number of each participant: 1 - M419, 2- M485, 3- M599, 4 - M620, 5 - M626, 6 - M665, 7 - M670
C1 <- c("1","2","3","4","5","6","7")
#mutual clicks
C2 <- c(2,2,2,3,4,1,2)
#subject's clicks
C3 <- c(2,3,3,5,6,1,4)
#partner's clicks with subject
C4 <- c(5,2,3,3,4,4,3)
#male table of all clicks 
data_men <- tibble( "subject" = C1, "mutual clicks" = C2, "subject's clicks" = C3, "partner's clicks with subject" = C4)
data_men
#male - directed graph
g_men <- graph(edges = c("1","2", "1","6", "2","1", "2","5", "2","7", "3","1", "3","4", "3","5", "4","1", "4","3", "4","5", "4","6", "4","7", "5","1", "5","2", "5","3", "5","4", "5","6", "5","7", "6","1", "7","3", "7","4", "7","5", "7","6") ,directed = TRUE)

#graph plot with the required color - 'orange' 
set.seed(1234)
g <- plot(g_men, layout = layout_with_graphopt, edge.arrow.size = 0.2, vertex.color="orange", vertex.label.color="black", vertex.frame.color="orange", vertex.label.cex = 0.6,vertex.size = 25, edge.color = "black")


#Q1
#male - undeirected graph
g_men_undirected <- graph(edges = c("1","2", "1","3", "1","4", "1","5", "1","6", "2","5", "2","7", "3","4", "3","5", "3","7", "4","5", "4","6", "4","7", "5","6", "5","7", "6","7"),directed = FALSE)
#number of cycles of 3 edegs 
graph.motifs(g_men_undirected,size=3)[length(graph.motifs(g_men_undirected,size=3))]

#Q3 
#male - undirected graph with two sided edges only 
g_men_Q3 <- graph(edges = c("1","2", "1","6", "2","5", "3","4", "3","5", "4","5", "4","7", "5","7") ,directed = FALSE)
plot(g_men_Q3)
#number of cycles of 3 edegs 
graph.motifs(g_men_Q3,size=3)[length(graph.motifs(g_men_Q3,size=3))]


