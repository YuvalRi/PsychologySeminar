library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M419"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M485") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M599") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M620") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M626") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M665") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M670") {
        data[i,j] <- "7"
      }
    }
  }
  return(data)
}
# function which return a data frame that represent an undirected graph (removing duplicated edges)
# Q3: version == FALSE
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 31){
        break
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

# converting data frame to directed graph
data_to_Dgraph <- function(data){
  data <- name_to_number(data)
  edges <- creating_edges(data)
  graph <- graph(edges, directed = T)
  return(graph)
}

# converting data frame to undirected graph
data_to_Ugraph <- function(data){
  data <- name_to_number(data)
  edges <- creating_edges(data)
  graph <- graph(edges, directed = F)
  return(graph)
}

# return a permutation of 0,1 where 1 represent an edges and 0 represent no edge
sample_values <- function(data){
  
  # original vector
  g_vec <- data$click_1yes_0no
  n <- length(g_vec)
  new_vec <- c()
  # sample permutations between [0,1] with length n
  seq_vec <- sample(c(1:n), n, replace = FALSE)
  
  for (i in 1:length(seq_vec)){
    new_vec[i] <- g_vec[seq_vec[i]]
  }
  return(new_vec)
}

# function for getting pvalue 
pvalue <- function(data, real_value){
  p_vec <- ifelse(data[,1] <= real_value, 0, 1)
  return(mean(p_vec))
}


# Question 3

#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42),c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, �..Participant)


# simulation 
B <- 10000
circles <- c()
sim_3 <- function(data){
  data <- name_to_number(data)
  data <- data[-c(5, 11, 17, 23, 29, 31:36, 42),]
  data <- directed_to_undirected_q3(data, version = FALSE)
  for (i in 1:B){
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    graph <- graph(edges, directed = F)
    circles[i] <-graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] ##??adding 
  }
  return(circles)
}

sim_results_3 <- data.frame("number of circles" = c(sim_3(Clicks_sorted_men))) 

# histogram 
p3 <- ggplot(sim_results, aes(x=number.of.circles,
                              fill= factor(ifelse(number.of.circles=="2","Highlighted","Normal")))) + 
  scale_fill_manual(name = "2", values=c("violetred","white")) +
  geom_histogram(bins = 40, aes(y = after_stat(count / sum(count))), colour="black")+
  stat_function (
    fun = dnorm, 
    args = list(mean = mean(sim_results$number.of.circles), sd = sd(sim_results$number.of.circles)), 
    lwd = 0.65, 
    col = 'midnightblue') +
  theme_bw() +
  xlab("Number of Circles") + ylab("Frequency") +
  theme( plot.title = element_text(size=15)) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5)) +
  ggtitle("Frequency of Number of circles in the shuffle")
p3 


#pvalue
2 > quantile(sim_results$number.of.circles, 0.95)
pvalue(sim_results, 2)

library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - sim_data_q3.xlsx")


#Clicks_sorted_men <- name_to_number(Clicks_sorted_men)
#Clicks_sorted_men <- Clicks_sorted_men[-c(5, 11, 17, 23, 29, 31:36, 42),]







