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
      } else if(data[i,j] == "W617") {
        data[i,j] <- "3"
      } else if(data[i,j] == "W622") {
        data[i,j] <- "4"
      } else if(data[i,j] == "W623") {
        data[i,j] <- "5"
      } else if(data[i,j] == "W674") {
        data[i,j] <- "6"
      } else if(data[i,j] == "W682") {
        data[i,j] <- "7"
      } else if(data[i,j] == "W764") {
        data[i,j] <- "8"
      } else if(data[i,j] == "W776") {
        data[i,j] <- "9"  
      } else if(data[i,j] == "W778") {
        data[i,j] <- "10"
      }
    }
  }
  return(data)
}

# function which return a data frame that represent an undirected graph (removing duplicated edges)
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 73){
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
  g_vec <- data$click0no1yes
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

Clicks_origin_women <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
#sub df - relevant columns
Clicks_women <- Clicks_origin_women[,c(1,2,25)]
#Sorted Women df
Clicks_sorted_women <- arrange(Clicks_women, Subject)
Clicks_sorted_women <- name_to_number(Clicks_sorted_women)
Clicks_sorted_women <- Clicks_sorted_women[-c(5, 12, 23, 32, 41, 46:54, 60, 69, 78, 87),]


# simulation 
B <- 10000
circles <- c()
sim_3 <- function(data){
  data <- name_to_number(data)
  data <- directed_to_undirected_q3(data, version = FALSE)
  for (i in 1:B){
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    graph <- graph(edges, directed = F)
    circles[i] <-graph.motifs(graph,size=3)[length(graph.motifs(graph,size=3))] ##??adding 
  }
  return(circles)
}

sim_results <- data.frame("number of circles" = c(sim_3(Clicks_sorted_women))) 

# histogram
p3_female <- ggplot(sim_results, aes(x=number.of.circles,
                                    fill= factor(ifelse(number.of.circles== "9","Highlighted","Normal")))) + 
  scale_fill_manual(name = "9", values=c("springgreen3","white")) +
  geom_histogram(bins = 100, aes(y= after_stat(count / sum(count))), colour= "black")+
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(sim_results$number.of.circles), sd = sd(sim_results$number.of.circles)), 
    lwd = 0.65, 
    col = 'midnightblue'
  ) +
  theme_bw() +
  xlab("Number of Circles") + ylab("Frequency") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks=c(0:10)) + 
  ggtitle("Frequency of Number of circles in the shuffle")
p3_female


#pvalue
9 > quantile(sim_results$number.of.circles, 0.95)
pvalue(sim_results, 9)

library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - sim_data_q3.xlsx")



