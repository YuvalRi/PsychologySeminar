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

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  if (all(df[df$�..Participant == v,3]) == "0"){
    vec[v] <- 0
  }
  return(vec[!is.na(vec)])
}

# input: edge - pair of vertices of df - dataframe
# output: TRUE - if there is no edge in the graph for a pair of vertices,
# FALSE - if there is an edge in the graph for a pair of vertices

#the function check if there are no edges in the graph for a pair of vertices

is_edge_2 <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i, 1] == edge[1] && df[i, 2] == edge[2] && df[i, 3] == "0"){
      return(TRUE)
    }
  }
  return(FALSE)
}

# counting the potential edges which could exist in our actual graph
is_any_edge <-  function(df, v){
  count <- 0
  new_vector <- neighbors_directed(df, v)
  for(j in 1:length(new_vector)){
    for(k in 1:length(new_vector)){
      if(is_edge_2(edge = c(new_vector[j], new_vector[k]), df)){
        count <- count + 1
      }
    }
  }
  return(count)
}

#input: df - data frame, n - number of vertices

diff_rates <- function(df, n){
  l <- list()
  for (i in 1:n){
    l[i] <- is_any_edge(df, i)
  }
  return(l)
}

#proportions
get_prop_directed <- function(data,n){
  total <- c()
  prop <- c()
  for (i in 1:n) {
    if(neighbors_directed(data,i) != "0"){
    total[i] <- (length(neighbors_directed(data,i)))*((length(neighbors_directed(data,i)))-1)
    prop[i] <- (total[i] - diff_rates(data, n)[[i]]) / total[i]
    }
    else {
      prop[i] <- NA
    }
  }
  if(any(is.na(prop) == TRUE)){
    prop[which(is.na(prop))] <- mean(prop, na.rm = TRUE) 
  }
  return(prop)
}


#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42), c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, �..Participant)
Clicks_sorted_men <- name_to_number(Clicks_sorted_men)
Clicks_sorted_men <- arrange(Clicks_sorted_men, �..Participant)

get_prop_directed(Clicks_sorted_men, 7)

#Normality test

prop_vec_Q2 <- get_prop_directed(Clicks_sorted_men, 7)
shapiro.test(prop_vec_Q2)

#according to the results we can imply that the distribution of the data are significantly different from normal distribution (p value < 0.05)

#two sided proportions Z-test for 2 populations 
alpha <- 0.05

# Question 2 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by one edge where both v_j,v_k are v_i's neighbors (there is an edge from v_i to v_j,v_k)?
n1 <- nrow(Clicks_sorted_men)
n2 <- nrow(Clicks_sorted_men)
Q2_p2_hat <- mean(prop_vec_Q2)
Q2_p1_hat <- (sum(Clicks_sorted_men$click_1yes_0no == "1"))/n1 #number of edges in the given graph 
Q2_p_hat <- (n1*Q2_p1_hat + n2*Q2_p2_hat)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2 != p1 - research hypothesis
# Z-test for 2 population proportions
z_observed_Q2 <- ((Q2_p1_hat- Q2_p2_hat) - 0)/(sqrt(Q2_p_hat*(1-Q2_p_hat)*((1/n1)+(1/n2))))
z_observed_Q2
z_observed_Q2 <=  -qnorm(1-(alpha/2)) | z_observed_Q2 >= qnorm(1-(alpha/2))

p_val_Q2 = 2*pnorm(z_observed_Q2,lower.tail = FALSE) #the difference is not significant at 5%
p_val_Q2






