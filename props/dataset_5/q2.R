library(tidyverse)
library(dplyr)
library(igraph)

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

# input: df - dataframe, v - vertex
# output: vector of all neighbors of the vertex v

neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  if (all(df[df$Subject == v,3] == "0")){
    vec[v] <- 0
  }
  return(unique(vec[!is.na(vec)]))
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
    total[i] <- (length(neighbors_directed(data,i)))*((length(neighbors_directed(data,i)))-1)
    prop[i] <- (total[i] - diff_rates(data, n)[[i]]) / total[i]
  }
  if(any(is.na(prop) == TRUE)){
    prop[which(is.na(prop))] <- mean(prop, na.rm = TRUE) 
  }
  return(prop)
}


#Data Frame
small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- arrange(small_female, Subject)
small_female <- name_to_number(small_female)
small_female <- arrange(small_female, Subject)

get_prop_directed(small_female, 6)


#Normality test

prop_vec_Q2 <- get_prop_directed(small_female, 6)
shapiro.test(prop_vec_Q2)

#according to the results we can imply that the distribution of the data are significantly different from normal distribution (p value < 0.05)

#two sided proportions Z-test for 2 populations 
alpha <- 0.05

# Question 2 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by one edge where both v_j,v_k are v_i's neighbors (there is an edge from v_i to v_j,v_k)?
n1 <- nrow(small_female)
n2 <- nrow(small_female)
p_chance <- (sum(small_female$click0no1yes == "1"))/n1 #number of edges in the given graph 
p_click <- mean(prop_vec_Q2)
Q2_p_hat <- (n1*p_chance + n2*p_click)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2 != p1 - research hypothesis
# Z-test for 2 population proportions
z_observed_Q2 <- ((p_chance- p_click) - 0)/(sqrt(Q2_p_hat*(1-Q2_p_hat)*((1/n1)+(1/n2))))
z_observed_Q2
z_observed_Q2 <=  -qnorm(1-(alpha/2)) | z_observed_Q2 >= qnorm(1-(alpha/2))

p_val_Q2 = 2*pnorm(z_observed_Q2,lower.tail = TRUE) 
p_val_Q2

## UNSIGNIFICANT





