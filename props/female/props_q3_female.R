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
# Q3: version == FALSE
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 91){
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

neighbors <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
    if( df[i,3] == "1" && df[i,2] == v){
      vec[i] <- df[i,1]
    }
  }
  if (is.null(vec)){ #if v has no neighbors -> return "0"
    return("0")
  }
  return(vec[!is.na(vec)])
}

# input: edge - pair of vertices of df - dataframe
# output: TRUE - if there is no edge in the graph for a pair of vertices,
# FALSE - if there is an edge in the graph for a pair of vertices

#the function check if there are no edges in the graph for a pair of vertices

is_edge_2 <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i,1] == edge[1] && df[i,2] == edge[2] && df[i,3] == "0"){
      return(TRUE)
    }
  }
  return(FALSE)
}

# counting the potential edges which could exist in our actual graph
is_any_edge <-  function(df,v){
  count <- 0
  new_vec <- neighbors(df,v)
  for(j in 1:length(new_vec)){
    for(k in 1:length(new_vec)){
      if( is_edge_2(edge = c(new_vec[j], new_vec[k]), df)){
        count <- count + 1
      }
    }
  }
  return(count)
}

#input: df - data frame (need to be an undirected graph), n - number of vertices

diff_rates <- function(df,n){
  l <- list()
  for (i in 1:n){
    l[i] <- is_any_edge(df,i)
  }
  return(l)
}

#proportions
get_prop <- function(data,n){
  total <- c()
  prop <- c()
  for (i in 1:n){
    total[i] <- choose(length(neighbors(data,i)), 2)
    prop[i] <- (total[i] - diff_rates(data, n)[[i]])/ total[i]
  }
  if(any(is.na(prop) == TRUE)){
    prop[which(is.na(prop))] <- mean(prop, na.rm = TRUE) 
  }
  return(prop)
}

Clicks_origin_women <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
#sub df - relevant columns
Clicks_women <- Clicks_origin_women[,c(1,2,25)]
#Sorted Male df
Clicks_sorted_women <- arrange(Clicks_women, Subject)
Clicks_sorted_women <- name_to_number(Clicks_sorted_women)
Clicks_sorted_women <- directed_to_undirected_q3(Clicks_sorted_women, FALSE)
Clicks_sorted_women <- arrange(Clicks_sorted_women, Subject)

get_prop(Clicks_sorted_women, 10)

prop_vec_Q3_female <- get_prop(Clicks_sorted_women, 10)
shapiro.test(prop_vec_Q3_female)

#according to the results we can imply that the distribution of the data are significantly different from normal distribution (p value < 0.05)

# Question 3 - Given a graph G = (V,E) we chose a vertex v_i. What is the probability that v_j and v_k are connect by two-sided edge where both v_j,v_k have two-sided edge with v_i?
alpha <- 0.05
n1 <- nrow(Clicks_sorted_women)
n2 <- nrow(Clicks_sorted_women)
Q3_p2_hat <- mean(get_prop(Clicks_sorted_women, 10))
Q3_p1_hat <- (sum(Clicks_sorted_women$click0no1yes == "1"))/nrow(Clicks_sorted_women)
Q3_p_hat <- (n1*Q3_p1_hat + n2*Q3_p2_hat)/(n1+n2)

# H0: p1 = p2 - null hypothesis
# H1: p2 != p1 - research hypothesis
z_observed_Q3 = ((Q3_p1_hat- Q3_p2_hat) - 0)/sqrt(Q3_p_hat*(1-Q3_p_hat)*((1/n1)+(1/n2)))
z_observed_Q3
z_observed_Q3 <= -qnorm(1-(alpha/2)) | z_observed_Q3 >= qnorm(1-(alpha/2)) #the difference is not significant at 5%

p_val_Q3 = 2*pnorm(z_observed_Q3,lower.tail = TRUE)
p_val_Q3


