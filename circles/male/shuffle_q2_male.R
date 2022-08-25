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

# check if edge is found in df (the df shoudl include only edges that appear in the graph)

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
  vec <- c(1,2,3,4,5,7) # the vertices in the graph
  for (v in 1:6){ 
    for(w in 1:5){
      if (w == v){
        next
      }
      for(z in (w+1):6){
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

#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42),c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, ï..Participant)

Clicks_sorted_men <- name_to_number(Clicks_sorted_men)

# simulation 
# we have 19 edges over all
B <- 10000 #number of iterations
circles <- c() #empty vector which will get the number of circles in each iteration (vector of length - 10000)
sim_2 <- function(data){
  data <- name_to_number(data) # changing participants name to number
  data <- data[-c(5, 11, 17, 23, 29, 31:36, 42),] # removing 6 participant
  for (i in 1:B){ 
    data[,3] <- sample_values(data) 
    vec1 <- ifelse(data[,3] == "1", data[,1],"0")
    vec1_nozero <- vec1[vec1 != "0"]
    vec2 <- ifelse(data[,3] == "1", data[,2],"0")
    vec2_nozero <- vec2[vec2 != "0"]
    d <- data.frame(vec1_nozero, vec2_nozero) #getting the data with edges only
    circles[i] <- count_circles(d) #counting circles in each iteration
  }
  return(circles)
}

sim_results <- data.frame("number of circles" = c(sim_2(Clicks_sorted_men))) 

# histogram
p2 <- ggplot(sim_results, aes(x=number.of.circles,
                              fill= factor(ifelse(number.of.circles=="20","Highlighted","Normal")))) + 
  scale_fill_manual(name = "20", values=c("salmon3","white")) +
  geom_histogram(bins = 80, aes(y = after_stat(count / sum(count))), colour="black")+
  theme_bw() +
  xlab("Number of Circles") + ylab("Frequency") +
  theme(
    plot.title = element_text(size=15)
  ) +
  scale_x_continuous(breaks=c(12:30))+
  ggtitle("Frequency of Number of circles in the shuffle")
p2
 
# we want to check if our result (number of circles) is bigger than the 95% precentile
#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42),c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, ï..Participant)

Clicks_sorted_men <- name_to_number(Clicks_sorted_men)
Clicks_sorted_men <- Clicks_sorted_men[-c(5, 11, 17, 23, 29, 31:36, 42),]

# creating df with edges only
vec1 <- ifelse(Clicks_sorted_men[,3] == "1", Clicks_sorted_men[,1],"0")
vec1_nozero <- vec1[vec1 != "0"]
vec2 <- ifelse(Clicks_sorted_men[,3] == "1", Clicks_sorted_men[,2],"0")
vec2_nozero <- vec2[vec2 != "0"]
d <- data.frame(vec1_nozero, vec2_nozero)

# checking if the number of circles in our graph is bigger than the 95% precentile
count_circles(d) > quantile(sim_results$number.of.circles, 0.95)

pvalue(sim_results, 20)

#potential number of required circles = 3*choose(6,3)

# exporting sim_results to excel file
library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - sim_data_q2.xlsx")



