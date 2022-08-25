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
# check if edge is found in df

is_edge <- function(edge, df){
  for (i in 1:nrow(df)){
    if(df[i,1] == edge[1] && df[i,2] == edge[2]){
      return(TRUE)
    }
  }
  return(FALSE)
}

# circle is defined as stated in question

is_circle <- function(v1, v2, v3, df){
  if (is_edge(edge = c(v1, v2), df) && is_edge(edge = c(v1, v3), df) && (is_edge(edge = c(v2, v3), df) | is_edge(edge = c(v3, v2), df))){
    return(TRUE)
  }
  return(FALSE)
}

# counting circles in the graph (directed graph)
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

#Df
# we want to check if our result (number of circles) is bigger than the 95% precentile
Clicks_origin_women <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
#sub df - relevant columns
Clicks_women <- Clicks_origin_women[,c(1,2,25)]
#Sorted Women df
Clicks_sorted_women <- arrange(Clicks_women, Subject)
Clicks_sorted_women <- name_to_number(Clicks_sorted_women)

# simulation 
B <- 10000 #number of iterations
circles <- c() #empty vector which will get the number of circles in each iteration (vector of length - 10000)
sim_2 <- function(data){
  data <- name_to_number(data) # changing participants name to number
  data <- data[-c(5, 12, 23, 32, 41, 46:54, 60, 69, 78, 87),] # removing 6 participant
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

sim_results <- data.frame("number of circles" = c(sim_2(Clicks_sorted_women))) 

# histogram
p2_female <- ggplot(sim_results, aes(x=number.of.circles,
                                    fill= factor(ifelse(number.of.circles== "85","Highlighted","Normal")))) + 
  scale_fill_manual(name = "85", values=c("red","white")) +
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
  #scale_x_continuous(breaks=c(30:51)) + 
  ggtitle("Frequency of Number of circles in the shuffle")
p2_female


Clicks_sorted_women <- Clicks_sorted_women[-c(5, 12, 23, 32, 41, 46:54, 60, 69, 78, 87),]


# creating df with edges only
vec1 <- ifelse(Clicks_sorted_women[,3] == "1", Clicks_sorted_women[,1],"0")
vec1_nozero <- vec1[vec1 != "0"]
vec2 <- ifelse(Clicks_sorted_women[,3] == "1", Clicks_sorted_women[,2],"0")
vec2_nozero <- vec2[vec2 != "0"]
d <- data.frame(vec1_nozero, vec2_nozero)

# checking if the number of circles in our graph is bigger than the 95% precentile
count_circles(d) > quantile(sim_results$number.of.circles, 0.95)

pvalue(sim_results, 85)

# test 
85 > quantile(sim_results$number.of.circles, 0.95)
pvalue(sim_results, 85)

# exporting sim_results to excel file
library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - sim_data_q2.xlsx")
