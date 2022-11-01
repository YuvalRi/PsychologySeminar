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

# pvalue for cc  
pvalue_1 <- function(data, real_value){
  vec <- ifelse(data <= real_value, 1,0)
  return(1-mean(vec))
}

# pvalue for aspl (continuous values) and modularity
pvalue_2 <- function(data, real_value){
  vec <- ifelse(data >= real_value, 0,1)
  return(mean(vec))
}

#pvalue for  diameter (discrete values)
pvalue_3 <- function(data, real_value){
  vec <- ifelse(data >= real_value, 1, 0)
  return(1-mean(vec))
}


#shuffle 
B <- 10000
cc <- c()
aspl_directed <- c()
aspl_undirected <- c()
diameter_directed <- c()
mod <- c()
sim_1 <- function(data){
  data <- name_to_number(data)
  for (i in 1:B){
    data[,3] <- sample_values(data)
    edges <- creating_edges(data)
    g <- graph(edges, directed = TRUE)
    cc[i] <- transitivity(g, type = c("global"))
    aspl_directed[i] <- mean_distance(g, directed = TRUE)
    aspl_undirected[i] <- mean_distance(g, directed = FALSE)
    diameter_directed[i] <- diameter(g, directed = TRUE)
    mod[i] <- modularity(g, membership = c(1:10), directed = FALSE)
  }
  df <- data.frame(cc, aspl_directed, aspl_undirected, diameter_directed, mod)
  colnames(df) <- c("CCrand","ASPL_directed_rand", "ASPL_undirected_rand", "Diameter_directed_rand", "Modularityrand")
  return(df)
}

#df
Clicks_origin_women <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
#sub df - relevant columns
Clicks_women <- Clicks_origin_women[,c(1,2,25)]
#Sorted Women df
Clicks_sorted_women <- arrange(Clicks_women, Subject)

sim_results <- sim_1(Clicks_sorted_women)

library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//measures//shuffle_dataset1_female_new.xlsx")

# simulation data set 
#sim_res_dataset1 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset1_female.csv")

sim_res_dataset1_test <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset1_female_new.csv")
# cc hist
cc_hist <- ggplot(sim_res_dataset1_test,
                              aes(x= CCrand)
) +
  geom_histogram(bins = 14,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Clustering Coefficient (CC)") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(0.5,1,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.803),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=0.825,
            y=0.27,
            label="0.803") + 
  coord_cartesian(ylim = c(0, 0.32), xlim = c(0.5, 1)) +
  theme(legend.position = "none") 
  #ggtitle("Frequency of Clustering Coefficient (CC) in the shuffle") 
cc_hist

pvalue_1(sim_res_dataset1_test$CCrand, 0.803)

# aspl directed hist
aspl_directed_hist <- ggplot(sim_res_dataset1,
                  aes(x= ASPL_directed_rand)
) +
  geom_histogram(bins = 10,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - directed graph") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(1.35,1.8,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.578),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=1.6,
            y=0.5,
            label="1.578") + 
  coord_cartesian(ylim = c(0, 0.6), xlim = c(1.35, 1.8)) +
  theme(legend.position = "none") 
  #ggtitle("Frequency of Average shortest path length (ASPL) in directed graph") 
aspl_directed_hist

pvalue_2(sim_res_dataset1_test$ASPL_directed_rand, 1.578)

# aspl undirected hist
aspl_undirected_hist <- ggplot(sim_res_dataset1,
                             aes(x= ASPL_undirected_rand)
) +
  geom_histogram(bins = 14,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - undirected graph") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(1,1.5,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.311),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=1.33,
            y=0.25,
            label="1.311") + 
  coord_cartesian(ylim = c(0, 0.3), xlim = c(1, 1.5)) +
  theme(legend.position = "none") 
  #ggtitle("Frequency of Average shortest path length (ASPL) in undirected graph") 
aspl_undirected_hist

pvalue_2(sim_res_dataset1_test$ASPL_undirected_rand, 1.311)


# diameter directed hist
diameter_directed_hist<- ggplot(sim_res_dataset1,
                               aes(x= Diameter_directed_rand)
) +
  geom_histogram(bins = 5,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Diameter - directed graph") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(1,5,1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 3),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=3.1,
            y=0.95,
            label="3") + 
  coord_cartesian(ylim = c(0, 1), xlim = c(1, 5)) +
  theme(legend.position = "none") 
  #ggtitle("Frequency of diameter in directed graph") 
diameter_directed_hist

pvalue_3(sim_res_dataset1_test$Diameter_directed_rand, 3)

# modularity hist
modularity_hist <- ggplot(sim_res_dataset1,
                  aes(x= Modularityrand)
) +
  geom_histogram(bins = 14,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Modularity") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(-0.12,-0.095, 0.01),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = -0.115),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=-0.114,
            y=0.2,
            label="-0.115") + 
  coord_cartesian(ylim = c(0, 0.25), xlim = c(-0.12, -0.095)) +
  theme(legend.position = "none") 
  #ggtitle("Frequency of Clustering Coefficient (CC) in the shuffle") 
modularity_hist

#pvalue_1(sim_res_dataset1_test$Modularityrand, -0.115)
pvalue_2(sim_res_dataset1_test$Modularityrand, -0.115)
