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

# pvalue for cc and modularity
pvalue_1 <- function(data, real_value){
  vec <- ifelse(data <= real_value, 1,0)
  return(1-mean(vec))
}

#pvalue for aspl (continuous values)
pvalue_2 <- function(data, real_value){
  vec <- ifelse(data >= real_value, 0,1)
  return(mean(vec))
}

#pvalue for  diameter (discrete values)
pvalue_3 <- function(data, real_value){
  vec <- ifelse(data > real_value, 1,0)
  return(mean(vec))
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
    mod[i] <- modularity(g, membership = c(1:7), directed = FALSE)
  }
  df <- data.frame(cc, aspl_directed, aspl_undirected, diameter_directed, mod)
  colnames(df) <- c("CCrand","ASPL_directed_rand", "ASPL_undirected_rand", "Diameter_directed_rand", "Modularityrand")
  return(df)
}

#Data Frame
Clicks_origin_men_and_women <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
#Sub df - Male only
Clicks_men <- Clicks_origin_men_and_women[c(1:42),c(1,2,16)]
#Sorted Male df
Clicks_sorted_men <- arrange(Clicks_men, ï..Participant)

sim_results <- sim_1(Clicks_sorted_men)

library(writexl)
write_xlsx(sim_results,"C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//measures//shuffle_dataset2_male.xlsx")


# simulation data set 
sim_res_dataset2 <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset2_male.csv")


# cc hist
cc_hist <- ggplot(sim_res_dataset2,
                  aes(x= ï..CCrand)
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
  scale_x_continuous(breaks = seq(0.5,1.1,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.7),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=0.71,
            y=0.26,
            label="0.7") + 
  coord_cartesian(ylim = c(0, 0.31), xlim = c(0.5, 1.1)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Clustering Coefficient (CC) in the shuffle") 
cc_hist

pvalue_1(sim_res_dataset2$ï..CCrand, 0.7)

# aspl directed hist
aspl_directed_hist <- ggplot(sim_res_dataset2,
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
  scale_x_continuous(breaks = seq(1.1,1.95,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.619),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=1.65,
            y=0.5,
            label="1.619") + 
  coord_cartesian(ylim = c(0, 0.6), xlim = c(1.1, 1.95)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Average shortest path length (ASPL) in directed graph") 
aspl_directed_hist

pvalue_2(sim_res_dataset2$ASPL_directed_rand, 1.619)

# aspl undirected hist
aspl_undirected_hist <- ggplot(sim_res_dataset2,
                               aes(x= ASPL_undirected_rand)
) +
  geom_histogram(bins = 10,
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
  scale_x_continuous(breaks = seq(0.95,1.4,0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.238),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=1.255,
            y=0.25,
            label="1.238") + 
  coord_cartesian(ylim = c(0, 0.4), xlim = c(0.95, 1.4)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Average shortest path length (ASPL) in undirected graph") 
aspl_undirected_hist

pvalue_2(sim_res_dataset2$ASPL_undirected_rand, 1.238)


# diameter directed hist
diameter_directed_hist<- ggplot(sim_res_dataset2,
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
  scale_x_continuous(breaks = seq(1,6,1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 4),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=4.1,
            y=0.95,
            label="4") + 
  coord_cartesian(ylim = c(0, 1), xlim = c(1, 6)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of diameter in directed graph") 
diameter_directed_hist

pvalue_3(sim_res_dataset2$Diameter_directed_rand, 4)

# modularity hist
modularity_hist <- ggplot(sim_res_dataset2,
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
  scale_x_continuous(breaks = seq(-0.17, -0.14, 0.01),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = -0.151),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=-0.1499,
            y=0.22,
            label="-0.151") + 
  coord_cartesian(ylim = c(0, 0.25), xlim = c(-0.17, -0.14)) +
  theme(legend.position = "none") 
#ggtitle("Frequency of Clustering Coefficient (CC) in the shuffle") 
modularity_hist

pvalue_1(sim_res_dataset2$Modularityrand, -0.151)
pvalue_2(sim_res_dataset2$Modularityrand, -0.151)

