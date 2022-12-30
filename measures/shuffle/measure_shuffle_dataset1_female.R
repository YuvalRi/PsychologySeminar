library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)
library(httpgd)

name_to_number <- function(data) {
  "
  A function which converts participants names into numbers
  "
  for (j in 1:2){
    for (i in 1:(nrow(data))){
      if (data[i, j] == "W396")  {
        data[i, j] <- "1"
      } else if (data[i, j] == "W515") {
        data[i, j] <- "2"
      } else if (data[i, j] == "W617") {
        data[i, j] <- "3"
      } else if (data[i, j] == "W622") {
        data[i, j] <- "4"
      } else if (data[i, j] == "W623") {
        data[i, j] <- "5"
      } else if (data[i, j] == "W674") {
        data[i, j] <- "6"
      } else if (data[i, j] == "W682") {
        data[i, j] <- "7"
      } else if (data[i, j] == "W764") {
        data[i, j] <- "8"
      } else if (data[i, j] == "W776") {
        data[i, j] <- "9"
      } else if (data[i, j] == "W778") {
        data[i, j] <- "10"
      }
    }
  }
  return(data)
}

# input - data frame, output - vector of characters
creating_edges <- function(data) {
  "
  A function which creates a vector of edges (a pair of numbers)
  from a given dataframe
  "
  edges_vec <- c()
  for (i in 1:nrow(data)) {
    if (data[i, 3] == 1) {
      edges_vec <- append(edges_vec, data[i, 1])
      edges_vec <- append(edges_vec, data[i, 2])
    }
  }
  return(edges_vec)
}

sample_values <- function(data) {
  "
  A function which sample 0 or 1 randomly,
  1 = A click (=edge)
  0 = No click (=no edge)
  "
  clicks_vec <- data[, 3] # third column in the data set
  n <- length(clicks_vec)
  new_vec <- c() # an empty vector
  seq_vec <- sample(c(1:n), n, replace = FALSE) # sampling
  for (i in 1:length(clicks_vec)){ # inserts the random vetcor into new_vec
    new_vec[i] <- clicks_vec[seq_vec[i]]
  }
  return(new_vec)
}

pv_right_tail <- function(data, real_value) {
  vec <- ifelse(data < real_value, 1, 0)
  return(1 - mean(vec))
}

pv_left_tail <- function(data, real_value) {
  vec <- ifelse(data > real_value, 0, 1)
  return(mean(vec))
}

#shuffle
n_sim <- 10000
cc <- c()
aspl_directed <- c()
aspl_undirected <- c()
diameter_directed <- c()
mod <- c()
sim_1 <- function(data) {
  data <- name_to_number(data)
  for (i in 1:n_sim){
    data[, 3] <- sample_values(data)
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

# Data set
origin_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
# Female columns only
dataset_1 <- origin_dataset[, c(1, 2, 25)]
# Sorted Women df
sorted_dataset_1 <- arrange(dataset_1, Subject)

sim_results <- sim_1(sorted_dataset_1)

library(writexl)
write_xlsx(sim_results, "C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//measures//shuffle_dataset1_female_new.xlsx")

# simulation data set
#laptop
sim_res_dataset1_test <- read.csv("C:\\Users\\yuval\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset1_female_new.csv")

#computer
#sim_res_dataset1_test <- read.csv("C:\\Users\\Coffe\\OneDrive\\english folder\\Seminar - clicks\\datasets created by simulations\\measures\\shuffle_dataset1_female_new.csv")

hgd()
hgd_browse()
cc_hist <- ggplot(sim_res_dataset1_test,
                              aes(x = CCrand)) +
  geom_histogram(bins = 14,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Clustering Coefficient (CC)") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(0.4, 1.2, 0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 0.803, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
          x = 0.87,
          y = 0.27,
          label = "0.803",
          color = "black",
          size = 5) +
  labs(x = "Global CC", y = "Frequency", size = 34, family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.32), xlim = c(0.45, 1)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
cc_hist

pv_right_tail(sim_res_dataset1_test$CCrand, 0.803)

aspl_directed_hist <- ggplot(sim_res_dataset1_test,
                  aes(x = ASPL_directed_rand)) +
  geom_histogram(bins = 14,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - directed graph") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(1.35, 1.8, 0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.578, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = 1.63,
            y = 0.58,
            label = "1.578",
            color = "black",
            size = 5) +
  labs(x = "ASPL - directed graph",
       y = "Frequency",
      size = 34,
      family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.7), xlim = c(1.3, 1.8)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
aspl_directed_hist

pv_left_tail(sim_res_dataset1_test$ASPL_directed_rand, 1.578)

aspl_undirected_hist <- ggplot(sim_res_dataset1_test,
                              aes(x = ASPL_undirected_rand)) +
  geom_histogram(bins = 14,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("ASPL - undirected graph") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(1, 1.5, 0.1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 1.311, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = 1.365,
            y = 0.3,
            label = "1.311",
            color = "black",
            size = 5) +
  labs(x = "ASPL - undirected graph",
      y = "Frequency",
      size = 34,
      family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.35), xlim = c(0.99, 1.5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
aspl_undirected_hist

pv_left_tail(sim_res_dataset1_test$ASPL_undirected_rand, 1.311)


diameter_directed_hist <- ggplot(sim_res_dataset1_test,
                               aes(x = Diameter_directed_rand)) +
  geom_histogram(bins = 5,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Diameter - directed graph") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(1, 5, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = 3, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
          x = 3.15,
          y = 0.95,
          label = "3",
          color = "black",
          size = 5) +
  labs(x = "Diameter - directed graph",
       y = "Frequency",
       size = 34,
       family = "Helvetica") +
  coord_cartesian(ylim = c(0, 1.1), xlim = c(1, 5)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
diameter_directed_hist

pv_left_tail(sim_res_dataset1_test$Diameter_directed_rand, 3)

modularity_hist <- ggplot(sim_res_dataset1_test,
                  aes(x = Modularityrand)) +
  geom_histogram(bins = 16,
                 aes(y = after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Modularity") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 17),
        aspect.ratio = 1) +
  scale_x_continuous(breaks = seq(-0.12, -0.095, 0.01),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = -0.115, size = 0.3),
             color = "#c72727",
             linetype = "dashed",
             size = 0.8) +
  annotate("text",
            x = -0.1115,
            y = 0.25,
            label = "-0.115",
            color = "black",
            size = 5) +
  labs(x = "Modularity",
       y = "Frequency",
       size = 34,
       family = "Helvetica") +
  coord_cartesian(ylim = c(0, 0.31), xlim = c(-0.121, -0.095)) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
modularity_hist

pv_left_tail(sim_res_dataset1_test$Modularityrand, -0.115)
