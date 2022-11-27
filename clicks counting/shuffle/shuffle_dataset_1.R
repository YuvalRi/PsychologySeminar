source("measures/shuffle/measure_shuffle_dataset1_female.R")
source("clicks counting/dataset1_female.R")
library(tidyverse)
library(dplyr)
library(igraph)
library(writexl)

#' The simulation creates random graphs.
#' In each graph, for each participant
#' both number of mutual clicks and
#' number of neighbors were calculated.
#' The simulation returns a data frame
#' with the results of the calculations.
simulation <- function(dataset) {
  n_sim <- 10000
  mutual_random <- rep(0, 100000)
  neighbors_random <- rep(0, 100000)
  dataset <- name_to_number(dataset)
  for (i in 1:n_sim) {
    if (i %% 1000 == 0) 
      cat("Progress: ", i / 100, "%\n")
    dataset[, 3] <- sample_values(dataset)
    edges <- creating_edges(dataset)
    graph <- graph(edges, directed = TRUE)
    num_of_vertices <- gorder(graph)
    for (v in 1:num_of_vertices) {
      index <- (i - 1) * num_of_vertices + v
      mutual_random[index] <- mutual_clicks(dataset, v)
      neighbors_random[index] <- length(neighbors_directed(dataset, v))
    }
  }
  df <- data.frame(mutual_random, neighbors_random)
  colnames(df) <- c("mutual clicks", "participant's clicks")
  return(df)
}

# Origin dataset
origin_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)

dataset_1 <- origin_dataset[, c(1, 2, 25)]
dataset_1 <- arrange(dataset_1, Subject)
sim_results <- simulation(dataset_1)

write_xlsx(sim_results, "C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset1.xlsx")
sim_results_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset1.csv")


mean_calc <- function(dataset) {
  mean_mutual_clicks <- rep(0, 10000)
  mean_participants_clicks <- rep(0, 10000)
  for (i in 1:nrow(dataset)) {
    mean_mutual_clicks[i] <- mean(dataset[i:i + 9, 1])
    mean_participants_clicks[i] <- mean(dataset[i:i + 9, 2])
  }
  return(data.frame(mean_mutual_clicks, mean_participants_clicks))
}

mean_vec_1 <- mean_calc(sim_results_dataset)

View(mean_vec_1)