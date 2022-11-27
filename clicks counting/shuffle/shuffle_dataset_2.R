source("measures/shuffle/measure_shuffle_datatset2_male.R")
source("clicks counting/dataset2_male.R")
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
  n_sim <- 10
  mutual_random <- rep(0, 100)
  neighbors_random <- rep(0, 100)
  dataset <- name_to_number(dataset)
  for (i in 1:n_sim) {
    if (i %% 1000 == 0) 
      cat("Progress: ", i / 100, "%\n")
    dataset[, 3] <- sample_values(dataset)
    edges <- creating_edges(dataset)
    graph <- graph(edges, directed = TRUE)
    num_of_vertices <- gorder(graph)
    print(num_of_vertices)
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

origin_dataset <- read.csv("C:/Users/yuval/OneDrive/english folder/Seminar - clicks/ClicksMales.csv", header = TRUE)
dataset_2 <- origin_dataset[c(1:42), c(1, 2, 16)]
dataset_2 <- arrange(dataset_2, Participant)

sim_results <- simulation(dataset_2)

write_xlsx(sim_results, "C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset21.xlsx")
sim_results_dataset <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//clicks_counting//shuffle_dataset2.csv")
