library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)


# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M567"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M597") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M607") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M614" | data[i,j] == "M614 ") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M615") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M632") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M633") {
        data[i,j] <- "7"
      }
    }
  }
  return(data)
}

# function which return a data frame that represent an undirected graph (removing duplicated edges)
# Q1: version == TRUE
# for Q1
directed_to_undirected_q3 <- function(df, version){
  
  df_undirected <- data.frame() # TODO: CREATE A NEW DF
  
  for( i in 1:nrow(df)){
    for( j in (i+1):nrow(df)){
      if (j == 17){
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


neighbors <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
    if( df[i,3] == "1" && df[i,2] == v){
      vec[i] <- df[i,1]
    }
    if (all(df[df$Subject == v,3] == "0")){
      vec[v] <- 0
    }
  }
  return(unique(vec[!is.na(vec)]))
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
  p_vec <- ifelse(data[,8] >= real_value, 0, 1)
  return(mean(p_vec))
}

# shuffle simulation 
# n - number of vertices 
B <- 10000
diff_mat <- list()
sim_1_diff <- function(data, n){
  data <- name_to_number(data)
  data <- directed_to_undirected_q3(data, version = TRUE)
  for (i in 1:B){
    if ( i %% 100 == 0)
      cat("progress: ", i / 100, "%\n")
    data[, 3] <- sample_values(data)
    diff_mat[[i]] <- diff_rates(data, n)
  }
  sim_1_diff_results <- as.data.frame(do.call(rbind, diff_mat))
  return(sim_1_diff_results)
}

# df
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
sim_res_1 <- sim_1_diff(small_male, 7)

# saving simulation results in excel file
library(rio)
export(sim_res_1, "sim_1_diff_q1_small_males.xlsx")

# the differences in our graph
small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_male_subset.csv")
small_male <- name_to_number(small_male)
small_male <- directed_to_undirected_q3(small_male,TRUE)
small_male <- arrange(small_male, ï..Participant)
diff_rates(small_male, 7)
mean(as.numeric(diff_rates(small_male, 7)))


# diff data

diff_data_q1_small_males <- read_csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//datasets created by simulations//diff//sim_1_diff_q1_small_males.csv")

# adding column - mean of the differences of each row

diff_data_q1_small_males$mean <- rowMeans(diff_data_q1_small_males, na.rm=TRUE)

# histogram
diff_hist_q1_small_male <- ggplot(diff_data_q1_small_males,
                                    aes(x= mean)
) +
  geom_histogram(bins = 4,
                 aes(y= after_stat(count / sum(count))),
                 fill = "gray63",
                 colour = "black") +
  theme_bw() +
  ylab("Frequency") +
  xlab("Differences") +
  theme(plot.title = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 15),
        aspect.ratio=1) +
  scale_x_continuous(breaks = seq(0,0.7,0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_vline(aes(xintercept = mean(as.numeric(diff_rates(small_male, 7)))),
             color="dodgerblue2",
             linetype="dashed",
             size=1) +
  geom_text(x=0.03,
            y=0.4,
            label="0.142") + 
  coord_cartesian(ylim = c(0, 0.45), xlim = c(-0.1, 0.7)) +
  theme(legend.position = "none") +
  ggtitle("Frequency of differences in the shuffle") 
diff_hist_q1_small_male


#pvalue
pvalue(diff_data_q1_small_males, mean(as.numeric(diff_rates(small_male, 7))))

## UNSIGNIFICANT  


