library(tidyverse)
library(dplyr)

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


neighbors_directed <- function(df, v){
  vec <- c()
  for(i in 1:nrow(df)){
    if( df[i,3] == "1" && df[i,1] == v){
      vec[i] <- df[i,2]
    }
  }
  return(unique(vec[!is.na(vec)]))
}

mutual_clicks <- function(df,v){
  count <- 0
  for(i in 1:7){
    if ((v %in% neighbors_directed(df,i)) && (i %in% neighbors_directed(df,v))){
      count <- count + 1
    }
  }
  return(count)
}

partner_click_with_subject <- function(df,v){
  count <- 0
  for(i in 1:7){
    if (v %in% neighbors_directed(df,i)){
      count <- count + 1
    }
  }
  return(count)
}


male <- read.csv("C://Users//yuval//Desktop//english folder//Seminar - clicks//ClicksMales.csv", header = TRUE)
male <- male[c(1:42), c(1,2,16)]
# sorted male df
male <- arrange(male, Participant)
male <- name_to_number(male)

# subject's clicks
length(neighbors_directed(male,1))
length(neighbors_directed(male,2))
length(neighbors_directed(male,3))
length(neighbors_directed(male,4))
length(neighbors_directed(male,5))
length(neighbors_directed(male,6))
length(neighbors_directed(male,7))


# mutual clicks
mutual_clicks(male,1)
mutual_clicks(male,2)
mutual_clicks(male,3)
mutual_clicks(male,4)
mutual_clicks(male,5)
mutual_clicks(male,6)
mutual_clicks(male,7)


# partner's click with subject
partner_click_with_subject(male,1)
partner_click_with_subject(male,2)
partner_click_with_subject(male,3)
partner_click_with_subject(male,4)
partner_click_with_subject(male,5)
partner_click_with_subject(male,6)
partner_click_with_subject(male,7)

