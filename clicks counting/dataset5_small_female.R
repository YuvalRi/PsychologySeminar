library(tidyverse)
library(dplyr)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "W396"){
        data[i,j] <- "1"
      } else if(data[i,j] == "W515") {
        data[i,j] <- "2"
      } else if(data[i,j] == "W686") {
        data[i,j] <- "3"
      } else if(data[i,j] == "W717") {
        data[i,j] <- "4"
      } else if(data[i,j] == "W755") {
        data[i,j] <- "5"
      } else if(data[i,j] == "W756") {
        data[i,j] <- "6"
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
  for(i in 1:6){
    if ((v %in% neighbors_directed(df,i)) && (i %in% neighbors_directed(df,v))){
      count <- count + 1
    }
  }
  print(count)
}


partner_click_with_subject <- function(df,v){
  count <- 0
  for(i in 1:6){
    if (v %in% neighbors_directed(df,i)){
      count <- count + 1
    }
  }
  print(count)
}

small_female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//small_female_subset.csv")
small_female <- name_to_number(small_female)


# subject's clicks
length(neighbors_directed(small_female,1))
length(neighbors_directed(small_female,2))
length(neighbors_directed(small_female,3))
length(neighbors_directed(small_female,4))
length(neighbors_directed(small_female,5))
length(neighbors_directed(small_female,6))


# mutual clicks
mutual_clicks(small_female,1)
mutual_clicks(small_female,2)
mutual_clicks(small_female,3)
mutual_clicks(small_female,4)
mutual_clicks(small_female,5)
mutual_clicks(small_female,6)


# partner's click with subject
partner_click_with_subject(small_female,1)
partner_click_with_subject(small_female,2)
partner_click_with_subject(small_female,3)
partner_click_with_subject(small_female,4)
partner_click_with_subject(small_female,5)
partner_click_with_subject(small_female,6)




