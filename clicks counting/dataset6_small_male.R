library(tidyverse)
library(dplyr)


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
  print(count)
}

# v is the subject 
partner_click_with_subject <- function(df,v){
  count <- 0
  for(i in 1:7){
    if (v %in% neighbors_directed(df,i)){
      count <- count + 1
    }
  }
  print(count)
}


small_male <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//Dataset6Corrected3columns.csv")
small_male <- name_to_number(small_male)


# subject's clicks
length(neighbors_directed(small_male,1))
length(neighbors_directed(small_male,2))
length(neighbors_directed(small_male,3))
length(neighbors_directed(small_male,4))
length(neighbors_directed(small_male,5))
length(neighbors_directed(small_male,6))
length(neighbors_directed(small_male,7))


# mutual clicks
mutual_clicks(small_male,1)
mutual_clicks(small_male,2)
mutual_clicks(small_male,3)
mutual_clicks(small_male,4)
mutual_clicks(small_male,5)
mutual_clicks(small_male,6)
mutual_clicks(small_male,7)


# partner's click with subject
partner_click_with_subject(small_male,1)
partner_click_with_subject(small_male,2)
partner_click_with_subject(small_male,3)
partner_click_with_subject(small_male,4)
partner_click_with_subject(small_male,5)
partner_click_with_subject(small_male,6)
partner_click_with_subject(small_male,7)


