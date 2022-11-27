library(tidyverse)
library(dplyr)

# function for converting names of participants to numebrs
name_to_number <- function(data) {
  for(j in 1:2){
    for (i in 1:nrow(data)){
      if (data[i,j] == "M629"){
        data[i,j] <- "1"
      } else if(data[i,j] == "M634") {
        data[i,j] <- "2"
      } else if(data[i,j] == "M646") {
        data[i,j] <- "3"
      } else if(data[i,j] == "M647") {
        data[i,j] <- "4"
      } else if(data[i,j] == "M648") {
        data[i,j] <- "5"
      } else if(data[i,j] == "M650") {
        data[i,j] <- "6"
      } else if(data[i,j] == "M651") {
        data[i,j] <- "7"
      } else if(data[i,j] == "M652") {
        data[i,j] <- "8"
      } else if(data[i,j] == "M653") {
        data[i,j] <- "9"  
      } else if(data[i,j] == "M654") {
        data[i,j] <- "10"
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
  for(i in 1:10){
    if ((v %in% neighbors_directed(df,i)) && (i %in% neighbors_directed(df,v))){
      count <- count + 1
    }
  }
  return(count)
}


partner_click_with_subject <- function(df,v){
  count <- 0
  for(i in 1:10){
    if (v %in% neighbors_directed(df,i)){
      count <- count + 1
    }
  }
  return(count)
}

males_45 <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//more datasets//45males_subset_new.csv")
males_45 <- name_to_number(males_45)


# subject's clicks
length(neighbors_directed(males_45,1))
length(neighbors_directed(males_45,2))
length(neighbors_directed(males_45,3))
length(neighbors_directed(males_45,4))
length(neighbors_directed(males_45,5))
length(neighbors_directed(males_45,6))
length(neighbors_directed(males_45,7))
length(neighbors_directed(males_45,8))
length(neighbors_directed(males_45,9))
length(neighbors_directed(males_45,10))

# mutual clicks
mutual_clicks(males_45,1)
mutual_clicks(males_45,2)
mutual_clicks(males_45,3)
mutual_clicks(males_45,4)
mutual_clicks(males_45,5)
mutual_clicks(males_45,6)
mutual_clicks(males_45,7)
mutual_clicks(males_45,8)
mutual_clicks(males_45,9)
mutual_clicks(males_45,10)

# partner's click with subject
partner_click_with_subject(males_45,1)
partner_click_with_subject(males_45,2)
partner_click_with_subject(males_45,3)
partner_click_with_subject(males_45,4)
partner_click_with_subject(males_45,5)
partner_click_with_subject(males_45,6)
partner_click_with_subject(males_45,7)
partner_click_with_subject(males_45,8)
partner_click_with_subject(males_45,9)
partner_click_with_subject(males_45,10)






