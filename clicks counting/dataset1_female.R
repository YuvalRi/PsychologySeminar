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
  print(count)
}


partner_click_with_subject <- function(df,v){
  count <- 0
  for(i in 1:10){
    if (v %in% neighbors_directed(df,i)){
      count <- count + 1
    }
  }
  print(count)
}

female <- read.csv("C://Users//yuval//OneDrive//english folder//Seminar - clicks//ClicksYuval.csv", header = TRUE)
# sub df - relevant columns
female <- female[,c(1,2,25)]
# sorted Women df
female <- arrange(female, Subject)
female <- name_to_number(female)

# subject's clicks
length(neighbors_directed(female,1))
length(neighbors_directed(female,2))
length(neighbors_directed(female,3))
length(neighbors_directed(female,4))
length(neighbors_directed(female,5))
length(neighbors_directed(female,6))
length(neighbors_directed(female,7))
length(neighbors_directed(female,8))
length(neighbors_directed(female,9))
length(neighbors_directed(female,10))


# mutual clicks
mutual_clicks(female,1)
mutual_clicks(female,2)
mutual_clicks(female,3)
mutual_clicks(female,4)
mutual_clicks(female,5)
mutual_clicks(female,6)
mutual_clicks(female,7)
mutual_clicks(female,8)
mutual_clicks(female,9)
mutual_clicks(female,10)


# partner's click with subject
partner_click_with_subject(female,1)
partner_click_with_subject(female,2)
partner_click_with_subject(female,3)
partner_click_with_subject(female,4)
partner_click_with_subject(female,5)
partner_click_with_subject(female,6)
partner_click_with_subject(female,7)
partner_click_with_subject(female,8)
partner_click_with_subject(female,9)
partner_click_with_subject(female,10)



