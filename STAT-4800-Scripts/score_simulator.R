# setwd("~/Fall 2021 Courses/STAT 4800/AdvancedSportsAnalytics")
library(tidyverse)
source('final_project_basic_drive.R')
source('field_goal_probability.R')


#This function loops and repeatedly calls the function full_drive until someone scores.
score_simulator <- function(fp, ytg, down, strat4th="NA", pos) {
  team <- 0 #start with team 0
  scoreless <- TRUE
  
  while(scoreless){
    
    drive_result <- full_drive(fp, ytg, down, strat4th, pos)
    strat4th_initial <- strat4th
    
    if(!is.na(drive_result$score)) {
      scoreless <- FALSE
      break
    }
    
    if(is.na(drive_result$score)) {
      
      # give the other team the ball where the previous team left off
      pos <- (pos+1)%%2
      down <- 100 - drive_result$end_yard
      ytg <- 10
      down <- 1
      strat4th <- ifelse(pos == 1, "NA", strat4th_initial)
    }
  }
    score <- ifelse(
      pos == 0,
      drive_result$score,
      -1*drive_result$score
    )
    
    return(score)
}
