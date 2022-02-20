#This function loops and repeatedly calls the function full_drive until someone scores.
#One of the main things this function does is keep track of which team has the ball.
game_simulator <- function(A, B, C, D, E, F, t=NA) {
   team <- 0 #start with team 0
   F_0 <- F
   # k <- 0 
   score <- list(team_0 = 0, team_1 = abs(E))
   game_is_active <- TRUE
   
   while(game_is_active){
      
      result_drive <- full_drive(A, B, C, D, E, F)
      
      if(result_drive$event == "End of Game") {
         game_is_active <- FALSE
      }
      
      if(!is.na(result_drive$score)) {
         # print(paste0("team ", team, " scored!")) 
         score[team+1] <- as.numeric(score[team+1]) + result_drive$score
         
         # other team gets the ball
         team <- (team+1)%%2
         
         A <- 75 # assume every kickoff is a touchback
         B <- result_drive$end_time - .2 # assume kickoff takes 12 seconds
         C <- 10
         D <- 1
         F <- ifelse(team == 1, 0, F) # Team 1 is always non-aggressive
         
      } else {
         # give the other team the ball where the previous team left off
         team <- (team+1)%%2 
         
         A <- 100 - result_drive$end_yard
         B <- result_drive$end_time
         C <- 10
         D <- 1
         F <- ifelse(team == 1, 0, F_0)
         
         # k <- k+1 #play counter in case things get out of hand.
      }
      
      # Change to aggressive if t argument is given
      if (!is.na(t) & team == 0 & B <= t) {
          F <- 1
          F_0 <- 1
          #print(paste("F is", F))
          #print("Aggressive now")
      }
   }
   
   print(paste0("The final score is: Team 0 - ", score[1], ", Team 1 - ",
                score[2]))
   
  return(c("Team_0" = score[[1]], "Team_1" = score[[2]]))
}

