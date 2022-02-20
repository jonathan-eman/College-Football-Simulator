# # Test variables
# fp <- 25
# ytg <- 10
# down <- 1
# strat4th <- 0
# pos <- 0

full_drive <- function(fp, ytg, down, strat4th, pos){
  
  # fp is starting field position of play (ranges from 0 to 100 (so your own 25 would be 25. 100 would be a touchdown))
  # ytg is yards to first down
  # down is down number
  # strat4th is 4th down strategy (0 if FG case, 1 going for it, NA other team has ball)
  # pos is team that starts with possession ()
  
  is_not_done <- TRUE
  pos <- pos # start with "our team" on offense
  
  drive_result <- list(event = NA, score = NA, end_yard = NA, pos = pos)
  
  while (is_not_done) {
    ### 1st or 2nd down
    if (down %in% c(1,2)) {
      
      ### If in back redzone
      if (fp <= 20) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.439, .561))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.594, .328, .078))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.4612, rate = 0.1145) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.5707, scale = 5.3136)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.809, .191))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.4612, sdlog = .1145) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
        
      } else if (between(fp, 20, 80)) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.507, .493))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.567, .362, .071))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.533, rate = 0.1178)
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.532, scale = 5.516)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.813, .187))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.509, sdlog = .895) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
        
      } else if (fp > 80) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.371, .629))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.487, .449, .064))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = ifelse(
              ytg <= 3,
              rlnorm(1, meanlog = 1.092, sdlog = .82),
              rgamma(1, shape = 2.603, rate = 0.301)
            )
          } else if (pos_neg == "NEG") {
            yards_gained = -rgamma(1, shape = 1.992, rate = .4133)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.813, .187))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.509, sdlog = .895) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
      }
    }
    
    ### 3rd down
    if (down == 3) {
      
      ### If in back redzone
      if (fp <= 20) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.747, .253))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.488, .405, .106))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.676, rate = 0.1212) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.644, scale = 5.988)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.815, .185))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.2056, rate = 0.1612) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rlnorm(1, meanlog = .7313, sdlog = .6045)
          }
          
        }
        
        
      } else if (between(fp, 20, 80)) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.708, .292))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.5107, .39, .0989))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rgamma(1, shape = 1.7538, rate = .1367) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rweibull(1, shape = 1.7498, scale = 6.6857)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.793, .207))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rweibull(1, shape = 1.014, scale = 6.905)
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .410)
          }
          
        }
        
        
      } else if (fp > 80) {
        play_type_prob <- sample.int(2, size = 1, prob = c(.601, .399))
        play_type <- c("P", "R")[play_type_prob]
        if (play_type == "P") {
          int <- sample.int(3, size = 1, 
                            prob = c(.452, .473, .075))
          pos_neg <- c("POS", "0", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = ifelse(
              ytg <= 3,
              rlnorm(1, meanlog = 1.383, sdlog = .8102),
              rgamma(1, shape = 2.643, rate = 0.310)
            ) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rlnorm(1, meanlog = 1.5396, sdlog = .7784)
          } else {
            yards_gained = 0 
          }
          
        } else if (play_type == "R") {
          int <- sample.int(2, size = 1, 
                            prob = c(.762, .238))
          pos_neg <- c("POS", "NEG")[int]
          if (pos_neg == "POS") {
            yards_gained = rlnorm(1, meanlog = 1.1019, sdlog = .818) 
          } else if (pos_neg == "NEG") {
            yards_gained = -rexp(1, rate = .482)
          }
          
        }
        
      }
    }
    
    ### 4th down
    if (down == 4) {
      if (fp < 60) {
        # print("Out of FG range")
        # Add punt distributions (this is a placeholder)
        punt_yards <- ifelse(
          fp < 40,
          rnorm(1, 40.5, 9.7),
          rnorm(1, 28.5, 6.7)
        )
        # print(punt_yards)
        drive_result$end_yard <- ifelse(fp + punt_yards >= 100,
                                        75,
                                        fp + punt_yards)
        drive_result$event <- "Punt"
        # print(is_not_done)
        is_not_done <- FALSE
        break
        # print(paste("is_not_done is now", is_not_done))
        
      }
      else if (fp >= 60) {
        # Always kick field goal
        if (strat4th == 0) {
          prob <- field_goal_probability(fp)
          field_goal <- sample.int(2, size = 1, prob = c(prob, 1 - prob))
          if (field_goal == 1) {
            drive_result$score <- 3
            drive_result$event <- "FG"
          } else {
            drive_result$score <- NA
            drive_result$end_yard <- fp-7 # move back 7 yards
            drive_result$event <- "Missed FG"
          }
          is_not_done <- FALSE
          break
        }
        # Go for it
        else if (strat4th == 1) {
          if (between(fp, 60, 80)) {
          play_type_prob <- sample.int(2, size = 1, prob = c(.583, 1-.583))
          play_type <- c("P", "R")[play_type_prob]
            if (play_type == "P") {
              int <- sample.int(3, size = 1, 
                                prob = c(.489, .437, .073))
              pos_neg <- c("POS", "0", "NEG")[int]
              if (pos_neg == "POS") {
                yards_gained = rgamma(1, shape = 1.784, rate = .1315)
              } else if (pos_neg == "NEG") {
                yards_gained = -rweibull(1, shape = 1.77, scale = 8.232)
              } else {
                yards_gained = 0 
              }
            } else if (play_type == "R") {
              int <- sample.int(2, size = 1, 
                                prob = c(.715, 1-.715))
              pos_neg <- c("POS", "NEG")[int]
              if (pos_neg == "POS") {
                yards_gained = rweibull(1, shape = 0.941, scale = 5.973)
              } else if (pos_neg == "NEG") {
                yards_gained = -rlnorm(1, meanlog = 0.847, sdlog = 0.797)
              }
            }
            
          }
            else {
              play_type_prob <- sample.int(2, size = 1, prob = c(.465, 1-.465))
              play_type <- c("P", "R")[play_type_prob]
              if (play_type == "P") {
                int <- sample.int(3, size = 1, 
                                  prob = c(.45, .49, .06))
                pos_neg <- c("POS", "0", "NEG")[int]
                if (pos_neg == "POS") {
                  yards_gained = ifelse(
                    ytg <= 3,
                    rlnorm(1, meanlog = 1.197, sdlog = 0.851),
                    rgamma(1, shape = 3.026, rate = 0.332)
                  ) 
                } else if (pos_neg == "NEG") {
                  yards_gained = -rgamma(1, shape = 2.471, rate = 0.374)
                } else {
                  yards_gained = 0 
                }
                
              } else if (play_type == "R") {
                int <- sample.int(2, size = 1, 
                                  prob = c(697, 1-.697))
                pos_neg <- c("POS", "NEG")[int]
                if (pos_neg == "POS") {
                  yards_gained = rlnorm(1, meanlog = 0.861, sdlog = .814) 
                } else if (pos_neg == "NEG") {
                  yards_gained = -rlnorm(1, meanlog = 0.625, sdlog = 0.572)
                }
              }
            }
        }
        # Other team
        else if (strat4th == "NA") {
          
          # determine if opp. team will go for it if within YTG range  
          go_for_it <- sample.int(2, size = 1, prob = c(.5, .5))
             if (ytg >= 3 | go_for_it == 1) {
               prob <- field_goal_probability(fp)
               field_goal <- sample.int(2, size = 1, prob = c(prob, 1 - prob))
               if (field_goal == 1) {
                 drive_result$score <- 3
                 drive_result$event <- "FG"
               } else {
                 drive_result$score <- NA
                 drive_result$end_yard <- fp-7 # move back 7 yards
                 drive_result$event <- "Missed FG"
               }
               is_not_done <- FALSE
               break
             } else if (ytg < 3 & go_for_it == 0) {
               
                 play_type_prob <- sample.int(2, size = 1, prob = c(.465, 1-.465))
                 play_type <- c("P", "R")[play_type_prob]
                 if (play_type == "P") {
                   int <- sample.int(3, size = 1, 
                                     prob = c(.45, .49, .06))
                   pos_neg <- c("POS", "0", "NEG")[int]
                   if (pos_neg == "POS") {
                     yards_gained = ifelse(
                       ytg <= 3,
                       rlnorm(1, meanlog = 1.197, sdlog = 0.851),
                       rgamma(1, shape = 3.026, rate = 0.332)
                     ) 
                   } else if (pos_neg == "NEG") {
                     yards_gained = -rgamma(1, shape = 2.471, rate = 0.374)
                   } else {
                     yards_gained = 0 
                   }
                   
                 } else if (play_type == "R") {
                   int <- sample.int(2, size = 1, 
                                     prob = c(697, 1-.697))
                   pos_neg <- c("POS", "NEG")[int]
                   if (pos_neg == "POS") {
                     yards_gained = rlnorm(1, meanlog = 0.861, sdlog = .814) 
                   } else if (pos_neg == "NEG") {
                     yards_gained = -rlnorm(1, meanlog = 0.625, sdlog = 0.572)
                   }
               }
             }
          }
        }
      } 
    
    fp = fp + yards_gained
    # print(paste("New fp is:", fp))
    ytg = ytg - yards_gained
    # print(paste("New ytg is:", ytg))
    down = ifelse(ytg <= 0, 1, down + 1)
    # print(paste("New down:", down))
    
    if (fp >= 100) {
      drive_result$score <- 7
      drive_result$event <- "Touchdown"
      # print("Touchdown")
      is_not_done <- FALSE
      break
    }
    
    if (down == 1) { #if first down, reset YTG to 10
      ytg = 10
      # print("First down")
    }
    
    if (down == 5) { # turnover on downs
      drive_result$end_yard <- fp
      drive_result$event <- "Turnover on Downs"
      # print("TOD")
      is_not_done <- FALSE
    }
        
  }
  
  return(drive_result)
}
