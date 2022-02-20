library(tidyverse)

fourth_down_go_for_it <- function(fp, ytg) {
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

fp = fp + yards_gained
# print(paste("New fp is:", fp))
ytg = ytg - yards_gained
# print(paste("New ytg is:", ytg))

success <- ifelse(ytg <= 0, 1, 0)
return(success)
}

# 0-1
replicate(10000, 
          fourth_down_go_for_it(fp = sample(60:68, 1),
                                ytg = 1)) %>% mean()
replicate(10000, 
          fourth_down_go_for_it(fp = sample(69:77, 1),
                                ytg = 1)) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(78:86, 1),
                                ytg = 1)) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(87:94, 1),
                                ytg = 1)) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(95:100, 1),
                                ytg = 1)) %>% mean()

# 2-3
replicate(10000, 
          fourth_down_go_for_it(fp = sample(60:68, 1),
                                ytg = sample(2:3, 1))) %>% mean()
replicate(10000, 
          fourth_down_go_for_it(fp = sample(69:77, 1),
                                ytg = sample(2:3, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(78:86, 1),
                                ytg = sample(2:3, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(87:94, 1),
                                ytg = sample(2:3, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(95:100, 1),
                                ytg = sample(2:3, 1))) %>% mean()

# 4-5
replicate(10000, 
          fourth_down_go_for_it(fp = sample(60:68, 1),
                                ytg = sample(4:5, 1))) %>% mean()
replicate(10000, 
          fourth_down_go_for_it(fp = sample(69:77, 1),
                                ytg = sample(4:5, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(78:86, 1),
                                ytg = sample(4:5, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(87:94, 1),
                                ytg = sample(4:5, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(95:100, 1),
                                ytg = sample(4:5, 1))) %>% mean()



# 6-7
replicate(10000, 
          fourth_down_go_for_it(fp = sample(60:68, 1),
                                ytg = sample(6:7, 1))) %>% mean()
replicate(10000, 
          fourth_down_go_for_it(fp = sample(69:77, 1),
                                ytg = sample(6:7, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(78:86, 1),
                                ytg = sample(6:7, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(87:94, 1),
                                ytg = sample(6:7, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(95:100, 1),
                                ytg = sample(6:7, 1))) %>% mean()

# 8-10
replicate(10000, 
          fourth_down_go_for_it(fp = sample(60:68, 1),
                                ytg = sample(8:10, 1))) %>% mean()
replicate(10000, 
          fourth_down_go_for_it(fp = sample(69:77, 1),
                                ytg = sample(8:10, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(78:86, 1),
                                ytg = sample(8:10, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(87:94, 1),
                                ytg = sample(8:10, 1))) %>% mean()

replicate(10000, 
          fourth_down_go_for_it(fp = sample(95:100, 1),
                                ytg = sample(8:10, 1))) %>% mean()
