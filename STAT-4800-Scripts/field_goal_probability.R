field_goal_probability <- function(x) {
  exp(-5.832 + 0.0861*(x)) / (1 + exp(-5.832 + 0.0861*(x)))
}

# Determine average field goal probability across range of field positions
lapply(list(c(60:68)), field_goal_probability) %>% unlist() %>% mean()

lapply(list(c(69:77)), field_goal_probability) %>% unlist() %>% mean()

lapply(list(c(78:86)), field_goal_probability) %>% unlist() %>% mean()

lapply(list(c(87:94)), field_goal_probability) %>% unlist() %>% mean()

lapply(list(c(95:100)), field_goal_probability) %>% unlist() %>% mean()


