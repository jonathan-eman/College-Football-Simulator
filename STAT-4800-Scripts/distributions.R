library(tidyverse)
library(fitdistrplus)

setwd("~/Fall 2021 Courses/STAT 4800")
read.csv("2019 PFF All Plays.csv") -> data

data %>%
  mutate(
    FIELDPOSITION = case_when(
      pff_FIELDPOSITION < 0 ~ pff_FIELDPOSITION * -1,
      pff_FIELDPOSITION > 0 ~ 100 - pff_FIELDPOSITION
    ),
    SIDE_OF_FIELD = ifelse(
      FIELDPOSITION >= 50, "OPPOSING", "OWN"
    ),
    FIELDZONE = case_when(
      FIELDPOSITION <= 20 ~ "OWN REDZONE",
      between(FIELDPOSITION, 20, 80) ~ "OPEN FIELD",
      FIELDPOSITION >= 80 ~ "OPPONENT REDZONE"
    )) -> data

# Determine when we need to condition on YTG for passes to account for short vs. long passes
data %>%
  group_by(pff_DISTANCE > 3,
           pff_DOWN,
           FIELDZONE) %>%
  filter(!is.na(pff_GAINLOSS),
         pff_RUNPASS == "P",
         pff_DOWN > 0,
         pff_GAINLOSS > 0) %>%
  summarize(mean(pff_GAINLOSS),
            median(pff_GAINLOSS)) %>%
  arrange(pff_DOWN, FIELDZONE) %>% View()

### 1st or 2nd down ### ----
data %>%
  filter(pff_DOWN %in% c(1,2)) -> first_second_down

# Own Redzone ----
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 43.9% pass, 56.1% run

# Pass
plotdist((first_second_down %>%
            filter(FIELDPOSITION <= 20,
                   pff_RUNPASS %in% c("P")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 59.4%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 32.8%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 7.8%
  )

# Positive Pass
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 1.4612, rate = 0.1145)

# Negative Pass
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Weibull(Shape = 1.5707, Scale = 5.3136)

# Run
plotdist((first_second_down %>%
            filter(FIELDPOSITION <= 20,
                   pff_RUNPASS %in% c("R")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 80.9%
    neg = mean(pff_GAINLOSS <= 0 & !is.na(pff_GAINLOSS))  # 19.1%
  )

# Positive Run
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Lnorm(meanlog = 1.507, rate = .915)

# Negative Run
first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS < 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

first_second_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Exp(rate = .482)

# Open Field ----
first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 50.7% pass, 49.3% run

# Pass
plotdist((first_second_down %>%
            filter(between(FIELDPOSITION, 20, 80),
                   pff_RUNPASS %in% c("P")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 56.7%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 36.2%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 7.1%
  )

# Positive Pass
first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 1.533, rate = 0.1178)

# Negative Pass
first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Weibull(Shape = 1.5322, Scale = 5.516)

# Run
plotdist((first_second_down %>%
            filter(between(FIELDPOSITION, 20, 80),
                   pff_RUNPASS %in% c("R")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 81.3%
    neg = mean(pff_GAINLOSS <= 0 & !is.na(pff_GAINLOSS))  # 18.7%
  )

# Positive Run
first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Lnorm(meanlog = 1.509, sdlog = .895)

# Negative Run
first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS < 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

first_second_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Exp(rate = .482)

# Opponent Redzone ----
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 37.1% pass, 62.9% run


# Pass
plotdist((first_second_down %>%
            filter(FIELDPOSITION >= 80,
                   pff_RUNPASS %in% c("P")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 48.7%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 44.9%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 6.4%
  )

# Positive Pass (YTG > 3)
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 2.603, rate = 0.301)

# Positive Pass (YTG <= 3)
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Lognormal(meanlog = 1.092, sdlog = 0.820)

# Negative Pass
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Gamma(Shape = 1.992, Rate = .4133)

# Run
plotdist((first_second_down %>%
            filter(FIELDPOSITION >= 80,
                   pff_RUNPASS %in% c("R")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 77.9%
    neg = mean(pff_GAINLOSS <= 0 & !is.na(pff_GAINLOSS))  # 22.1%
  )

# Positive Run
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Lnorm(meanlog = 1.165, sdlog = .803)

# Negative Run
first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS < 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

first_second_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Exp(rate = .440)


### 3rd down ### ----
data %>%
  filter(pff_DOWN == 3) -> third_down

# Own Redzone ----
third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 74.7% pass, 25.3% run

# Pass
plotdist((third_down %>%
            filter(FIELDPOSITION <= 20,
                   pff_RUNPASS %in% c("P")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 48.8%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 40.5%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 10.6%
  )

# Positive Pass
third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 1.676, rate = 0.1212)

# Negative Pass
third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Weibull(Shape = 1.644, Scale = 5.988)

# Run
plotdist((third_down %>%
            filter(FIELDPOSITION <= 20,
                   pff_RUNPASS %in% c("R")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 81.4%
    neg = mean(pff_GAINLOSS <= 0 & !is.na(pff_GAINLOSS))  # 18.6%
  )

# Positive Run
third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 1.2056, rate = 0.1612)

# Negative Run
third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS < 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

third_down %>%
  filter(FIELDPOSITION <= 20,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Lnorm(meanlog = .7313, sdlog = .6045)


# Open Field ----
third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 70.8% pass, 29.2% run

# Pass
plotdist((third_down %>%
            filter(between(FIELDPOSITION, 20, 80),
                   pff_RUNPASS %in% c("P")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 51.1%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 39.0%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 9.9%
  )

# Positive Pass
third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 1.7538, rate = .1367)

# Negative Pass
third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Weibull(Shape = 1.7498, Scale = 6.6857)

# Run
plotdist((third_down %>%
            filter(between(FIELDPOSITION, 20, 80),
                   pff_RUNPASS %in% c("R")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 79.3%
    neg = mean(pff_GAINLOSS <= 0 & !is.na(pff_GAINLOSS))  # 20.7%
  )

# Positive Run
third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Weibull(shape = 1.014, scale = 6.905)

# Negative Run
third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS < 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

third_down %>%
  filter(between(FIELDPOSITION, 20, 80),
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Exp(rate = .410)

# Opponent Redzone ----
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R", "P")
  ) %>%
  summarize(mean(pff_RUNPASS == "P")) # 60.1% pass, 39.9% run


# Pass
plotdist((third_down %>%
            filter(FIELDPOSITION >= 80,
                   pff_RUNPASS %in% c("P")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 45.1%
    inc = mean(pff_GAINLOSS == 0 | is.na(pff_GAINLOSS)), # 47.3%
    neg = mean(pff_GAINLOSS < 0 & !is.na(pff_GAINLOSS))  # 7.5%
  )

# Positive Pass

# YTG > 3
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE > 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Gamma(shape = 2.643, rate = 0.310)

# YTG <= 3
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS),
         pff_DISTANCE <= 3
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Lnorm(meanlog = 1.3833, sdlog = 0.8102)

# Negative Pass
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("P"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# -Lognormal(meanlog = 1.5396, sdlog = .7784)

# Run
plotdist((third_down %>%
            filter(FIELDPOSITION >= 80,
                   pff_RUNPASS %in% c("R")
            ))$pff_GAINLOSS,
         histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R")
  ) %>%
  summarize(
    pos = mean(pff_GAINLOSS > 0 & !is.na(pff_GAINLOSS)), # 76.2%
    neg = mean(pff_GAINLOSS <= 0 & !is.na(pff_GAINLOSS))  # 23.8%
  )

# Positive Run
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS > 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  pull(pff_GAINLOSS) %>%
  fitdist("lnorm") -> fln

plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Lnorm(meanlog = 1.1019, sdlog = .818)

# Negative Run
third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS < 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  plotdist(histo = TRUE, demp = TRUE)

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("weibull") -> fw

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("gamma") -> fg

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("lnorm") -> fln

third_down %>%
  filter(FIELDPOSITION >= 80,
         pff_RUNPASS %in% c("R"),
         pff_GAINLOSS< 0,
         !is.na(pff_GAINLOSS)
  ) %>% 
  mutate(loss = -1*pff_GAINLOSS) %>%
  pull(loss) %>%
  fitdist("exp") -> fexp

plot.legend <- c("Weibull", "lognormal", "gamma", "exp")
denscomp(list(fw, fln, fg, fexp), legendtext = plot.legend, plotstyle = "ggplot", xlegend = "top")

# Exp(rate = .482)

### 4th down ### ----
data %>%
  filter(pff_DOWN == 4) -> fourth_down

fourth_down %>%
  filter(pff_SPECIALTEAMSTYPE == "FIELD GOAL") %>%
  mutate(FG_MADE = ifelse(
    grepl("made", pff_KICKRESULT, ignore.case = TRUE),
    1,
    0
  )) %>% 
  dplyr::select(FIELDPOSITION, FG_MADE, pff_KICKRESULT) -> field_goals

# field goal probability model
glm(data = field_goals, FG_MADE ~ FIELDPOSITION, family = 'binomial') -> fg_prob
summary(fg_prob)

field_goal_probability <- function(x) {
  exp(-5.832 + 0.0861*(x)) / (1 + exp(-5.832 + 0.0861*(x)))
}
