# E[Opp 1st & 10 at 25]
monte_carlo <- replicate(10000,
                         score_simulator(fp = 25,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep

# E[Opp 1st & 10 at FP-7]
monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(60:68), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_60_68

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(69:77), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_69_77

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(78:86), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_78_86

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(87:94), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_87_94

monte_carlo <- replicate(10000,
                         score_simulator(fp = 100-sample(c(95:100), 1) + 7,
                                         ytg = 10,
                                         down = 1,
                                         pos = 1
                         ))

mean(monte_carlo) -> ep_95_100

# E[1st & 10 at FP+YTG] (after we convert on 4th down)
# 60-68 FP, 0-1 YTG
replicate(10000,
          score_simulator(fp = sample(60:68, 1) + 1,
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# 60-68 FP, 2-3 YTG
replicate(10000,
          score_simulator(fp = sample(60:68, 1) + sample(2:3, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()


# 60-68 FP, 4-5 YTG
replicate(10000,
          score_simulator(fp = sample(60:68, 1) + sample(4:5, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# 60-68 FP, 6-7 YTG
replicate(10000,
          score_simulator(fp = sample(60:68, 1) + sample(4:5, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# 60-68 FP, 8-10 YTG
replicate(10000,
          score_simulator(fp = sample(60:68, 1) + sample(4:5, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# 95-100 FP, 0-1 YTG
replicate(10000,
          score_simulator(fp = sample(95:100, 1) + sample(0:1, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# 95-100 FP, 2-3 YTG
replicate(10000,
          score_simulator(fp = sample(95:100, 1) + sample(2:3, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# 95-100 FP, 4-5 YTG
replicate(10000,
          score_simulator(fp = sample(95:100, 1) + sample(4:5, 1),
                          ytg = 10,
                          down = 1,
                          pos = 0
          )) %>% mean()

# E[Opp 1st & 10 at FP] (after we convert on 4th down)
# 60-68 FP
replicate(10000,
          score_simulator(fp = 100-sample(60:68, 1),
                          ytg = 10,
                          down = 1,
                          pos = 1
          )) %>% mean()

# 69-77 FP
replicate(10000,
          score_simulator(fp = 100-sample(69:77, 1),
                          ytg = 10,
                          down = 1,
                          pos = 1
          )) %>% mean()

# 78-86 FP
replicate(10000,
          score_simulator(fp = 100-sample(78:86, 1),
                          ytg = 10,
                          down = 1,
                          pos = 1
          )) %>% mean()

# 87-94 FP
replicate(10000,
          score_simulator(fp = 100-sample(87:94, 1),
                          ytg = 10,
                          down = 1,
                          pos = 1
          )) %>% mean()

# 95-100 FP
replicate(10000,
          score_simulator(fp = 100-sample(95:100, 1),
                          ytg = 10,
                          down = 1,
                          pos = 1
          )) %>% mean()
