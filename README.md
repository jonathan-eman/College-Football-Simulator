# College-Football-Simulator

I conducted the analysis in this repository as part of two courses, STAT 4996: Capstone and STAT 4800: Advanced Sports Analytics. The data analyzed includes over 100 variables describing every play that took place in the 2019 NCAA College Football season.

In the first course, my group sought to understand when a team should transition from a conservative to aggressive offensive playing strategy, depending on a number of game conditions including down, field position and score differential.
In the second course, I continued to build on this foundation to assess when it is beneficial to go for it in field goal range rather than kicking a field goal.

The analysis techniques leveraged include distribution fitting, markov chains, monte carlo simulations, and logistic regression, and all programming was done in R. 

Included in this repository are the final deliverables for each of these courses, as well as the scripts used for data exploration and visualization, distribution fitting and simulations.

Here is a guide to these files:
* STAT 4996: Capstone
  * [Capstone Executive Summary:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/Capstone%20Executive%20Summary.pdf) A high-level, non-technical overview of our results and their implications for the game of College Football
  * [Capstone Technical Writeup:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/Capstone%20Technical%20Writeup.pdf) An in-depth report featuring our statistical methods (distributions, models, simulations) and results. 
  * [run_full_drive:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4996-Scripts/run_full_drive.R) An R script that simulates a full offensive drive based on fitted distributions
  * [game_simulator:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4996-Scripts/game_simulator.R) An R script that loops full_drive repeatedly until a complete game has been played and a winner has been determined
  * [monte_carlo_game:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4996-Scripts/monte_carlo_game.R) An R script that simulates 10,000 games and determines the win probability based on various starting conditions
* STAT 4800: Advanced Sports Analytics
  * [STAT 4800 Final Project:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT%204800%20Final%20Project.pdf) A writeup of the results of our exploration of when teams should go for it rather than kicking a field goal
  * [distributions:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4800-Scripts/distributions.R) An R script that fits probability distributions to various scenarios in order to build mixed models for our simulations
  * [expected_points:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4800-Scripts/expected_points.R) An R script that uses a monte carlo simulation to calculate the expected points scored in various game scenarios
  * [field_goal_probability:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4800-Scripts/field_goal_probability.R) An R script that contains a field goal probability model, as well as the probability of making a field goal from different field positions
  * [final_project_basic_drive:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4800-Scripts/final_project_basic_drive.R) An R script that simulates a full offensive drive based on more extensive fitted distributions
  * [fourth_down_go_for_it_success:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4800-Scripts/fourth_down_go_for_it_success.R) An R script that uses a monte carlo simulation to calculate the probability of successfully converting a 4th down in various game scenarios
  * [score_simulator:](https://github.com/jonathan-eman/College-Football-Simulator/blob/main/STAT-4800-Scripts/score_simulator.R) An R scipt that loops the drive function repeatedly until a team scores
