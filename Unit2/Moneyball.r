# MoneyBall in short is a technique to find those parameters (indp var) which can
# help in making good predictions on various fronts such as player performance,
# team performance, game winning, player stats, League winning, etc..

# Moneyball technique uses statistics and Regression models to predict these.


# The goal of the baseball teams is to get to the playoffs.

# 1. We will predict whether the team will make into playoffs based on the number of matches they won.

# 2. We'll then use linear regression to predict how many games a team will win using
# the difference between runs scored and runs allowed, or opponent runs.

# 3. We'll then use linear regression again to predict the number of runs a team will score 
# using batting statistics, and the number of runs a team will allow using fielding and pitching statistics.

# So we have 3 things ->> Playoffs -- Match Winning -- Runs Scored and Runs allowed.

# We will go from Top down prediction approach.

# Using Match winning stats to learn about Playoffs.
# Using Runs scored and allowed to learn about Match Winning.
# And using a 4th things called fielding and pitching stats to learn about Runs scored and allowed.



baseball= read.csv('baseball.csv')


str(baseball) # Data is from (1962-2012)
summary(baseball)

# We want to take only that data which Paul took while building the moneyball model (years before 2002)


moneyball = subset(baseball, baseball$Year < 2002 )

str(moneyball)
summary(moneyball)  # 1962 - 2001


# We want to predict the winning of a team using Runs scored and Runs allowed.
# Paul claimed that if RD >= 135, teams will win atleast 95 games and will qualify for the playoffs.

#Playoffs = Number of times a team made it to the playoffs. W= Number of matches they won in 162 games.
table(moneyball$W, moneyball$Playoffs)

# Paul chose 95 seeing that a team if won 95 games had a chance of 15(15+5)*100 = 75% chance of qualifying to the playoffs.


# So lets start by calculating RD
# And then predicting Winning using RD.

moneyball$RD = moneyball$RS - moneyball$RA   # RD = Run diff, RS = Runs scored, RA = Runs Allowed

str(moneyball)

# We want to see whether RD has any relationship with W (Winning)

plot(moneyball$RD, moneyball$W, col = 'orange')

# W indicates number of matches won.
# Strong Positive Linear Corelation.

# Lets build a lm() to predict Wins using RD.

WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)

# *** = Very Significant
# RSquare = 88%

# Regression Equation = Intercept + Coeff(RD)
# W = 80.88 + 0.10576(RD)


# Paul told us that a team should win atleast 95 games to make it to the playoffs.
# W >= 95
# 80.88 + 0.10576(RD) >= 95

# RD >= (95-80.88)/0.10576
# RD >= 133.5 ~ 135

# So our model's equation almost predicted that RD needs to be 135 for a team to win 95 games.


# If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team to win?

# Using the linear regression model constructed during the lecture, enter the number of games 
# we expect the team to win:

# W = 80.88 + 0.10576(713-614)
# W = 80.88 + 0.10576(99)
# W = 91.35 games



# There were 3 variables who were selected to predict the number of runs scored
# by a team.

# OBP (On base %), SLG(Slugging %), BA(Batting Avg)

# We will build a model predicting RS (RunsScored) using these 3 variables
# and will see which is the most significant among them.

RunsReg = lm(RS ~ OBP + SLG + BA, data = moneyball)
summary(RunsReg)

# We see among the variables, BA has 2 stars and OBP and SLG are equally highly
# significant.

# Also we see something contradicting in our summary. The coeff of BA is negative
# which implies that less BA indicates higher Runs.

# It happened because all the 3 variables are highly corelated among themselves.
# The problem of Multi-Collinarity.

# Lets build another model removing BA.

RunsReg = lm(RS ~ OBP + SLG, data = moneyball)
summary(RunsReg)

# This model too has RSquare almost 93%.
# Simpler model as it involved less variables.
# All coeff are also positive.
# Overall a better model than the earlier one.
# 
# Regression Equation -->> RS = -804.63 + 2737.77(OBP) + 1584.91(SLG)

# These variables are already scaled.
# So higher the coeff value, most important that variable becomes for prediction.

# OBP therefore having 2737.77 as the coeff is the most important predictor var.

# Thereforee BA is overvalued predictor var, SLG is a good predictor var and 
# OBP is the most imp predictor var.


# Similarly we can make a model using OOBP and OSLG (first O here stands for Opponent)
# to predict RA (Runs Allowed)

RunsReg2 = lm(RA ~ OOBP + OSLG, data = moneyball)

summary(RunsReg2)

# Regression Equation -->>  RA = -837.38 + 2913.60(OOBP) + 1514.27(OSLG)


# If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?

# Using RS regression eq ->> RS = -804.63 + 2737.77(OBP) + 1584.91(SLG)
# RS = -804.63 + 2737.77(0.311) + 1584.91(0.405)

# 688.705

# OOBP = 0.297 and OSLG = 0.370

# RA = -837.38 + 2913.60(0.297) + 1514.27(0.370)

# 588.239


# Till now we saw One variable L-Reg model predicting W using RD.
# And also Multiple Var L-Reg model, predicting RS and RA using OBP and SLG
# and OOBP and OSLG respectively.





# Now we know if we want to predict how many W.
# We have to predict RD.

# For that we have to predict RS and RA.

# These all for the next year using data from the previous year.

# So previous year data = Training data 
# Next year data = Test data.

# But every year team changes and to predict a team's statistics using a
# previous years stats can be difficult in the real world.

# So we assume that past and future performace corelates.
# little to no injuries.

rm(data2002)

data2001 = subset(baseball, Year == 2001)

tapply(data2001$OBP, data2001$Team, mean, na.rm = TRUE)
# Team OBP = 0.345 (given in the course = 0.339)

tapply(data2001$SLG, data2001$Team, mean, na.rm = TRUE)
# Team SLG = 0.439  (given in the course = 0.430)

# It is told to us that in the beginning of of 2002, OAK has 24 batters.

# # Our regression equation was ->> 
# RS = -804.63 + 2737.77(OBP) + 1584.91(SLG)

# By plugging in Team OBP and Team SLG,  RS = 805 runs


# For Team's OOBP and OSLG. OAK had 17 pitchers at the beginning of 2002.

tapply(data2001$OOBP, data2001$Team, mean, na.rm = TRUE)
# Team OOBP = 0.308 (given in the course = 0.307)

tapply(data2001$OSLG, data2001$Team, mean, na.rm = TRUE)
# Team OOBP = 0.380 (given in the course = 0.373)

# We see the difference in our data calculated and given in the course
# keeping in mind that some players might have been traded or retired.

# So a slight change in Team's OBP, SLG , OOBP and OSLG by adjusting it accordingly.
 
# RA = -837.38 + 2913.60(OOBP) + 1514.27(OSLG)

# Plugging in the values, RA = 622 runs.

# RD = RS - RA = 805-622 = 183

# Now using W and RD model's equation we can find how many matches will be won by OAK in 2002.

# W = 80.88 + 0.10576(183) = 100.23 ~ 100 games 

# 100 > 95 ->> Will qualify for the playoffs.



# Why teams can't predict for World Championship?
# Because after getting into playoffs, there is a sample size problem.
# Not enough games are played.

# Corelation between a Regular season win and Winning World Championship is very
# less = 0.03

# In this case, Logistic Regression can be used to predict whether the team
# will win in a World Championship or not.



