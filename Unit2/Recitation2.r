NBA = read.csv('nba_train.csv')
str(NBA)

# Note we have X2p, X2pA, x3, x3pA.
# These X were not a part of the original variables in the data-set.
# But since they are starting with a number, R automatically puts X infront of them.


table(NBA$W, NBA$Playoffs) # W = Number of matches won in the regular season.
# 0/1 for how many times they did made it into playoffs and how many times they din't.

# Note : All Unique values of W are listed.
# And their count w.r.t to playoff qualifier is displayed under 0/1.


# How to keep a threshold as how many W are needed to make it to the playoffs?

plot(jitter(NBA$W), jitter(NBA$Playoffs))
table(NBA$W, NBA$Playoffs)

# We can use plot to make a guess and table to be more data-accurate.

# We see around after 42 wins, almost all teams have gone into playoffs.

# And if you have won 42 games % chance of going into playoffs = 29/37 = 78%



# Now we will see the relation between Points difference and Winning chance.

NBA$PTSdiff = NBA$PTS - NBA$oppPTS
# This will create a vector having the point diff for each team.

# So lets look at the possible relationship between Points diff and W.

plot(NBA$PTSdiff, NBA$W)
# Too Strong positive linear correlation

cor(NBA$PTSdiff, NBA$W)
# 0.97 = 97%

# i.e, teams having more point diff have recorded more wins in the league.

# Now lets build a linear reg model to see the prediction quality (Rsquare).

WinsReg = lm(W ~ PTSdiff, data = NBA)
summary(WinsReg)

# Highly Sig *** , RSquare = 94.23% = Adj RSquare.

# Reg Eq ->> W = 4.100e+01 + 3.259e-02(PTSdiff) ->> W = 41 + 0.0326(PTSdiff)

# Now we already in the beginning set a threshold of 42 games to be qualified
# for the playoffs.

# Now using that in our Equation we can find the threshold for the PTSdiff.

# W >= 42
# 41 + 0.0326(PTSdiff) >= 42
# PTSdiff >= (42-41)/(0.0326)
# PTSdiff >= 30.67 ~ 31


# By doing directly using tapply
# Calculating mean PTSdiff for all the winning categories.
tapply(NBA$PTSdiff, NBA$W, mean, na.rm = TRUE)

# It came out to be 30.08 for teams who won 42 games. Close to our prediction.


# Now we want to build a model to predict Points scored using all the basket ball
# statistics.

PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST +
                 ORB + DRB + TOV + STL + BLK, data  = NBA)

summary(PointsReg)

# We have some Non significant variables.

# Lets calculate the SSE
# Note : residuals is a vector containing individual error of each data point w.r.t.
# the regression line.
SSE = sum(PointsReg$residuals^2)

SSE # It came out to be in millions. Not useful.

# Lets try out RMSE
RMSE = sqrt(SSE/nrow(NBA))  # nrow(NBA) will give me total numb of obs or data points.

RMSE  # 184.4049
# This means we get an error of 184 points per game.
mean(NBA$PTS)
# 8370

# %error = 184/8370 = 2.2% = Okay.


# Lets try to improve upon the model by removing some insignificant variables.
# Lets remove the var one by one.
# Starting with the one with Highest P value (that means Probab of being insig).

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST +
                 ORB + DRB + STL + BLK, data  = NBA)

# Removed TOV which was insignificant and had Highest P value. 

summary(PointsReg2)

# We see that our RSquare remains almost same (90%). 
# And removing TOV din't affected our Model.
# We simplified our model as well.

# Next var we should remove based on P values is DRB (defensive rebound).
PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST +
                  ORB + STL + BLK, data  = NBA)

summary(PointsReg3)

# Again we see our model is equally good with 89.91% RSqaure and it got simplified.

# Removing BLK (blocks)

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST +
                  ORB + STL, data  = NBA)
summary(PointsReg4)

# Great! Our Rsqaure is still 89.91% and our model got more simpler with now just
# 6 insp var.

# Now lets see SSE and RMSE.

SSE = sum(PointsReg4$residuals^2)

SSE # Again in millions, but lesser than before.

RMSE = sqrt(SSE/nrow(NBA))

RMSE #184.493 # Almost same. ~ 2%
# 
# -----------------------------------------------------------------


# Now we'll make predictions using the PointsReg4 model we made.
# We'll predict Points Scored.
# We'll use a test data to see our accuracy for predictions.

NBA_test = read.csv('NBA_test.csv')

str(NBA_test) # 28 observations

str(NBA) # 835 observations

summary(NBA)  # 1980 - 2011

summary(NBA_test) # 2013

# Our test data is of 2013.

PointsPrediction = predict(PointsReg4, newdata = NBA_test)

PointsPrediction # We will get a vector with 28 different predictions of points
# these predictions are made using the regression equation.

# For each data point, the value of PTSDiff is calculated and plugged in.
# The resultant is this.

# Our In-Sample RSqaure value was 89.91%, it tells how good our model fits the 
# training data.

# Now we want to calculate Out-Sample RSqaure to see how good our model is for
# prediction on the test data.

# SSE = sum((Actual data on test dataset - Predicted Data on test data set)^2)
SSE = sum((NBA_test$PTS - PointsPrediction)^2)

SSE


# SSE = sum((Actual data on test dataset - Mean or Baseline value of the dep var)^2)
SST = sum((NBA_test$PTS - mean(NBA$PTS))^2)

# Baseline is calculated from the data of Training set because we want to take
# already known data into consideration for mean.

RSquare = 1-(SSE/SST)

RSquare # 81.27% Good one.
