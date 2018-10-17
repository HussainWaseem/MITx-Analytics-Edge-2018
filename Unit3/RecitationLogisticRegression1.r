polling = read.csv('PollingData.csv')

str(polling) #145 obs / 7 var

table(polling$Year) # each obs corresponds to the vote of each state and not the vote
# of each voter.

# We have all 50 states voting results from 2004 and 2008.
# But for future 2012 we have only collected potential voting data from 45 states.

summary(polling)
# decent number of NA values in Rasmussen and SurveyUSA variables.


# Rasmussen and SurveyUSA = %R votes - %D votes
# DiffCount = Polls with R winner - Polls with D winner


# There are several ways to deal with the missing data.
# 1. To delete obs having NA
# 2. To delete the variable having NA
# 3. To fill the NA value with some reasonable value like average of that variable.


# We will do Multiple Imputations ->> Filling in the NA using the available data.

# installing and loading mice package.

library(mice)

# While we are doing imputation we want the data set with only indp var.
simple = polling[c('State','Year','Rasmussen','SurveyUSA','DiffCount','PropR')]

# This will extract a data frame from polling without the Republican var.

summary(simple) # and we have our same NA values

# Also since imputation is a random process, to get along with the course data
# we need to fix the random value

set.seed(144)

imputed = complete(mice(simple)) # 5 rounds of imputations took place.

summary(imputed) # imputed is a developed version of simple data set with all
# NA filled appropriately.

# Now we have to take those 2 var which had NA and have to overwrite them in our
# polling data set.

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

summary(polling)

# Now as our source data is ready to use, we'll now split the data into train and test.
# We'll use subset and not split.

train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012 )

nrow(train) # 100
nrow(test) #45

# So we have about 69% data in our training set and remaining in test set.

table(train$Republican)

# Baseline predicts the more common outcome.
# since here the more common outcome is Republican. Therefore it will be predicted
# by our baseline model.

# Accuracy of baseline model on training set ->> 53/100 = 53%
# Basline is not good as it predicts Republicans everytime.

# Better if we can adjust our baseline to be more sophisticated and real.

# sign() ->> it returns +1 for a positive argument. -1 for a negative arg and 0 for 0.

# Rasmussen is the diff of % R votes and % D votes.
# So if %R is greater that means chances of R to win is better and vice versa.

# so if we pass Rasmussen into sign, it will return 1 if %R is greater,
# -1 if %D is greater and 0 if we have a tie.

# So 1 if R is winning, -1 if D is winning and 0 for a tie.
# This baseline model is smart as it'll not always predict R.

table(sign(train$Rasmussen))
# We have 54 times R and 44 times D predicted, 2 times the model is unable to decide.

# Lets compare it with real data.

table(train$Republican, sign(train$Rasmussen))

# 43 times we predicted D and the results were also D.
# 51 times we predicted R and results were also R.

# 3 times we predicted R but results were D.
# 1 time we predicted D but result was R.
# 1 time we were unable to predict.



# Checking for Multi-Collinearity between the indp var and dep var 
# before building the model.

cor(train[c('Rasmussen','PropR','DiffCount','SurveyUSA','Republican')])

# Very high corr between Rasmussen and SurveyUSA ->> 85%

# Now the best indp var to include in the model is the one which has max cor with 
# Republican ->> PropR ->> 95%

# Only using PropR to build the logistic model
mod1 = glm(Republican ~PropR, data = train, family = binomial)

summary(mod1)
# Very sig var. Coeff is also +ve. As PropR will contribute to predict R.
# AIC is also low. Good model.

# predicting on the training data first (for in sample accuracy)
pred1 = predict(mod1, type= 'response')

pred1

# Now building the confusion matrix
table(train$Republican, pred1 >= 0.5) # t is 0.5

# Very good accuracy
# 4 mistakes and rest true. Almost equal to our sophisticated baseline model.


# Now to improve the model we would add more indp var into the model.
# We need to add Rasmussen or SurveyUSA , anyone among them as they have M-Colntry

# Trying out SurveyUSA and DiffCount
# Both are highly correlate with Republican but less inter correlated.

mod2 = glm(Republican ~SurveyUSA + DiffCount, data = train, family = binomial)

summary(mod2)

pred2 = predict(mod2, type = 'response')
table(train$Republican, pred2 >= 0.5)

# No improvement in the model except the AIC got lower.

# We'll use this 2 variable model to make predictions on the test set.

# First testing our basline model on test set.
# Building a confusion matrix.
table(test$Republican, sign(test$Rasmussen))

# 18 times predicted D correctly.
# 20 times predicted R correctly.
# 4 times predicted R but real data had D.
# 3 undecided.

TestPrediction = predict(mod2, newdata = test, type = 'response')
table(test$Republican, TestPrediction >= 0.5)
# Our model only predicted 1 wrong value.

