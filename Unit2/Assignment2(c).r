FluTrain = read.csv('FluTrain.csv')

str(FluTrain)

# Week is in date form but not read as date by R
# ILI = % of ILI related Physician visits/week.
# Queries = Proportion of Query searches related to Influenza/week.

# Which week corresponds to the highest percentage of ILI-related physician visits?

max(FluTrain$ILI) # 7.618892

a = subset(FluTrain, ILI == 7.618892134)
a # 2009-10-18 - 2009-10-24

# Which week corresponds to the highest percentage of ILI-related Query fraction?

b = subset(FluTrain, Queries == max(FluTrain$Queries)) # 1
b # 2009-10-18 - 2009-10-24


# Plot ILI on a histogram.

hist(FluTrain$ILI) # Rightly Skewed data.


# When handling a skewed dependent variable, it is often useful to predict the 
# logarithm of the dependent variable instead of the dependent variable itself --
# this prevents the small number of unusually large or small observations from 
# having an undue influence on the sum of squared errors of predictive models.

# Plotting log(ILI) vs Queries

plot(log(FluTrain$ILI), FluTrain$Queries, col = 'red') # We see a +ve corelation between them.

cor(log(FluTrain$ILI), FluTrain$Queries) # 84.2%

# Since there is a +ve linear corelation, predicting values of ILI using Linear Reg model
# can be a good choice.

# So we'll be predicting log(ILI) as already mentioned, since its original value is skewed.
# Reg Eq will look like this ->> log(ILI) = Intercept + CoeffReg(Queries)

# Since both are +vely corelated, the CoeffReg will be positive.
# which means, it Queries increases, log(ILI) will also increase.

FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)

summary(FluTrend1)

# Queries are Highly sig. RSquare = 70.9%
# Correlation^2 is equal to the R-squared value. It can be proved that this is always the case.


# Loading in the Test data

FluTest = read.csv('FluTest.csv')

str(FluTest) # 52 obs. Data is from 2012. Training data was upto 2011.

# We will predict ILI on our test data using FluTrend1 model we created.

PredTest1 = predict(FluTrend1, newdata = FluTest)

# But this would give me a PredTest1 vector which would have log(ILI) values.
# We want to predict ILI values.

# So we will use exp() function on this PredTest1 vector to normalize it.

PredTest1 = exp(PredTest1)

# What is our estimate for the percentage of ILI-related physician 
# visits for the week of March 11, 2012?

# We've to see it using PredTest1[index]
# But before that using Test data set we've to find out the index ->>  

grep('2012-03-11', FluTest$Week) #grep(string, vector) ->> returns the obs.
#11

PredTest1[11] # 2.187378

# What is the relative error betweeen the estimate (our prediction) and the observed value
# for the week of March 11, 2012?

# RelativeError = (observedILI - PredictedILI / ObservedILI)
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]
# 4.62%

RMSE = sqrt(sum((FluTest$ILI - PredTest1)^2)/nrow(FluTest))
RMSE

# ------------------------------------------------------------------------


# The observations in this dataset are consecutive weekly measurements of the 
# dependent and independent variables. This sort of dataset is called a "time series." 
# Often, statistical models can be improved by predicting the current value of the 
# dependent variable using the value of the dependent variable from earlier weeks. 

# In our models, this means we will predict the ILI variable in the current week using 
# values of the ILI variable from previous weeks.

# Because the ILI variable is reported with a 1- or 2-week lag, 
# a decision maker cannot rely on the previous week's ILI value to predict the current 
# week's value.
# Instead, the decision maker will only have data available from 2 or more weeks ago. 

# We will build a variable called ILILag2 that contains the ILI value from 2 weeks before 
# the current observation.


# We will use 'zoo' package of R to work with Time Series.

# zoo(vector_having_data)
# lag(zoo(), -number_of_obs_needed_to_be_returned)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

# The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset, 
# where we can't compute the data from 2 weeks earlier.
str(ILILag2)
summary(ILILag2)

FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

# coredata(zoo series) ->> Converts a zoo series into a normal numeric vector.

plot(log(FluTrain$ILILag2), log(FluTrain$ILI), col= 'red') # +ve Linear corelation

str(FluTrain) # A lag was created by adding two NAs in the first 2 weeks 
# (since we don't have data for them from the previous weeks)


# Now building a model using Queries as well as log(ILILag2) variables to predict log(ILI).

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)

summary(FluTrend2)

# Both var are highly sig.
# RSq = 90.63%

# Now before making predictions, we have to add ILILag2 vector for our test data in our test data.


ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE)

str(ILILag2)
ILILag2

FluTest$ILILag2 = coredata(ILILag2)
str(FluTest)
summary(FluTest)


# We can fill in the missing values in the FluTest data set because we have the data of
# FluTrain 2nd last and last week.

FluTrain$ILI[416:417]

FluTest$ILILag2[1:2]

FluTest$ILILag2[1:2] = FluTrain$ILI[416:417] 

FluTest$ILILag2[1:2]

PredTest2 = predict(FluTrend2, newdata = FluTest)

PredTest2 = exp(PredTest2) # Normalizing from log(ILI) to ILI.

SSE = sum((FluTest$ILI - PredTest2)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE