# Linear Regression = Building a model to predict the Outcome variable or a dependent variable
# using independent variables.

# One Variable Linear Regression = We use a Line of Regression for measuring our model.

# 3 kinds of measure for Regression Model = SSE, RMSE, Rsquare

# SSE = Sum of Squared Errors.
# RMSE = Root Mean Squared Error = UnderRoot(SSE/N); N = Total number of data points.
# RSquare = 1-(SSE/SST); SST = Total Sum of Squared Error = SSE of Baseline.

# SSE is not very useful alone since it takes all data points in consideration without
# normalization and it has squared units of the dependent variable.

# RMSE is an improvement over SSE by normalizing it by N (taking its mean) and making its unit
# correct by taking under root.

# RSquare is a good measure of Linear Regression model because it is Unit-less and can be
# used universally. Also it takes SSE in its calculation.
# RSquare is not a good measure for Hard Linear Regression Models.



# Baseline prediction - is the average value of dependent variable.


# Multiple Linear Regression = When Multiple Independent variables are used to predict a dependent
# variable.

# It is used to improve the model.

# CAUTION :  Not all variables should be used to build the model.
# Otherwise despite of having High RSquare, it will cause overfitting.

# Overfitting the model generally takes the form of making an overly complex model.

# NOTE : Rsquare value never decreases as we keep on adding more Indp variables.


wine = read.csv('wine.csv')

str(wine) # 25 observation.

# Price is our Dependent variable we want to predict.

# AGST = Avg Growing Season Temperature.

# FrancePop = Population of France.

summary(wine)

# Building a Linear Regression model using lm() which stands for linear model.

# lm(dependent_var ~ independent_var, data = name_of_dataframe)

model1 = lm(wine$Price ~ wine$AGST, data = wine)

summary(model1)

# Intercept value = -3.4178
# Regression Coefficient = 0.6351
# RSqaure = 0.435
# Adjusted RSquare = 0.4105

# Rsquare will always increase on adding more indp var.
# Adjusted RSqaure is adjusted value of RSquare with number of indp var used and total data
# points. Its value decreases if an indp var is used which doesn't contributes to the model.


# To see the value of our residuals.

model1$residuals

# Now to calculate SSE

SSE = sum(model1$residuals^2)   #model1$residuals is a vector, So any operation applied on it
# will be applied on all the elements of the vector.

# sum() calculates the sum of terms of numeric vector.

SSE #5.734875


# Building 2nd Model but by using AGST and HarvestRain

# NOTE : Unlike all other methods, we can just use the Variable name instead of using
# dataframe$var_name inside lm().

model2 = lm(Price ~ AGST + HarvestRain, data = wine)

summary(model2)

# We see that we now get a better Rsqaure and adjusted RSqaure for our new model.

SSE = sum(model2$residuals^2)

SSE # 2.970373

# Also our SSE is reduced. (Because of this only our RSqaure has improved).


str(wine)

# Now using all indp var except Year to predict Price.

model3 = lm(Price ~ AGST + HarvestRain + Age + WinterRain + FrancePop, data = wine)

summary(model3)

# We get a much better model than the above 2 models.
# Rsqaure = 0.8294 and Adjusted RSquare = 0.7845

SSE = sum(model3$residuals^2)

SSE # 1.732113



# Now building some other models for the same dataset.

modelnew1 = lm(Price ~ HarvestRain + WinterRain, data = wine)

summary(modelnew1) # Poor Rsqaure = 0.3177, adjustedRSquare = 0.2557



# Explaining the Coefficient columns in the summary of our models.

# Estimate = Intercept and Coefficient of Regression of Independent variables.

# it should be Significantly different than 0 to be included in our model.
# The more it is way from 0, the better it is.

# Std Error is the error in our Coefficient.

# t value = Estimate/Std Error
# Its absolute value is taken into account.
# The more the abs(t), the better is the independent var for the model.

# P(>|t|) = Probability that absolute t is actually non-significant or closer to 0.

# Higher values of P means, we need to remove that indp var due to its non-significance to
# predict the dep var of our model.


# So all in all, we need high |t| value and low P value.
# High |t| value in itself makes the P value lesser.

# In R the significant values given at he column's end is the best way to determine 
# which variables are highly, modertely and almost significant.

# *** = P <= 0.001  || Highly Sig
# ** = 0.001 <= P <= 0.01   || Moderately Sig

# * = 0.01 <= P <= 0.05 || Almost Sig.

# . = 0.05 <= P <= 0.1  || Ok. (Better to not include these var)



# Finding corelation ->> cor(var1, var2)

# We only consider the abs value to tell the strength of corelation.
# 1 = Strongly Positively Corelated
# -1 = Strongly Negatively Corelated
# 0 = No Corelation.

# Closer to 0 = Weak Corelation.

cor(wine$WinterRain, wine$Price)
# 0.136 # Weak corelation

cor(wine$Age, wine$FrancePop)
# -0.99 # Strongly Neg Corelated

# To find Cor between all the variables of a dataset = cor(data_frame)

cor(wine)


# Multi-Colinearity = High Corelation between 2 independent variables of the model.

# Multi colinearity badly affects the model as the model is unable to determine how the 2 multicolinear
# variables are individually contributing to the model.
# We should use only 1 of the 2 highly corelated indp variables in our model.

# Cor > 0.7 or > -0.7 is considered good.




# Predicting the outcome / Dep variable.

# Training vs Test data 

# Training Data = The data used to build the model.
# Test Data = The new data on which prediction is made.

wineTest = read.csv('wine_test.csv')
str(wineTest) # We have 2 data points only. These data points were not included in wine dataset.

# Rebuilding our model using AGST, HarvestRain, WinterRain and Age. 
# We have not used FrancePop as FrancePop and Age are both Multicolinear with a corr of -0.99

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)

# RSquare = 0.8286
# AdjustedRSquared = 0.7943

# We'll use this model as our training model.
# predict() is used to predict the outcome var.
# predict(model, newdata = test_dataframe)

predictTest = predict(model4, newdata = wineTest)
predictTest

# Now calculating SSE by summing up all the squared values of  (Actual value - Predicted value).

SSE = sum((wineTest$Price - predictTest)^2)  # Differencing 2 vectors will given corresponding diff.

# Then summing them up.

SSE

# Now calculating SST, SST is calculated by summing up the all the squared values of (Actual Val - Mean val)

SST = sum((wineTest$Price - mean(wine$Price))^2)
SST

RSq_Test = 1-(SSE/SST)
RSq_Test
# 0.79 # We have an accuract of 0.79.


# ModelRsquare will always increase as we add new var.
# TestRSqaured may increase or may decrease.

# We want a model with better Model as well as Test Rsquare values.

# NOTE : Test Rsqaure value can be NEGATIVE also. Why?
# This is because our Model can't do worse than the baseline model on the training data.
# But can do worse than baseline model on the test data.


# Model RSqaure lies between 0 and 1.
# 0 = Identical to Baseline Model.
# 1 = Fully explained the Outcome var.

# But Test RSqaure or Out of the Sample RSqaure can even go less than 0.
# This is because Out-of-sample model can be worse than the baseline.

# So TestRSqaure has a range of -ve Infinity to 1.



