# Analytics to prevent Heart Disease.

# Step 1 ->> Analyze the independent var (Risk Factors)
# Step 2 ->> Building a Logistic Regression model on data and predicting the outcome
# Step 3 ->> Validating our model using more random data.
# Step 4 ->> Defining interventions using the model.

# Predicting 10 years risk of CHD (Coronary Heart Disease)

framingham = read.csv('framingham.csv')

str(framingham) # 4240 obs

# Data was collected by the researchers of Framingham Heart Study
# So risk factors were pre-defined.
# TenYearCHD is our Outcome/Dep/Response var.


#Steps ->>
# Step 1 ->> Randomly splitting the data set into Training and Test set.
# Step 2 ->> Building a Logistic Reg Model on training set to predict positive CHD or not.
# Step 3 ->> Testing our predictive power on test set.


# NOTE : whenever you want to build the Logistic Reg model and you've to use split()
# load the library caTools

library(caTools)

set.seed(1000)

split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
# 65% data will form training set.

mean(framingham$TenYearCHD) # Baseline is 15% CHD positive

split # A logical vector.

# Using split to form train and test data.

framinghamTrain = subset(framingham, split == TRUE)
framinghamTest = subset(framingham, split == FALSE)

nrow(framinghamTrain) # 2756 obs ->> 2756/4240 = 65%

mean(framinghamTrain$TenYearCHD) # Baseline = 15% Positive CHD is also maintained.
# 
# # NOTE : When we have good number of obs we can afford to put some 
# less data in training set and increase our data in Test set.
# # This will increase our confidence in the ability of the model to extend to 
# # new data since we have a larger test set,

# Usually 50% to 80% data is put in Training Set.

# All our variables except the outcome var is a Risk factor.

framinghamLog = glm(TenYearCHD ~. , data = framinghamTrain, family = binomial )

summary(framinghamLog)
# We have some significant var and all those var have positive coeff.
# That means all of them will make CHD positive.

predictTest = predict(framinghamLog, newdata = framinghamTest, type = 'response')

predictTest # Note : we have NA values also.

# Building a confusion matrix
table(framinghamTest$TenYearCHD, predictTest > 0.5 ) # t value is 0.5

# Our True positive is just 11 out of 1273 obs, it means our model predicts
# CHD for 10 years very rarely.

# True Positive Rate = TP/(TP+FN) = 11/(11+187) = 5.5%

# Overall Accuracy of the model = Total True values / N = (1069 + 11)/ 1273 = 85%

# Baseline accuracy from classification matrix = False positive rate = 
# 1069/1273 = 85%

# So our model barely beats the baseline.


# Calculating AUC of our model
library(ROCR)
ROCRpred = prediction(predictTest, framinghamTest$TenYearCHD)

auc = as.numeric(performance(ROCRpred, 'auc')@y.values)
auc
# 74.2% model accuracy on Out sample data.

# auc tells us that how well our model can differentiate between patients with
# positive CHD and negative CHD.

# Overall accuracy of the model which came out to be almost equal to baseline accuray
# tells us the predictive power of the model and max cases 1026 were of (0|0) 
# that is predicting no CHD or negative CHD.



# Sensitivity of our model = 11/(11+187) = 5.5% = Our prediction for +ve CHD was very rare.

# Specificity = 1069/(1069+6) = 99.4% = Our prediction for Negative CHD was very much.


# If our model accuray was not good then,
# in that case we should change our threshold to get max +ve CHD cases.


# Internal Validation ->> Using a single source of data and splitting it into
# train and test to check our predictions.
# This validation has a problem that it can't be generalized to other data with
# different representation of the population.

# External validation ->> Building such a model which can be generalized to the 
# the entire population.

