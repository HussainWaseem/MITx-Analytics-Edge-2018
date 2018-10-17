# # In linear regression, the outcome (dependent variable) is continuous. 
# # It can have any one of an infinite number of possible values. 
# 
# # In logistic regression, the outcome (dependent variable) has only a limited number of
# # possible values. Logistic regression is used when the response variable (dependent var)
# # is categorical in nature.
# 

# 
# # Logistic regression is a statistical method for analyzing a dataset 
# # in which there are one or more independent variables that determine an outcome. 
# # The outcome is measured with a dichotomous variable (in which there are only 
# # two possible outcomes).

# 
# # Logistic Regression is one of the most used Machine Learning algorithms for 
# # binary classification.


# Just like linear regression equation was used to predict the continuous values
# of a dep var.

# Logistic Regression uses Logistic Response Function to predict the Probability
# of the dependent var between 0 and 1.

# LRF ->> P(y=1) = 1/(1+e^-(linear Regression equation for the same model))

# Positive coeff value increase Linear Regr eq part of LRF and thus
# increases the P(y=1) towards 1

# Negative coeff value decreases Linear Reg eq part of LRF and thus 
# decreases the P(y=1) towards 0

# Odds = P(y=1)/P(y=0)

# Odds = e^(Linear Reg Eq for the model)
# log(Odds) = Linear Reg Eq for the model
# log(odds) is called Logit.

# Therefore P(y=1) can be also written as ->> P(y=1) = 1/1 + e^(-Logit)

# beta0 = -1.5, beta1 = 3, beta2 = -0.5
# x1 = 1, x2 = 5

# x ->> Indp var
# beta ->> Coeff of Reg
# Logit = beta0 + x1(beta1) + x2(beta2) ->> -1.5 + 3 + 5(-0.5) = -1

# log(Odds) = -1; Odds = exp(-1) = 0.3678794

# P(y=1) = 1/1 + e^ -(Logit) = 1/(1+exp(-(-1))) = 1/(1+exp(1)) = 0.2689414


#--------------------------------------------------------



quality = read.csv('quality.csv')

str(quality) #131 obs / 14 var

table(quality$PoorCare) # PoorCare is a binary var.

mean(quality$PoorCare) # 25% people only got poor care and 75% got good care.

# Baseline = Mean of our Predicting variable on the Training data set.
# which here is 25% for PoorCare.

# Installed and downloaded caTools library.
# install.packages('caTools')
# library('caTools')

# We will now randomly split our data.
# But to get the same split as our course instructor we need to set the seed value
# This will initialize the random value.

set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)

# sample.split(outcome_var, SplitRatio = % of data you want in training set)

# Also, sample.split() makes sure that the baseline value is maintained in
# both the Training set as well as the Test set that is 25% PoorCare.
# That is why split() asks for Outcome Var.

split

# Its a logical vector. TRUE stands for obs of Training Set
# FALSE stands for obs of Test Set.


str(quality)

mean(split) # 75% is 1 ->> 75% is TRUE ->> 75% is of Training set.

qualityTrain = subset(quality, split == TRUE)

# NOTE : split is not a vector of quality dataframe but since it is of same size
# we can use it in our criteria to take out the required obs.


qualityTest = subset(quality, split == FALSE)

nrow(qualityTrain) #99 obs

nrow(qualityTest)  # 32 obs

# 99/131 ~ 75% data is in our training Set

# To check whether split() has maintained the baseline
table(qualityTrain$PoorCare)
# % of PoorCare obs = 25/(74+25) ~ 25%

table(qualityTest$PoorCare)
# % of PoorCare obs = 8/32 = 25%

# Yes baseline is maintained.
# It maintains the baseline so perfectly. How?
# It puts TRUE to only those values ->> (keeping the criteria in check)
# 75% split and basline.



# Building our model ->> we'll use glm() function which stands for generalized
# linear model.
# Logistic Regression is a type of generalized linear model.

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain,
                 family = binomial )

# family = binomial tells glm() to build a logistic reg model.

summary(QualityLog)

# AIC is the measure of goodness of the model.
# Its based upon number of indp var used to predict the Outcome vs number of 
# predictions made.

# Lesser the AIC value, better the model.

# AIC is not Universal like Rsq, and it can only be compared on a model build
# on the same dataset.




# Prediction

# Predicting on the training data first to see the comparison
predictTrain = predict(QualityLog, type = 'response')

# predict(model_name, newdata = name of newdata, type = response/class)

# type argument ->> the type of prediction required.
# type = "response" gives the predicted probabilities.


predictTrain # You get a vector with P(PoorCare), each value being between 0 and 1.

tapply(predictTrain, qualityTrain$PoorCare, mean)
# Since the size of both the vectors are same, wherever PoorCare was TRUE
# for all those data points mean was calculated and put under 1.

# for all the datapoints where PoorCare was False, mean was calculated of those
# data points and put under0.

# P(0) = P(GoodCare) ~ 0.19
# P(1) = P(PoorCare) ~ 0.44



#Building another model.

QualityLog2 = glm(PoorCare ~StartedOnCombination + ProviderCount, data = qualityTrain
                  , family = binomial)

summary(QualityLog2)
# Only ProviderCount is somewhat sig.
# AIC is more than previous model, that means this is worse than previous model.


# StartingOnCombination is a logical variable. Therefore it is a binary variable.
# Positive Coeff means it is indicative of Poor health care. p(y) ->> p(PoorCare).




# Threshold ->> Value chosen to convert the continuous values of Probability to binary.
# We can either choose t to be small, in that case more p(y) = 1
# will be predicted
# For t value larger, less p(y) = 1 will be predicted.

# If the analyst has no preference of any error, its better to choose t = 0.5
# It will predict the most likely outcome.
# If P(y) >= t ->> 1 is selected as prediction
# If p(y) < t ->> 0 is selected as prediction



# 
# 
# # True Positive ->> (Actual | Predicted) ->> (1|1)
# # True Negative ->> (0|0)
# # False Positive ->> (0|1)
# # False Negative ->> (1|0)

# Remember first fill y in (x|y). Positive = 1, Negative = 0
# TRUE = +(y) in x
# False = -(y) in x

# #
# # Using Classification Matrix ->> Also called Consfusion Matrix
# # Sensitivity ->> TP/(TP + FN) ->> Measures the actual '1' cases we classify correctly.
# 
# # Its also called True Positive Rate.
# # P(1|1)/P(1|1) + P(1|0)  ->> Here all actual values are 1
# 
# 
# 
# # Specificity ->> TN/(TN + FP) ->> Measures the actual '0' we classify correctly.
# # Its called True Negative Rate
# 
# # Here all actual values are 0
# # (0|0)/(0|0) + (0|1)
# 
# 

# We earlier said, that higher t will have lesser p(y) = 1 values
# Therefore higher t will have lower Sensitivity.

# Lower t value will have Higher Sensitivity.

# t is inversely Proportional to Sensitivity.
# t is directly proportional to Specificity.

# Lower t value, less number of 1s, less sensitivity, more 0s, more Specificity.


# Lets build a classification matrix on our Prediction and Training set.

table(qualityTrain$PoorCare, predictTrain > 0.5)

# Since both the vectors are of same size. For obs where 0 was in 1st and 
# corresponding was FALSE, 70 were recorded such.
# For obs where 0 was in 1st vector and corresponsing was TRUE, 4 were recorded and 
# so on.

# TN = 70, FP = 4, FN = 15, TP = 10

# Sensitivity = 10/10 + 15 = 0.4 = 40%
# Specificity = 70/70+4 = 0.945 = 95%


# If we increase out threshold to 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)
# Sensitivity = 8/25 = 0.32 ~  32%
# Specificity = 73/74 = 0.986 ~ 99%



# 
# # A receiver operating characteristic curve, i.e., ROC curve, 
# # is a graphical plot that illustrates the diagnostic ability of a binary 
# # classifier system as its discrimination threshold is varied.

# Installing 'ROCR' packages and running it.

# Everytime you want to build an ROC curve you have to create a prediction vector.

# prediction(vector_with_predicted_values, Outcome_var)

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

ROCRpred

#  This function is used to transform the input data 
# (which can be in vector, matrix, data frame, or list form) 
# into a standardized format.

# After creating a prediction object, we have to create a performance object using 
# predcition object.

ROCRperf = performance(ROCRpred, 'tpr', 'fpr')

# performance(prediction_object, performance_measure1, performance_measure2)

# tpr = True Positive Rate
# fpr = False positive Rate

# Now plot the output of performance function.

plot(ROCRperf)

# To add colors to the plot
plot(ROCRperf, colorize = TRUE)

# The colored legend at the right is the threshold value spectrum.

# If you want to add numeric values to the plot.

plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1),
     text.adj = c(-0.2,1.7))

# print.cutoffs.at(start, end, increment) ->> at every increment of 0.1, the number
# is added to the plot.

# text.adj is for adjusting the text of number added.




# # Overall Accuracy of our model using confusion matrix ->> Sum of True values/N
# # (TP + TN)/N;   N = Number of predicted values. ie; Number of values on our predict
# # vector.
# 
# # Overall error rate = Sum of false values/N = (FP + FN)/N
# 
# # False Negative Error rate = FN/(TP + FN)
# 
# # False Positive Error rate = FP/(TN+FP)
# 
# 
# # Also ->> False Positive Error rate = (1-specificity)
# 




# Making predictions on the Test set

predictTest = predict(QualityLog, type = 'response', newdata = qualityTest)

predictTest
# We have now P(y) for 32 data points.

# Making a consfusion matrix with Threshold of 0.3
# table(actual_data, Predicted_probabilities > t)
# Since predictions were made on test data, our actual data is qualityTest

table(qualityTest$PoorCare, predictTest > 0.3) 

# Sensitivity = 6/8 = 75% (We predicted 75% data points for PoorCare as correct)
# Specificity = 5/24 = 21% (we only predicted 21% correct values for GoodCare)

# Overall Accuracy of the model = TN+TP/N = 25/32 = 78%
# Overall Error of the model = FP + FN /N = 7/32 = 21%




# 
# # You can compute the test set AUC by running the following two commands in R:
# 
# ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
# 
# auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, 'auc')@y.values)

auc # 80%
# This is our model accuracy on test set. (Out of sample accuracy)
# given a random patient from the dataset who actually received poor care, 
# and a random patient from the dataset who actually received good care, 
# the AUC is the perecentage of time that our model will classify which is 
# which correctly.

# AIC and AUC are different.
# AIC is known from the summary of In-Sample model ->> This is not the accuracy.
# AUC is known from Out-Sample Model ->> This is accuracy.


