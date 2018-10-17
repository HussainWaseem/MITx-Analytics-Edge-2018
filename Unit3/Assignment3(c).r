# In the lending industry, investors provide loans to borrowers in exchange for 
# the promise of repayment with interest. If the borrower repays the loan, 
# then the lender profits from the interest. 

# However, if the borrower is unable to repay the loan, then the lender loses money. 
# Therefore, lenders face the problem of predicting the risk of a borrower being 
# unable to repay a loan.

# Using various independent variables we will predict whether a borrower will repay
# a loan or not.

# Data set has loans which were given for a period of 3 years.

# May 2007 and February 2010. 

# The binary dependent variable not_fully_paid indicates 
# that the loan was not paid back in full

#--------------------------------------------------------------


loans = read.csv('loans.csv')

str(loans) #9578 obs / 14 var

# We have one unordered factor variable ->> purpose

# What proportion of the loans in the dataset were not paid in full?
table(loans$not.fully.paid)
mean(loans$not.fully.paid) # 16% loans were not fully paid.

summary(loans)

# We have 4 NA for log.annual.inc
# 29 NA for days.with.cr.line
# 62 NA for revol.util
# 29 NA for delinq.2yrs
# 29 for inq.last.6mths
# 29 for pub.rec

# We have in total 178 missing values.

# We want to see how many obs fall under these missing values


# is.na(variable) ->> returns a logical vector with TRUE values if NA is found 
# FALSE otherwise

# Criteria written below will select all those obs where TRUE values were found

missing = subset(loans, is.na(loans$log.annual.inc) | is.na(loans$days.with.cr.line)
                 | is.na(loans$revol.util | is.na(loans$delinq.2yrs) | 
                           is.na(loans$inq.last.6mths) | is.na(loans$pub.rec)))

nrow(missing)
# We have only 62 observations who have 178 missing values in total.
# That is only 0.65% obs who have missing data.

# If we remove this data, it would not lead to overfitting.

mean(missing$not.fully.paid) # 19%
# which is almost same as 16% of our original data.
# So removing this data woud not cause the bias in our remaining data.

# But if we want to predict loan worthiness of all data points then we have to
# fill in the values rather than removing them.

library(mice)

set.seed(144)

indp_var = setdiff(names(loans),'not.fully.paid')

# setdiff is the set difference between names and the dep var.
# So it contains the remaining indp vars.

# loans[indp_var] ->> It is the subset data containing only indp var.

imputed = complete(mice(loans[indp_var])) # Using all indp var for imputation

loans[indp_var] = imputed  # overwriting the indp var subset.

summary(loans) # Na values are gone.

# 
# # Imputation predicts missing variable values for a given observation using 
# # the variable values that are reported. 

library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)

loans_train = subset(loans, split == TRUE)
loans_test = subset(loans, split == FALSE)

nrow(loans_train)/nrow(loans) #70%
mean(loans_train$not.fully.paid) #16%

# Both the split and the baseline is maintained in our train as well as test set.

model1 = glm(not.fully.paid ~., data = loans_train, family = binomial)

summary(model1)

# We have some sign var
# +ve coeff contributes to not.fully.paid to be 1, ie; defaulters.
# -ve coeff contributes to dep var to be 0, ie; borrower will possibly repay the 
# loan.




# Consider two loan applications, which are identical other than the fact that 
# the borrower in Application A has FICO credit score 700 while the borrower 
# in Application B has FICO credit score 710.

# Logit(A) - Logit(B) = coeff of FICO(A) - coeff of FICO (B)
# -0.009294(700) - (-0.009294(710)) = 0.09294

# log(oddsA) - log(oddsB) = 0.09294

# Odds(A)/Odds(B)?

# exp(intercept) * exp(-0.009294(700)) * exp(othervar)/ exp(intercept) * exp(-0.009294(710)) * exp(othervar)

# RULE used ->> exp(A+B+C) = exp(A)*exp(B)*exp(C)

# 1.097396


predict.risk = predict(model1, newdata = loans_test, type = 'response')

loans_test$predict.risk = predict.risk

# Added this prediction vector to our test set to use it later.

# Building classification matrix
table(loans_test$not.fully.paid, predict.risk >= 0.5)

# We have a huge number of false Negative.
# That means we predicted 0 and it came out to be 1.
# We predcited the borrower will pay back but they din't paid.

# Also our True negatives are correct and are large in number.
# We only had 13 True positives correct.

# Overall accuracy 
2403/nrow(loans_test) # 83.6%

# Baseline accuracy
1 - mean(loans_train$not.fully.paid) # 83.9%
# Our models are commonly predicting 0

# Our model has not even crossed bseline.

# AUC
library(ROCR)
ROCR = prediction(predict.risk, loans_test$not.fully.paid)

AUC = as.numeric(performance(ROCR, 'auc')@y.values)

AUC # 67.18% differentiation power.
# The model has poor accuracy at the threshold 0.5.


# To improve our baseline we will be building another model using only
# int.rate as an independent var.

model2 = glm(not.fully.paid ~int.rate, data = loans_train, family = binomial)

summary(model2)

# int.rate is highly sig and contributes positively to dep var.
# but int.rate was not sig in our previous model.
# This is because int.rate decided by the Loan providing company was set
# using loan's risk, which in turn was decided using other indp var of the data.

# So int.rate has high correlation with other var of the data.

cor(loans[setdiff(names(loans),'purpose')])
# fico and int.rate are negatively 71% related.

# We'll use model2 and do predictions
predict.risk2 = predict(model2, newdata = loans_test, type = 'response')

# Highest Probability of not.fully.paid in predcition vecotr?
predict.risk2[which.max(predict.risk2)] # 42.66%
# If we keep t = 0.5 then no point will fall under sensitivity.

# AUC

ROCR2 = prediction(predict.risk2, loans_test$not.fully.paid)

auc2 = as.numeric(performance(ROCR2, 'auc')@y.values)

auc2 # 62.3% differentiation power



#-------------------------------------------------------------------


# While thus far we have predicted if a loan will be paid back or not, 
# an investor needs to identify loans that are expected to be profitable. 

# If the loan is paid back in full, then the investor makes interest on the loan. 
# However, if the loan is not paid back, the investor loses the money invested. 

# Therefore, the investor should seek loans that best balance this risk and reward.

# To compute interest revenue, consider a $c investment in a loan that has an 
# annual interest rate r over a period of t years. 
# Using continuous compounding of interest, this investment pays back c * exp(rt) 
# dollars by the end of the t years.

# Therefore Profit if the borrower returns money back ->> c*exp(rt)-c

# And if the borrower returns nothing ->> Profit = -c




# In order to evaluate the quality of an investment strategy, 
# we need to compute this profit for each loan in the test set.

# We will assume a $1 investment (so c = 1),
# if not.fully.paid = 0 ->> Loan was paid back ->> Profit = c(exp(rt)-1) ->> exp(rt)-1

# if not.fully.paid = 1 ->> loan not paid back ->> Profit = 1

# All loans are 3-years long loan.
# int.rate for every obs is given.

loans_test$profit = exp(loans_test$int.rate*3)-1
# This formuala will fill in for all obs taking value for int.rate from each obs.

loans_test$profit[loans_test$not.fully.paid == 1] = -1
# loans_test$not.fully.paid == 1, this will return a logical vector having
# TRUE for 1 as the value of dep var and FALSE otherwise.

# Now loans_test$profit[logical_vector] = -1 ->> will fill TRUE index obs as 1

str(loans)

loans_test$profit[which.max(loans_test$profit)]
# Max profit for 1 dollar came out to be 0.8894769 dollars.

# For any value of c now profit can be analyzed for any borrower
# Just multiple the profit by c ->> c(exp(rt)-1) ->> c(Profit_calculated)



# To have maximum profit Investor will only invest in loans which are of
# High Interest rates(>=15) and low risk.

highInterest = subset(loans_test, int.rate >= 0.15)

nrow(highInterest)

# Avg profit for $1 investment in such borrowers?
mean(highInterest$profit) #0.22$ per dollar

# What proportion of the high-interest loans were not paid back in full?
mean(highInterest$not.fully.paid) # 25.17%


# Now we want to have a cutoff for our not.fully.paid probability 
# and we want to select only those loans which have High Interest and 
# are less than this cutoff.

# we choose the probability of 100th loan placed in increasing order.

cutoff = sort(highInterest$predict.risk, decreasing = FALSE)[100]
cutoff 

SelectedLoans = subset(highInterest, highInterest$predict.risk <= cutoff)

nrow(SelectedLoans) # 100 loans

# How many SelectedLoans were not paid back in full?
mean(SelectedLoans$not.fully.paid) # 19%

nrow(subset(SelectedLoans, SelectedLoans$not.fully.paid == 1)) # 19 
# (since total was 100, 19% of 100 is 19 )
