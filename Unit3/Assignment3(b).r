# Predicting Parole(Bail) violaters 

# using a dataset from US 2004 parole candidates (jail period = 6-18months)
# we will predict the canditates as possible parole violaters or not based on various
# indp var.

parole = read.csv('parole.csv')

str(parole) #675 obs/ 9 var. All var are numeric but among them state and crime are
# numerically coded which 3 or more than 3 levels.

# violater is the outcome/response/dep var.

table(parole$violator)
# 78 people violated their parole ->> 78/675 ->> 11.55% 
# It looks like baseline will choose 0 (not violated) most of the times.

# unordered vars with 3 or more levels = state and crime

# We need to take these var into our model.
# They are already levelled.
# We just need to convert their type as factor so that R considers them as 
# categories and not numeric values.

parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

str(parole)

summary(parole)
# Now in summary we can clearly see that R now treats these var and their numeric levels
# as categories.


# Now splitting our dataset using sample.split()

library(caTools)

set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.70)

# This split will have 70% true and 30% false (approx)
# And it will have our baseline of violater maintained in both the true and false values.
# split just organizes the data and doesn't changes the actual data.

train = subset(parole, split == TRUE)

test = subset(parole, split == FALSE)

table(train$violator) # 55 1's
table(test$violator) # 23 1's

# Our total 1's were 78 and you can clearly see that the sum of 1's in our test
# and train equal 78.
# This verifies that split just organizes data and doesn't changes the value.


# Building a log model on violater using all our indp var.

pmod1 = glm(violator~., data = train, family = binomial)

summary(pmod1)

# race, state and multiple.offenses came out to be sig.

# Explaining the coeff of multiple.offenses ->> +ve ->> will contribute
# positively for being a violator.

# log(odds) = LR
# if all other coeff of all parolees are kept constant.
# then; log(odds) = coeff of multiple.offenses
# odds = exp(c)
# odds = exp(1.61) = 5.002

# This means that our model will predict that a parolee who has committed
# multiple offense has 5.002 times higher odds of being a violater than 
# one who has not committed multiple crimes.

# Remember odds(P(y=1)) = p(y=1)/p(y!=1)


# Predicting for a perosn ->>

# race = white = 1
# male = yes = 1
# age = 50
# state = Maryland = 1
# time.served = 3 months
# max.sentence = 12 months
# multiple.offenses = No = 0
# crime = larceny = 2

# Since this data is just a single data point we have to use Logistic Response 
# function to predict violator variable value for this data point.

# LRF = -4.2411574 + 0.3869904(male) + 0.8867192(race) + -0.0001756(age) 
#       + -0.1238867(time.served) + 0.0802954(max.sentence) + 0.6837143(crime2)

# LRF = -1.016914

# log(odds) = LRF = -1.016914
# odds = exp(-1.016914) = 0.3617095
 
# P(y = 1) = 1/(1+e^-LRF) = 0.265629


#-------------------------------------------------------------------------

# predicting on the test set.

predparole = predict(pmod1, newdata = test, type = 'response')

# Max predicted probability
predparole[which.max(predparole)]

# Building a confusion matrix
table(test$violator, predparole >= 0.5)

# Sensitivity = 12/23 = 52%
# Specificity = 167/179 = 93%
# Overall model's accuracy = 179/(nrow(test)) = 88.6%

# Baseline model accuracy (calculated on training set)
1-mean(train$violator) # 88.37%

#---------------------------------------------------------


# Consider a parole board using the model to predict whether parolees will be 
# violators or not. 

# The job of a parole board is to make sure that a prisoner is ready to be released 
# into free society, and therefore parole boards tend to be particularily concerned 
# about releasing prisoners who will violate their parole. 

# Which of the following most likely describes their preferences and best course 
# of action?


# Board will regret its decision of paroling someone if that person tends out to
# be a violator.
# In our model, 1 is for violator and 0 for Non violator.

# False Negatives ->> We predicted 0 but person came out to be 1.
# This is what board is more concerned of.

# Board should spend more on False negatives and also 0 should be rare.
# So to make 0 rare, we have to increase 1's, in other words we have to increase
# Sensitivity of the model, therefore threshold t will decrease.

# So the conclusion is : Board should spend more cost on False Negatives
# and choose a threhold lesser than 0.5 for the model.


# This model is of value to the board since it can be improved by decreasing the
# t value.


#----------------------------------------------------


library(ROCR)

ROCR = prediction(predparole, test$violator)

auc = as.numeric(performance(ROCR, 'auc')@y.values)

auc # 89.4% # Out-Sample Model accuracy.

# AUC 89.4% means that our model can successfully differentiate between a random
# violator
# and a non violator with an accuracy of 89.4%




# Our goal has been to predict the outcome of a parole decision, 
# and we used a publicly available dataset of parole releases for predictions. 
# In this final problem, we'll evaluate a potential source of bias associated 
# with our analysis. It is always important to evaluate a dataset for possible 
# sources of bias.

# The dataset contains all individuals released from parole in 2004, either due to completing their 
# parole term or violating the terms of their parole. 

# However, it does not contain parolees who neither violated their parole nor 
# completed their term in 2004, causing non-violators to be underrepresented. 

# This is called "selection bias" or "selecting on the dependent variable," 
# because only a subset of all relevant parolees were included in our analysis, 
# based on our dependent variable in this analysis (parole violation). 

# How could we improve our dataset to best address selection bias?



# While expanding the dataset to include the missing parolees and labeling each 
# as violator=0 would improve the representation of non-violators, 
# it does not capture the true outcome, since the parolee might become a violator 
# after 2004. 

# Though labeling these new examples with violator=NA correctly identifies 
# that we don't know their true outcome, we cannot train or test a prediction 
# model with a missing dependent variable.

# As a result, a prospective dataset that tracks a cohort of parolees 
# and observes the true outcome of each is more desirable. ie; they either
# violated or completed their term.

