pisaTrain = read.csv('pisa2009train.csv')
str(pisaTrain) # 3363 obs/24 var

# average reading test score of males?
tapply(pisaTrain$readingScore, pisaTrain$male, mean, na.rm = TRUE)
# 483.5325 for males
# 512.9406 for females.

# Which variables have missing data in at least one observation in the training set?

summary(pisaTrain)
# All except reading, urban, grade and male.


# Linear regression discards observations with missing data, 
# so we will remove all such observations from the training and testing sets. 
# Later in the course, we will learn about imputation, which deals with missing data 
# by filling in missing values with plausible information.

# To remove all obs with NA values, use na.omit(dataframe)

pisaTrain = na.omit(pisaTrain)

str(pisaTrain) # 2414 obs 


# Lets load our test data also and omit the NA obs.

pisaTest = read.csv('pisa2009test.csv')

str(pisaTest) #1570 obs

pisaTest = na.omit(pisaTest)

str(pisaTest) # 990 obs


# Factor variables are variables that take on a discrete set of values, 
# like the "Region" variable in the WHO dataset from the second lecture of Unit 1. 
# This is an unordered factor because there isn't any natural ordering between the levels. 
# An ordered factor has a natural ordering between the levels 
# (an example would be the classifications "large," "medium," and "small").


# In our dataset we have grades as ordered factor var, raceeth as unordered factor var, etc.


# To include unordered factors in a linear regression model, we define one level 
# as the "reference level" and add a binary variable for each of the remaining levels. 

# In this way, a factor with n levels is replaced by n-1 binary variables. 
# The reference level is typically selected to be the most frequently occurring level in 
# the dataset.

# As an example, consider the unordered factor variable "color", with levels "red", "green", 
# and "blue". If "green" were the reference level, then we would add binary variables 
# "colorred" and "colorblue" to a linear regression problem. 

# NOTE : Reference level is not added in the regression model.

# All red examples would have colorred=1 and colorblue=0. 
# All blue examples would have colorred=0 and colorblue=1. 
# All green examples would have colorred=0 and colorblue=0.


# Similarly in our data-set we have raceeth as an unordered factor var.
# We want it to be included in our reg model.
# So we will quantify it using binary var.

table(pisaTrain$raceeth)
# Max is White.
# Max is taken as Reference.
# So we will include all other raceeth in our model with binary values except White.

# Using str(pisaTrain) we saw that raceeth is read as FACTOR variable by R.
# But R automatically sets the first occuring value as reference.
# So here we need to reset it to White.

pisaTrain$raceeth = relevel(pisaTrain$raceeth, 'White')

# We are relevelling raceeth factor var by White and overwriting it to our raceeth var.
# We use relevel(factorVar, reference) for this.

pisaTest$raceeth = relevel(pisaTest$raceeth, 'White')

# Now we want to build a lm model to predict readingScores using all other var.

lmScore = lm(readingScore ~ grade + male + raceeth + preschool + expectBachelors + motherHS
             + motherBachelors + motherWork + fatherHS + fatherBachelors + fatherWork
             + selfBornUS + motherBornUS + fatherBornUS + englishAtHome + computerForSchoolwork
             + read30MinsADay + minutesPerWeekEnglish + studentsInEnglish + schoolHasLibrary 
             + publicSchool + urban + schoolSize + readingScore, data = pisaTrain)

# R provides a short-hand and saves time from writing all the variables using depvar ~.

lmScore = lm(readingScore ~. , data = pisaTrain)

summary(lmScore) # RSquare = 32.51% which means that these independent var can only explain
# 32.51% variance in the dependent var.

SSE = sum(lmScore$residuals ^2)
SSE

RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE # an error of 73.36555 score.

mean(pisaTrain$readingScore) # 517.96

# % error using RMSE = 73.36555/517.96 = 14.16%


# Consider two students A and B. They have all variable values the same, 
# except that student A is in grade 11 and student B is in grade 9. 
# What is the predicted reading score of student A minus the predicted reading 
# score of student B?

# We can use our Reg Eq for the same.

# Reg Eq ->> RS = 29.542707(grade) + X  (we are using X here since the question states 
# all other values for the given situation is same)

# RSa = 29.54271(11) = 324.9698
# RSb = 29.54271(9) = 265.8844

# RSa - RSb = 59.0854


tapply(pisaTrain$readingScore, pisaTrain$raceeth, mean)

# What is the meaning of the coefficient associated with variable raceethAsian?

# It is Predicted difference between the reading score of an Asian student and a White student.


# In the summary of our model, we have too many insignificant var we need to remove to 
# simplify our model.

# Also NOTE : since some of the sub categories of raceeth are significant, we won't remove
# raceeth though some sub categories are insignificant too.


# Predicting using the model we build earlier.

predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)
# min = 353.2 and max = 637.7, range = 284.5

SSE = sum((pisaTest$readingScore - predTest)^2)
RMSE = sqrt((SSE)/nrow(pisaTest))

SSE
RMSE

SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST

RSqPred = 1-(SSE/SST)
RSqPred

