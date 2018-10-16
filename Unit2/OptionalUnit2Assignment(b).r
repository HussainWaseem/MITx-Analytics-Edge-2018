# # An important application of linear regression is understanding sales. 
# # Consider a company that produces and sells a product. 
# # In a given period, if the company produces more units than how many consumers 
# # will buy, the company will not earn money on the unsold units and will 
# # incur additional costs due to having to store those units in inventory 
# # before they can be sold. 
# 
# # If it produces fewer units than how many consumers will buy, the company 
# # will earn less than it potentially could have earned. 
# 
# # Being able to predict consumer sales, therefore, is of first order 
# # importance to the company.


# 
# 
# # In this problem, we will try to predict monthly sales of the Hyundai Elantra 
# # in the United States. The Hyundai Motor Company is a major automobile 
# # manufacturer based in South Korea. 
# 
# # The Elantra is a car model that has been produced by Hyundai since 1990 
# # and is sold all over the world, including the United States. 
# 
# # We will build a linear regression model to predict monthly sales using 
# # economic indicators of the United States as well as Google search queries.


elantra = read.csv('elantra.csv')

elantra
str(elantra) # 50 obs / 7 var

elantraTrain = subset(elantra, Year <= 2012)
elantraTest = subset(elantra, Year >= 2013)

str(elantraTrain) # 36 obs / 7 var

str(elantraTest) # 14 obs / 7 var


# Build a linear regression model to predict monthly Elantra sales 
# using Unemployment, CPI_all, CPI_energy and Queries as the independent variables.

# Use all of the training set data to do this.

elantra_model1 = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, 
                    data = elantraTrain)

summary(elantra_model1) # All came out to be insig.
# Rsq = 42.82%, AdjRSq = 35.44%

# Interpreting the Coeff ->> Unemployment ->> For a unit change in Unemp 
# predicted value of ElantraSales will change by 3179 units inversely.

# That is ->> Increasing a unit Unemp will decrease the sales by 3179 units.

# NOTE : Unemp is in %. Therefore unit % increase or decrease in Unemp will
# decrease or increase the sales by 3179 units.

#-----------------------------------------------------------------------



# Our model R-Squared is relatively low, so we would now like to improve our model. 
# In modeling demand and sales, it is often useful to model seasonality. 

# Seasonality refers to the fact that demand is often cyclical/periodic in time. 
# For example, in countries with different seasons, demand for warm outerwear 
# (like jackets and coats) is higher in fall/autumn and winter 
# (due to the colder weather) than in spring and summer


# In our problem, since our data includes the month of the year 
# in which the units were sold, it is feasible for us to incorporate monthly 
# seasonality.


sort(tapply(elantraTrain$ElantraSales, elantraTrain$Month , mean, na.rm = TRUE))


# To incorporate the seasonal effect due to the month, build a new linear regression
# model that predicts monthly Elantra sales using Month as well as Unemployment, 
# CPI_all, CPI_energy and Queries. 

elantra_model2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries
                    , data = elantraTrain)

summary(elantra_model2)
# Rsq = 43.44%, Adj Rsq = 34.02%

# As we can see that RSq has not changed by much (this little increase is just
# because the added var has non zero coeff).
# AdjRsq has decreases denoting that the added var is insig and this model 
# is more complex with no improvement.

# So a bad model.



# what is the absolute difference in predicted Elantra sales given that one period is 
# in January and one is in March?
# Provided two monthly periods that are otherwise identical 
# in Unemployment, CPI_all, CPI_energy and Queries,

janSales = 148330.49 + 110.69
marchSales = 148330.49 + 110.69*(3)

salesDiff = marchSales - janSales 
salesDiff

SalesDiff = 148330.49 + 110.69*(5) - janSales # For may - jan
SalesDiff



# One problem in the model2.
# We see that Month var was a numeric variable (numerically coded as 1 for Jan, 2 for Feb, etc)

# And we see that coeff of Reg of Month is ~ +110.
# So for every change in month (1 ->> 2, Jan ->> Feb) there will be 110 units increase
# in sales. This is just not right.

# Because Sales should be seasonal and not linearly positive with month.


# So we need to convert Sales in factor variable.

elantraTrain$MonthFactor = as.factor(elantraTrain$Month)

str(elantraTrain)

# Factor = Nominal Variable = No Mathematical Sig and Distinct.
# We have our MonthFactor var as Nominal var vector now.

# Re-run the regression with the Month variable modeled as a factor variable. 

elantra_model3 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy
                    + Queries , data = elantraTrain)

summary(elantra_model3)


# Note : MonthFactor is an unordered Factor variable and we have used it in our
# Regression model without relevelling because 1 (Jan) is by default the ref.

# And we can see using tapply(sales, month, mean), the max avg sales in during Jan only.


# We see in our new model3 that some var have become sig, but the sign of
# Queries Coeff has changed.
# Queries of Elantra and Sales have become inversely related.

# This is due to Multi-Collinearity.
# 
# # Multi-Col either makes a sig var insig or changes the sign of coeff.
# 

# Furthermore, CPI_energy has a positive coefficient -- 
# as the overall price of energy increases, we expect Elantra sales to increase, 
# which seems counter-intuitive 

# To check, compute the correlations of the variables in the training set.

# Which of the following variables is CPI_energy highly correlated with?

cor(elantraTrain$CPI_energy, elantraTrain$Unemployment)
# Strongly correlated negatively. (80%)

cor(elantraTrain$CPI_energy, elantraTrain$CPI_all)
# Strongly correlated positively. (91.32%)

cor(elantraTrain$CPI_energy, elantraTrain$Queries)
# Strongly positive corelated 83.28%

cor(elantraTrain$CPI_energy, elantraTrain$Month)
# No corelation.

# To do the above in one go.
cor(elantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])


# For Queries as well as CPI_energy whose coeff had problems in our new model3
# We see except for Month, they are moderately to highly corelated with
# other factors.

# We'll first simplify our model by removing insig var.

summary(elantra_model3)
# Queries is showing insig.
# Lets remove it first.

elantra_model4 = lm(ElantraSales ~MonthFactor + Unemployment + CPI_all + CPI_energy,
                    data = elantraTrain)

summary(elantra_model4)
# Energy's coeff is now fixed.
# Removing Query helped as it was hindering the model due to High Multi-col.
# Also model was unable to read its sig to prediction.
# Simpler Model with better Rsq = 81.8%, Adj Rsq = 69.67%

#-----------------------------------------------------------------------------

# We have to first add MonthFactor to our Test data set.
# As we have included this var in our model.

elantraTest$MonthFactor = as.factor(elantraTest$Month) 

PredElantra = predict(elantra_model4, newdata = elantraTest)

PredElantra # vector of ElantraSales value
summary(PredElantra)

SSE = sum((elantraTest$ElantraSales - PredElantra)^2)
SSE

RMSE = sqrt(SSE/nrow(elantraTest))
RMSE
# 3691.28 units
mean(elantraTest$ElantraSales) # 19973.64 units/month

# error = 3691.28/19973.64 = 18.48%

SST = sum((elantraTest$ElantraSales - mean(elantraTrain$ElantraSales))^2)

RsqElantra = 1-(SSE/SST)
RsqElantra # 72.80%

# Largest abs Error we make in our prediction

sort(abs(PredElantra - elantraTest$ElantraSales))



# In which period (Month,Year pair) do we make the largest absolute error 
# in our prediction?

elantraTest$Month[which.max(abs(PredElantra - elantraTest$ElantraSales))]
# 3 = March
elantraTest$Year[which.max(abs(PredElantra - elantraTest$ElantraSales))]
# 2013