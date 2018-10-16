statedata = read.csv('statedata.csv')

str(statedata) # 50 obs / 15 var

plot(statedata$x, statedata$y, col = 'red') # Lat and Long are plotted of each state
# and the outline of US map is formed.

# determine which region of the US (West, North Central, South, or Northeast) 
# has the highest average high school graduation rate of all the states in the region.

tapply(statedata$HS.Grad, statedata$state.region, mean, na.rm = TRUE)
# West

# Now, make a boxplot of the murder rate by region

boxplot(statedata$Murder ~ statedata$state.region, col = 'orange')

# Highest median Murder rate is of ->> South
# We also noted from previous tapply() that South has poorest HS grad %

# This might have been one of the reasons of increased crime rate in the region.

plot(statedata$HS.Grad, statedata$Murder)
cor(statedata$HS.Grad, statedata$Murder) # -0.4879 ~ Almost 50% negatively cor.


# In the box plot plotted above, there is an outlier found in the NorthEast region.
# Which state is it?

MurderG10 = subset(statedata, statedata$Murder > 10 & statedata$state.region == 'Northeast')
MurderG10
# Its index 32 ->> NewYork


# Also we could have found out the same using cross tabs.

table(statedata$Murder, statedata$state.region)
# Northeast = 10.9

match(statedata$Murder, 10.9)
# You found 1 in 32nd index
statedata$state.name[32] # NewYork
# 
# --------------------------------------------------------------------


# Building a model to predict Life Expectancy of states using state stats.

#  (Population, Income, Illiteracy, Murder, HS.Grad, Frost, and Area).

model1 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + 
              Frost + Area, data = statedata)

summary(model1)


# Coeff of Regression of any var means ->> For unit increase/decrease in the var
# the predicted value will increase/decrease in any order by |value of coeff|.

plot(statedata$Life.Exp, statedata$Income)
cor(statedata$Life.Exp, statedata$Income)
# 34.2% ~ Somewhat corelated.

# From above corelation output we see that income should contribute to predict
# Life.Exp but it doesnot.

# However, in the presence of all of the other variables, 
# income does not add statistically significant explanatory power to the model. 
# This means that multicollinearity is probably the issue.



# 
# # HOW TO TACKLE MULTICOLLINEARITY?
# 
# # Experiment with removing independent variables from the original model. 
# # Remember to use the significance of the coefficients to decide which variables 
# # to remove (remove the one with the largest "p-value" first, or the one with the 
# # "t value" closest to zero), and to remove them one at a time 
# # (this is called "backwards variable selection"). 
# 
# # This is important due to multicollinearity issues - removing one insignificant 
# # variable may make another previously insignificant variable become significant.
# 
# 

# Removing Area first.

model2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + 
              Frost, data = statedata)
summary(model2)

# No improvement in the individual sig of the var and no improvment in RSq as well.
# But model became simpler.

# Removing Illiteracy also gave us no improvement on model.
# Removing Income.
model3 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost , data = statedata)
summary(model3)


# Population + Murder + HS.Grad + Frost ->> All are sig.
# Much simpler model and RSq is also not sacrificed in the tradeoff.



# # This model with 4 variables is a good model. 
# # However, we can see that the variable "Population" is not quite significant. 
# # In practice, it would be up to you whether or not to keep the variable 
# # "Population" or eliminate it for a 3-variable model. 
# 
# # Population does not add much statistical significance in the presence of murder, 
# # high school graduation rate, and frost days. 
# 
# # However, for the remainder of this question, we will analyze the 4-variable model.
# 


cor(statedata$Life.Exp, statedata$Population) # Only -6.8% (Weak)
cor(statedata$Life.Exp, statedata$Murder) # -78% (Good)
# As murder increases, Life.Exp decreases. Logical.

cor(statedata$Life.Exp, statedata$HS.Grad) # 58% (Okay)
# As Education in area increases, Life.Exp increases due to good standard of living.

cor(statedata$Life.Exp, statedata$Frost) # 26% but it has negative coeff.
# True. If frost (freezing temp) is increasing, chances of deaths increases.

summary(model1) # RSq = 73.62%, AdjRSq = 69.2% ->> All variables
summary(model3) # Rsq = 73.6%, Adj RSq = 71.26% ->> Only 4 sig variables.


# WHY MULTIPLE RSQUARE VALUE SLIGHLY DECREASES WHEN MODEL BECOMES BETTER BY
# REMOVING UNWANTED VAR.

# # When we remove insignificant variables, the "Multiple R-squared" will 
# # always be worse, but only slightly worse. 
# # This is due to the nature of a linear regression model. 
# # It is always possible for the regression model to make a coefficient zero, 
# # which would be the same as removing the variable from the model. 
# # The fact that the coefficient is not zero in the intial model means 
# # it must be helping the R-squared value, even if it is only a very small 
# # improvement. 
# 
# # So when we force the variable to be removed, it will decrease the R-squared 
# # a little bit. However, this small decrease is worth it to have a simpler model.




#  WHY ADJUSTED RSQUARE IMPROVES WHEN UNWANTED VARS ARE REMOVED?

# # On the contrary, when we remove insignificant variables, 
# # the "Adjusted R-squred" will frequently be better. 
# # This value accounts for the complexity of the model, and thus tends to 
# # increase as insignificant variables are removed, 
# # and decrease as insignificant variables are added.
# 


# Predictions. We'll be doing prediction on the training set itself.

PredLE = predict(model3)  # we won't pass in new data as we are using the same data.

PredLE # A vector with 50 values of LifeExp is created.

SSE = sum((statedata$Life.Exp - PredLE)^2)
SSE # 23.30804

RMSE = sqrt(SSE/nrow(statedata))
RMSE # 0.6827597 # Not even 1 year. Error is fine.

SST = sum((statedata$Life.Exp - mean(statedata$Life.Exp))^2)
SST

Rsq = 1-(SSE/SST)
Rsq # 73.6%

# Which state do we predict to have the lowest life expectancy?
sort(PredLE)  # 1st obs has the lowest life exp.
statedata$state.name[1] # Albama

# Lets see which state has the lowest life exp in the actual data

sort(tapply(statedata$Life.Exp, statedata$state.name, min , na.rm =TRUE))
# Its South Carolina

# We could also have done that using which.min(statedata$Life.Exp)
# This would have returned an index
# statedata$state,name[index] would have given South Carolina.




# Which state do we predict to have the highest life expectancy?

sort(PredLE) #47th
statedata$state.name[47] #Washington

# Which state actually has the highest life expectancy?
statedata$state.name[which.max(statedata$Life.Exp)]
# Hawaii

ResidualVector = statedata$Life.Exp - PredLE
ResidualVector # Actual value - predicted value. Its individual errors.

# For which state do we make the smallest ABSOLUTE error?
statedata$state.name[which.min(abs(ResidualVector))]
# Indiana

# NOTE : We used abs(ResidualVector)

# For which state do we make the largest absolute error?
statedata$state.name[which.max(abs(ResidualVector))]

# Hawaii
