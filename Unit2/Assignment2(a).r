climate_change = read.csv('climate_change.csv')

str(climate_change) # 308 obs/11 var.

# Splitting the data into training and test data-set.

# Training 1983-2006.

# Test 2007-2008

climate_train = subset(climate_change,  Year >= 1983 & Year <= 2006 )

str(climate_train) # 284 obs
summary(climate_train)


climate_test = subset(climate_change, Year >= 2007 & Year <= 2008 )

str(climate_test) # 24 obs


#  build a linear regression model to predict the dependent variable Temp, 
# using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent var
# (Year and Month should NOT be used in the model).
# use training data to build the model.

TempReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, 
             data = climate_train)

summary(TempReg)

# We see that N20 and CFC11 despite being topmost Greenhouse gases 
# showed negative coeff as well.
# which means higher concentration of these gases means lower Global Temp
# which is a scientific contradiction.

# So we see whether their is a problem of MultiCollinearity?

plot(climate_train$N2O, climate_train$CFC.11)

cor(climate_train$N2O, climate_train$CFC.11) # 52.24% 
# These variables are not cor with each other that much but are cor with
# other indp var in the model.


cor(climate_train)
# N2O is highly corelated with var in the model (> 0.7) ->> CO2, CH4, CFC.12, Temp

# # CFC.12 is highly corelated with var in the model (> 0.7) ->> CH4, CFC.12, Temp


# Given that the correlations are so high, let us focus on the N2O variable and build a model
# with only MEI, TSI, Aerosols and N2O as independent variables.

TempReg2 = lm(Temp ~N2O + MEI + TSI + Aerosols, data = climate_train)

summary(TempReg2)

# All came highly significant this time. N2O also which was less sig in the previous model.
# Also N2O's coeff of Reg came out to be positive.

# RSquare is reduced from 75% to 73% which is okay.


# R provides a function, step() that will automate the procedure of trying different 
# combinations of variables to find a good compromise of model simplicity and RSquare.

# step() takes the complex model as argument and produces simpler model by removing vars.

simpler_TempReg  = step(TempReg)

summary(simpler_TempReg)

# This din't helped much as only CH4 is removed, N2O is still less sig and has neg coeff.
# RSquare too is 75%.

# NOTE : step function does not address the collinearity of the variables.
# So if the var are corelated, step() is not a good option to use.
# It can be used where there are many vars and you want a simpler model by removing lesser 
# and insig vars.


# Using the model created from step function we will use it for prediction

TempPrediction = predict(simpler_TempReg, newdata = climate_test)

TempPrediction
str(TempPrediction) # 24 obs

SSE = sum((climate_test$Temp - TempPrediction)^2)

SST = sum((climate_test$Temp - mean(climate_train$Temp))^2)

RSquare_Temp = 1-(SSE/SST)

RSquare_Temp # 62.86%