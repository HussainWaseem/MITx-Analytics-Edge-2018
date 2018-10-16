# Rules for writing Identifiers in R. 
# Identifiers can be a combination of letters, digits, period (.) and underscore (_). 
# It must start with a letter or a period. If it starts with a period, it cannot be followed by a digit.



# R only allows one data type in a vector (either string, int, float, complex)

# x:y prints all data from x to y both included.
# seq(start, end, jump) function prints data from start to end with the required jump. start and end are included.

# jump can be length = x

# in that case the data will be spreaded over the length x.



# Vectors are 1D data represetation.

# Data frames are 2D data representation (like Pandas or Spreadsheets).

Country = c('Brazil', 'India', 'Mexico', 'Pakistan')

LifeExpectancy = c(75,65,78,62)

CountryData = data.frame(Country, LifeExpectancy)

# Combining vectors to form a Data Frame.

print(CountryData)


# To add a new column to the existing DataFrame we use a dollar sign

CountryData$Population = c(199000,600000,100000,520000)

print(CountryData)


Country = c('Australia', 'Switzerland')
LifeExpectancy = c(80,83)
Population = c(80000, 75000)
NewCountryData = data.frame(Country, LifeExpectancy, Population)

print(NewCountryData)



# rbind function called as rowbind function stacks up rows.


AllCountryData = rbind(CountryData, NewCountryData)

print(AllCountryData)




# Vectors = Columns.  Each vector defined, takes a column in a data frame.

# To add a new vector (Column) to a data frame ->> Use a $ sign 

# Observations = Rows 

# To add a new observation (row) to the data frame ->> Use rbind function


# Here set your working directly where the csv file is.
# USing Session ->> Set working directory ->> Files Pane location

dataWho = read.csv('Who.csv')

print(dataWho)


print(str(dataWho))

#str(dataWho) returns the total number of observations , variable names and 
# some description of each variable by its side (data type and initial values).


# subset(dataframe, criteria) ->> returns a subset of dataframe fulfilling 
# that criteria.


subset_data_who = subset(dataWho, Region == 'Europe')

print(str(subset_data_who))

# Only 53 out of 194 observations were returned.


# Summary Statistics of the data using summary(dataframe)

# Min, Max, Mean, 1st and 3rd Quartile, Median and number of NA 
# for all numeric data.

# For categorical data ->> How many times it comes. (count)

print(summary(dataWho))


# write.csv(Dataframe, Filename) ->> Writes that Dataframe into a new file 
# and save it in the working directory


# ls() ->> lists all variable names used in the script.
print(ls())

# rm(variable name) ->> Removes the variable name from ls. You can't access
# the variable anymore.

# NOTE ->> ls only lists the variables you created in the script.
# Variables present inside the data frame are not listed.

# To access the variables inside the data frame you have to use dataframe$variable.

print(dataWho$Under15)

print(mean(dataWho$Under15))

print(sd(dataWho$Under15))

# We can also find the summary of a particular column (vector)

print(summary(dataWho$Under15))

# To find which obdervation has the min or max value

print(which.min(dataWho$Under15))  #prints out obs (row) number 86

print(which.max(dataWho$Under15)) #124

# To access 86 and 124 we can index the vector Under15

print(dataWho$Under15[86])

print(dataWho$Under15[124])

# We get the min and max values.

# To know which countries correspond to these values

print(dataWho$Country[86])
print(dataWho$Country[124])


# To plot we use ->> plot(x axis variable, y axis variable)

plot(dataWho$GNI, dataWho$FertilityRate)

#It plots a scatter plot

# Each dot in the scatter plot is a country.

# There are some outliers in the plot too, GNI is greater than 10,000 and 
# fertility rate is greater than 2.5

Outliers = subset(dataWho, GNI > 10000 & FertilityRate > 2.5)

nrow(Outliers)  # returns number of rows in the subset data frame Outliers.

# We got 7. So there are 7 countries which fall in the given criteria.

print(Outliers$Country)

# If we want to extract multiple variables at once from a data frame we use 
# c function (c indexing).

Outliers[c('Country', 'GNI', 'FertilityRate')]


# To find which country has Largest literacy rate ->>

dataWho$Country[which.max(dataWho$LiteracyRate)]


# To draw a histogram ->> hist(Vector)

hist(dataWho$CellularSubscribers)

# Plotting a box plot ->> boxplot(y~x) ->> Plots y values for each x.

# To put in labels boxplot(y~x, xlab ='', ylab = '')

# To put in colors use col= ''

# To make it horizontal, use horizontal = TRUE

# To put a main label, use main = ''


boxplot(dataWho$LifeExpectancy ~dataWho$Region, xlab ='', ylab = 'Life Expectancy
        ', main = 'Life Expectancy of countries by region', col = 'orange')



# tapply(varaible of interest data-wise, variable of grouping, function to apply)

# It applies the function on variable of interest and group the results using the
# 2nd variable.

tapply(dataWho$LiteracyRate, dataWho$Region, min, na.rm = TRUE)


# It creates groups by region, puts in min Literacy rates of each.

tapply(dataWho$GNI, dataWho$Country, mean)

# Groups of countries are created and we get mean GNI for each country.

tapply(dataWho$ChildMortality, dataWho$Region, mean)

