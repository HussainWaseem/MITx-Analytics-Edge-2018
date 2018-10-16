mvt = read.csv('mvtWeek1.csv')

str(mvt)

# We have got 1,91,641 observations and 11 variables.

# That means 191641 rows and 11 columns.

# Using the "max" function, what is the maximum value of the variable "ID"?
mvt$ID[which.max(mvt$ID)]  # 9181151

# What is the minimum value of the variable "Beat"?
mvt$Beat[which.min(mvt$Beat)] # 111

# How many observations have value TRUE in the Arrest variable
ArrestTrue = subset(mvt, mvt$Arrest == TRUE)

str(ArrestTrue) # 15536
nrow(ArrestTrue) # 15536


# How many observations have a LocationDescription value of ALLEY?

LocationAlley = subset(mvt, mvt$LocationDescription == 'ALLEY')

nrow(LocationAlley) # 2308

mvt$Date[1] # Date is in the form Month/Day/Year Hour:Minute

# R doesn't recognizes date automatically. We have to convert it manually.

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y%H:%M"))

summary(DateConvert, na.rm = TRUE) # Summary will only be shown if the conversion to DateTimeObject is successful.

# The strptime command is used to take a string and convert it into a time data type. 

# strptime (vector, format)

# as.Date(vector, format*)



# To extraxt months from DataTime object we use months(DateTimeobject) function.
# Similarly to extract weekdays we use weekdays(DateTimeObject) function.

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

str(mvt) # Weekday and Month column is added


# In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)  # Table just groups the values using count
# We see the least theft occured in Feb.

# NOTE - The observations in the dataset are all for Crimes. So Max count = Max Crime in that month/Weekday.


# On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)   # Friday



# Which month has the largest number of motor vehicle thefts for which an arrest was made?

table(mvt$Arrest, mvt$Month)

# We used Cross Tabs to count for each month for both TRUE and FALSE values.


# We want to plot a Histogram of a vector Date with 100 bars.

mvt$DateConvert = DateConvert
hist(mvt$DateConvert, breaks=100)

# Using this we saw the increase or decrease in the crimes over the years.


# Plotting a box plot to see the spread of the data.

boxplot(mvt$DateConvert ~mvt$Arrest)

# The plot is drawn for Date sorted over Arrest value.

# We can see that from 2001-2006 and from 2007-2012 which period has more arrest.

# And in TRUE boxplot we see that more arrest has been done in the the period
# 2001-2006 (first half of the data)




# For what proportion of motor vehicle thefts in 2001 was an arrest made?
# We have to find how many TRUE values are there in 2001.
# So we can make Cross Tabs for Year--Arrest basis.

# Proportion of TRUE and 2001 = 2152/(2152+18517) = 0.104
table(mvt$Year, mvt$Arrest)

# By fiding the propotions of arrests made during 2001, 2007 and 2012 (Start, Median and End), 
# we find the trend dropping from 10% to 8% to 3%.

# Why such falling trend?
# Since there may still be open investigations for recent crimes, 
# this could explain the trend we are seeing in the data. 

# There could also be other factors at play, and this trend should be investigated further. 
# However, since we don't know when the arrests were actually made, our detective work in this area has 
# reached a dead end.



# If we want to know which location has max theft we can find this using table-counts.

# But we have 78 different locations. So to get our value aligned we will sort our table.

sort(table(mvt$LocationDescription))

# Top 5 Location where Max thefts have occured.

# DRIVEWAY - RESIDENTIAL = 1675 
# GAS STATION = 2111 
# ALLEY = 2308                                            
# PARKING LOT/GARAGE(NON.RESID.)= 14852
# STREET = 156564 


# Now we want to take a subset of this data from our main data set.

# Subset will have theft data of only these Top-5 locations.

Top5 = subset(mvt, mvt$LocationDescription == 'DRIVEWAY - RESIDENTIAL' | mvt$LocationDescription == 'GAS STATION' |
                mvt$LocationDescription == 'ALLEY' | mvt$LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' | 
                mvt$LocationDescription == 'STREET')

nrow(Top5) # Number of observations in Top5.

str(Top5)
# A problem in R ->> If two dataset have same variable name as here we have LocationDescription in 
# mvt and Top5 both.

# If I access Tpo5$LocationDescription then I'll get some unnecessary values too from mvt dataset.

# So I need to refresh R.

Top5$LocationDescription = factor(Top5$LocationDescription)

str(Top5)


# Now we want to know which location has MAX Arrest rate.
table(Top5$LocationDescription, Top5$Arrest)

# Calcualtion proportion for arrest rate in each Top 5 locations.
# Alley = 10.7%
# Driway = 7.8%
# Gas Station = 20.8%
# Parking Lot = 10.8%
# Street = 7.4$

# On which day of the week do the most motor vehicle thefts at gas stations happen?
# Again cross Tabs. 
table(Top5$Weekday, Top5$LocationDescription)

                                          