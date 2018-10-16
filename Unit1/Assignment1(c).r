CPS = read.csv('CPSData.csv')

str(CPS) #1,31,302 obs and 14 variables

summary(CPS)

# If we want to know number of interviewees per region (count), but we want to know Top region.

sort(table(CPS$Region))

sort(table(CPS$State))

# What proportion of interviewees are citizens of the United States?
# Count in Native Citizen as well as Naturalized
table(CPS$Citizenship) # (116639+7073) /(131302)


# We have to find for which race we have at least 250 hispanic interviewees.

table(CPS$Race, CPS$Hispanic)
# American Indian, Black, Multiracial, White


# Which variables have at least one interviewee with a missing (NA) value?
# We can find that using Summary.

summary(CPS)

# MetroAreaCode, Married, Education, Employment Status, Industry.

# TO check na values exist for a variable, instead of using summary function we can directly use is.na()

is.na(CPS$Married)

# An entire vector is returned of the size of Married vector with TRUE/FALSE values based on whether each
# data point is na or not.

table(is.na(CPS$Married)) # we have 25,338 na values in total for Married vector.

# Now we want to see the count of nas of Married vector w.r.t region.

table(CPS$Region, is.na(CPS$Married))

table(CPS$Age, is.na(CPS$Married)) # We see a trend in NA values for Married and Age here.
# Age 0-14 have NA values consistently.

table(CPS$Sex, is.na(CPS$Married))

table(CPS$Citizenship, is.na(CPS$Married))

# We see a trend only in the Age vector w.r.t NA values of Married vector.



# Now we want to analyse how many people live in a Non-Metropolitan area. 
# For those who live in Non Metropolitan area, the value of MetroAreaCode is NA.
table(is.na(CPS$MetroAreaCode))

# we have in total 34,238 people living in Non Metro Politan areas.

# We want to see it w.r.t states.

table(CPS$State, is.na(CPS$MetroAreaCode))

# We want to see those states which have 0 False values (i.e all people have TRUE NAs and all live in 
# Non MetroPolitan area, i.e that state is a Non MetroPolitan).
# and vice versa


# People having all TRUE NAs and 0 False NAs = Non Metro = Alaska and Wyoming

# 0 TRUE NAs = Metro Politan = District of Columbia, New Jersey and Rhode Island.


# Which region of the United States has the largest proportion of interviewees 
# living in a non-metropolitan area?

table(CPS$Region, is.na(CPS$MetroAreaCode))

# Midwest = 34.78%
# Northeast = 21.62%
# South = 23.78%
# West = 24.36% 

# Calculating the proportion manually is a tedious task. Its better to use mean() function which
# on the basis of count calculates the proportion.

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
# Wisconsin (29.93%)

# Which state has the largest proportion of non-metropolitan interviewees, ignoring states where all
# interviewees were non-metropolitan?
# Montana (83.61%)


# How can mean() calculate the proportion??
# It just calculates the mean but converts TRUE to 1 and False to 0.

# (1,1,0,1) = Proportion of 1 = Mean = 3/4

MetroAreaMap = read.csv('MetroAreaCodes.csv')
CountryMap = read.csv('CountryCodes.csv')

nrow(MetroAreaMap) #271
nrow(CountryMap) #149

names(MetroAreaMap)


# Merging 2 data frames using common column. (Provided the common column is exactly identical in both).

# We want to merge CPS and MetroAreaMap data frames over MetroAreaCode/Code to map the MetroAreaCode values
# to their real values which are in MetroAreaMap dataframe.

# The resulting CPS will have MetroArea column as an additional column.

CPS = merge(CPS, MetroAreaMap, by.x = 'MetroAreaCode', by.y = 'Code', all.x = TRUE)


# If the names of the common columns were to be same in CPS and MetroAreaMap, we could have just used
# by argument.

# But since the name of the common column in CPS is MetroAreaCode and that in MetroAreaMap is Code,
# we used by.x = 'name of the column in CPS', by.y= ' name of the column in MetroAreaMap'.

# x denotes 1st dataframe that was CPS here. And y for 2nd dataframe that was MetroAreaMap here.

#  all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the rows' 
# MetroAreaCode doesn't match any codes in MetroAreaMap

# That is why we get some NA values too, because for that x we din't had a y value.


names(CPS) # We now see we have an extra column named MetroArea.

summary(CPS)

# We see that MetroArea vector has the exact names of the area mapped to the codes.


# Counting the number of intervieews by MetroArea.
sort(table(CPS$MetroArea))

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?

# Since we want to cross tab and calculate proportion, we use tapply() with mean.

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean, na.rm = TRUE))

# We can use mean here since CPS$Hispanic is in the form 1/0 vector.


# determine the number of metropolitan areas in the United States from which at 
# least 20% of interviewees are Asian.

rm(AsianCount)

AsianTrue = CPS$Race == 'Asian'

table(AsianTrue)

sort(tapply(AsianTrue, CPS$MetroArea, mean, na.rm = TRUE))
# Vallejo-Fairfield, CA, San Jose-Sunnyvale-Santa Clara, CA , San Francisco-Oakland-Fremont, CA, 
# Honolulu, HI


# Interviewees from each metropolitan area who have not received a high school diploma 

sort(tapply(CPS$Education == 'No high school diploma', CPS$MetroArea, mean, na.rm = TRUE))


# Now Merging CPS and CountryMap DataFrame into CPS.

names(CountryMap)
# We have Code and Country as variables here.
# Code is the common column and Country will be added to CPS.

names(CPS)

CPS = merge(CPS, CountryMap, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x = TRUE)
str(CPS)

# We now have Country added to our CPS dataframe.


summary(CPS)

# Which country was the most common place of birth apart from North American Countries.

sort(table(CPS$Country)) # Philippines


# What proportion of the interviewees from MetroArea "New York-Northern New Jersey-Long Island, NY-NJ-PA" 
# have a country of birth that is not the United States? For this computation,
# don't include people from this metropolitan area who have a missing country of birth.

# MetroArea vs Country of Birth

tapply(CPS$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', CPS$Country, mean, na.rm = TRUE)

# The calculation we did above gave us separate country-wise peoportion of intervieews of that MetroArea.

# But we want a single proportion.

# So we have to create a TRUE/FALSE vector of countries of US and not of US.
# and then we have to use tapply()


CountriesNotOfUS = CPS$Country != 'United States'
table(CPS$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', CountriesNotOfUS)

# We want that data which has TRUE for MetroArea as well as CountryNotOfUs

# 1668 is that data. To find the proportion we use ->> 1668/(1668+3736) = 0.309
# Out total is restricted only to New York Northern NEw Jersey Area .


# Which metropolitan area has the largest number of interviewees with a country of birth in India?
table(CPS$MetroArea , CPS$Country == 'India')


# Which metropolitan area has the largest number of interviewees with a country of birth in Brazil?
table(CPS$MetroArea , CPS$Country == 'Brazil')


# Which metropolitan area has the largest number of interviewees with a country of birth in Somalia?
table(CPS$MetroArea , CPS$Country == 'Somalia')


