getwd() # returns the working directory we are in.

USDA = read.csv('USDA.csv')

str(USDA)
summary(USDA)

names(USDA) #returns the variables of the Dataframe

# In the summary above we found an extreme max value of the Sodium content.

USDA$Sodium

which.max(USDA$Sodium)  #265th observation has max sodium value

# To know which food it is

USDA$Description[which.max(USDA$Sodium)] #Table salt

# Now we want to see how many foods have sodium level above 10000 in every 100gm of food

HighSodium = subset(USDA, Sodium > 10000)

str(HighSodium)  # We see that there are 10 observations

nrow(HighSodium) # We see that there are 10 observations

# To find the names of these 10 foods

HighSodium$Description


# To find the index of a particular observation we use match function

# match(name_to_be_matched, Vector)

match('CAVIAR', USDA$Description)    # 4154 obs number

USDA$Sodium[4154]

USDA$Sodium[match('CAVIAR', USDA$Description)] #1500

# Lets see the mean of the Sodium vector

mean(USDA$Sodium, na.rm= TRUE)

# We could see mean, median, etc in one go using summary of the vector.

summary(USDA$Sodium)

sd(USDA$Sodium, na.rm = TRUE)


# Plotting our Varibles

plot(USDA$Protein, USDA$TotalFat, xlab = 'Protein', ylab = 'Fat', main = 'Protein vs Fat levels', col = 'red')

hist(USDA$VitaminC, xlab = 'VitC levels per 100mg', main = 'Histogram of Vit C levels', col = 'Blue')

# We see that most of the food in our 7080 foods have vit c level less than 200mg.

# So lets limit our x axis to 100 mg.

hist(USDA$VitaminC, xlab = 'VitC levels per 100mg', main = 'Histogram of Vit C levels', col = 'Blue',
     xlim = c(0,100))

# We see only a Uniform graph.

# We need to put breaks in cells to view it separately for each food item.

hist(USDA$VitaminC, xlab = 'VitC levels per 100mg', main = 'Histogram of Vit C levels', col = 'Blue',
     xlim = c(0,100), breaks = 100)


# Breaks = Number of cells we will see

# But we don't see 100 cells. Why? 

# Originally our x axis has max value of 2000.

# We divided it by 100

# So now x axis is spreaded over multiples of 20.

# We need to divide it by 2000 originally to get individual food items.

hist(USDA$VitaminC, xlab = 'VitC levels per 100mg', main = 'Histogram of Vit C levels', col = 'Blue',
     xlim = c(0,100), breaks = 2000)

# Now we see 100 breaks each spaced at 1mg.

# We see that max number of food items have less than a mg vit c.



boxplot(USDA$Sugar, main = "Box plot for Sugar Level", ylab = 'Sugar(mg)', col = 'orange')

# We see that the spread of sugar level is very less ->> That means most of the foos have less than 20mg of sugar.

# But there are some outliers too going up to have 100mgsugar in 100mg food.

USDA$Description[which.max(USDA$Sugar)] #Sugar itself

USDA$Sugar[which.max(USDA$Sugar)] # 99.8 mg

HighSugar = subset(USDA, Sugar > 80)

nrow(HighSugar)

# We have 30 food items having very high sugar.

# If we want a vector to have all TRUE values for foods having Sodium content greater than mean.

HighSodiumVector = USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE)

# We are comparing a vector with a vector.
# Resultant is the vector formed after corresponding element-element comparison.

str(HighSodiumVector)

# If we want to store 1/0 instead of True/False we have to recreate the vector using as.numeric function.

HighSodiumVector = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm = TRUE))

str(HighSodiumVector)

# Renaming HighSodiumVector as HighSodium and adding it to our USDA data frame.

USDA$HighSodium <- HighSodiumVector

str(USDA)

# Similarly adding HighProtein, HighFat and HighCarbs to our data frame.

USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm= TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm = TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm = TRUE))

str(USDA)

# Now if we want to convert a vector in a form of table (showing count of each type)

table(USDA$HighSodium)

table(USDA$HighFat)

# Now if we want to make Cross Tables/ 2way frequency tables.

table(USDA$HighSodium, USDA$HighFat)

# Since we have used Sodium first, the rows having 0 and 1 are of Sodium and columns having 0 and 1 are of Fat.


# Now we want to see what is the average content of iron in the food having low protein content and 
# average iron content in food having high protein content.

tapply(USDA$Iron, USDA$HighProtein, mean, na.rm = TRUE)

# Firstly it will group the data as food with 1 as HighProtein value and food with 0 as HighProtein value.

# Then all the values of iron falling under 1, mean is calculated for them.
# And for all values falling under0, mean is calculated for them.


# If we want to do the same for Vitamin C variable, grouping it by High and Low Carbs.

tapply(USDA$VitaminC, USDA$HighCarbs, mean, na.rm = TRUE)

# We see that food having High carbs have really High Vit c content.

# Using tapply() we are able to find corelation overview among these 2 variables.


