poll = read.csv('AnonymityPoll.csv')

str(poll) # 1002 people participated in the limited version of the poll.

summary(poll)

table(poll$Smartphone) # Count of people using and not using Smartphones.

# Summary tells us how many NA are there for Smartphone variable.

# To see how many males and females have participated in the poll from various regions.
table(poll$Sex, poll$Region)

# Which of the following are states in the Midwest census region?

table(poll$ State, poll$Region == 'Midwest')

# We will get all the states listed with values under TRUE and FALSE.

# Wisconsin, South Dakota, Ohio, North Dakota, Nebraska, Missouri, Michigan, Kansas, Iowa, Indiana,
# Illinois.


# Which was the state in the South census region with the largest number of interviewees?

table(poll$State, poll$Region == 'South')

# So here we have to see all TRUE values and the one which is largest.


# How many interviewees reported not having used the Internet and not having used a smartphone?

# We have to use Cross Tabs.

table(poll$Internet.Use, poll$Smartphone) # 186

# Using both = 470

# Internet but no smartphone = 285

# No internet but a smartphone = 17

# How many interviewees have a missing value for their Internet use?

summary(poll)
# Only 1 NA for Internet use.
# 43 NA for smartphone use.

limited = subset(poll, poll$Internet.Use == 1 | poll$Smartphone == 1)
nrow(limited)

# We have 792 participants in poll who use either of them or both.


# Now we will use this 'limited' data frame for our further analysis.

summary(limited)


# What is the average number of pieces of personal information on the Internet, 
# according to the Info.On.Internet variable?
# Mean in the summary under Info.on.Internet = 3.795


# How many interviewees reported a value of 0 for Info.On.Internet?
table(limited$Info.On.Internet) # 105
# max 11 = 8

# What proportion of interviewees who answered the Worry.About.Info question 
# worry about how much information is available about them on the Internet? 

# This variable has 1/0 as options.
table(limited$Worry.About.Info)
# Proportion = 386/(386+404) = 0.4886 = 48.86%

# In above problem, we have used only those people who did replied to our question as our Total.



# What proportion of interviewees who answered the Anonymity.Possible question think it is 
# possible to be completely anonymous on the Internet?

# Again 1/0 option variable.

table(limited$Anonymity.Possible)
# Proportion = 278/(278+475) = 0.3691 = 36.91%

# Also note, for all those variables having 1/0 as option, summary is the easiest way to see the proportion.
# As mean = proportion for 1/0 option variable.
summary(limited)


# What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their 
# identity on the Internet?
# Using summary mean for Tried.Masking.Identity = 0.1633 = 16.33%


# What proportion of interviewees who answered the Privacy.Laws.Effective question 
# find United States privacy laws effective?

# Summary of this variable = mean = 0.2559 = 25.99%


# To see the frequency of Age which has many values so it will be tough
# to view it in a table form to comment on it.
# Build a histogram of the age of interviewees. What is the best represented age group in the population?

hist(limited$Age, col = 'blue', breaks = 50)
# We see that highest frequency is of people aged about 60 years old.


# Both Age and Info.On.Internet are variables that take on many values, 
# so a good way to observe their relationship is through a graph.

plot(limited$Age, limited$Info.On.Internet, col = 'red')

# But since we have so many data points spreaded from Age 18-93, we can't read the graph 
# properly.



# What is the largest number of interviewees that have exactly the same value in their
# Age variable AND the same value in their Info.On.Internet variable?

# We'll use table()

table(limited$Age, limited$Info.On.Internet)

# Here we are cross tabulating Age and Info.on.Internet.
# So we'll get all ages and all values on Info.
# Data will be filled on count.

# We have to report Max count = 6


# jitter() ->> Adds or Subtracts some random noise to the numeric vector.
# Everytime gives different result since the noise is randomly added.
# Used when plots have overlapping scatter dots.
# Noise makes the dots move up/down slightly from their original location, so the resulting
# plot looks more clear and readable.

plot(limited$Age, limited$Info.On.Internet)
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

# Comparing both the plots we can clearly see the difference.

# In the 2nd plot the dark region is the unwanted noise but it is helping us to 
# know the accumulation of dots.

# Older age group has moderate vote for smaller values of Info.on.Internet.



# What is the average Info.On.Internet value for smartphone users?
limited$Info.On.Internet

# We will calculate mean for all Info.on.internet values falling under TRUE.
# Similarly for False.
tapply(limited$Info.On.Internet, limited$Smartphone, mean, na.rm = TRUE)

# Mean for S-users = 4.36 ~ Approximately 4 = Their cell-phone number
# Mean for Not users = 2.92 ~ Approximately 3 = Their Home phone number.


# Tried Masking identity and Smartphone Users/Non Users. Mean.

tapply(limited$Tried.Masking.Identity, limited$Smartphone, mean, na.rm = TRUE)

# Tried Masking for S-Users = Mean value = Proportion (since Tried.masking is 1/0) = 0.19
# Tried Masking for Non S-Users = Mean Value = Proportion = 0.117

