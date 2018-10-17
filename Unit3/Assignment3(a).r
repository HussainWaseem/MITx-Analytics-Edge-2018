# Predicting whether a song will come in TOP 10 songs of billboard.
songs = read.csv('songs.csv')
str(songs) # 7574 obs / 39 var

nrow(subset(songs, year == 2010)) # We have 373 obs from 2010
nrow(subset(songs, artistname == 'Michael Jackson')) # 18

# Which of the songs of Michael Jackson made it to the Top 10?

jackson = (subset(songs, artistname == 'Michael Jackson'& Top10 == 1))

jackson$songtitle

# What are the discrete values of timesignature that are in our dataset?
table(songs$timesignature) # 0,1,3,4,5,7
# 8 being the most frequent and 7 being the least frequent.

# Out of our data, song which has the highest tempo?
songs$songtitle[which.max(songs$tempo)]
# Wanna be Startin Somethin

#-------------------------------------------------------------------


# Splitting our data using subset(), songs upto 2009 are in Training set and 
# songs of 2010 are in Test set.

SongsTrain = subset(songs, year>= 1990 & year <= 2009)
SongsTest = subset(songs, year == 2010)

nrow(SongsTrain) # 7201 = 95% data is in our training set.

# Building a model using all numeric and int variables 
# So we'll be excluding year, ids and names/titles.

# Since there are a lot of variables and also we are not using all of them
# we can't use ~. method.

# So we have to use another short method which could save us time writing all those
# var names in our model.

nonvars = c('year','songtitle','artistname','songID','artistID')

SongsTrain = SongsTrain[,!names(SongsTrain) %in% nonvars]

SongsTest = SongsTest[,!names(SongsTest) %in% nonvars]

str(SongsTrain) # We can see that those var are removed.

mod1 = glm(Top10 ~., data = SongsTrain, family = binomial)

summary(mod1)

# In our model we see that confidence vars are all significant.
# They have a positive coeff, that means they contribute to the songs to be in 
# Top10

# Coeff of loudness is +ve and it is sig.
# More loud, more chance of the song to be in Top10

# But energy has negative coeff and is sig.
# Less energy, more chance of the song to be in Top10.
# This is a pretty much contradiction.
# A loud song will always have large energy.

# Lets look at the multi-col issue.
cor(SongsTrain$loudness, SongsTrain$energy) # +vely cor 74%
SongsTrain

# We are having correlation issues in those 2 var, thats why the sign of coeff
# of energy is negative in our model summary.

# We will be removing loudness first.

mod2 = glm(Top10 ~.-loudness, data = SongsTrain, family = binomial)
# Here we can do a subtraction only if the subtracted variable is numeric.

# since our names and ids and year wasn't numeric we had to use a different method.

summary(mod2)

# We now see that our energy now has a positive coeff.
# More energetic songs will positively contribute to Top10.
# But we also see that energy is not sig in this model.

# Now removing energy and keeping loudness in a new model.

mod3 = glm(Top10~.-energy, data = SongsTrain, family = binomial)

summary(mod3)

# loudness has +ve coeff and is sig.
# so we'll use this model to make our predictions.
# We'll directly make predictions on the test set.

predsongs1 = predict(mod3, newdata = SongsTest, type = 'response')

table(SongsTest$Top10, predsongs1 >= 0.45) # confusion matrix with t of 0.45

# Overall Accuracy of the model = 309+19/373 = 87.9%
# Sensitivity = 19/59 = 32%
# Specificity = 309/314 = 98.4%


table(SongsTrain$Top10) # Our baseline model was predicting 0 with 85.2% 
# and 1 with 14.7%
# 0 is the most common outcome, So our baseline will predict 0 for most of the data.

# Checking the accuray of our baseline model on the test set.
1- mean(SongsTest$Top10)
# 84.18% 
# Our model has 87.9%, performing slightly better than baseline.



#-------------------------------------------------------------------


# 
# # Let's view the two models from an investment perspective. 
# 
# # A production company is interested in investing in songs that are highly likely 
# # to make it to the Top 10. The company's objective is to minimize its risk of 
# # financial losses attributed to investing in songs that end up unpopular.
# 
# # A competitive edge can therefore be achieved if we can provide the 
# # production company a list of songs that are highly likely to end up in the Top 10. 
# # We note that the baseline model does not prove useful, 
# # as it simply does not label any song as a hit. 
# 
# # Let us see what our model has to offer.


# How many songs does Model 3 correctly predict as Top 10 hits in 2010 
# (remember that all songs in 2010 went into our test set), using a threshold of 
# 0.45?

table(SongsTest$Top10, predsongs1 > 0.45) # 19

# How many non hits were predicted ad hits? # 5

# Our model rarely predicts a song to be in Top10.
# So our predictions for those songs are very strong.


