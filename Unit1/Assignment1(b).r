IBM = read.csv('IBMStock.csv')
CocaCola = read.csv('CocaColaStock.csv')
ProcterGamble = read.csv('ProcterGambleStock.csv')
GE = read.csv('GEStock.csv')
Boeing = read.csv('BoeingStock.csv')

# Each data frame has two variables, described as follows:
# 
# # Date: the date of the stock price, always given as the first of the month.
# # StockPrice: the average stock price of the company in the given month.

# Converting the date of each dataset in a DateObject.
# as.Date(vector, format*)
IBM$Date = as.Date(IBM$Date, '%m/%d/%y')
CocaCola$Date = as.Date(CocaCola$Date, '%m/%d/%y')
ProcterGamble$Date = as.Date(ProcterGamble$Date, '%m/%d/%y')
GE$Date = as.Date(GE$Date, '%m/%d/%y')
Boeing$Date = as.Date(Boeing$Date, '%m/%d/%y')


str(IBM) # 480 obs

# Earliest Year is 1970

IBM$Date[480]
# Largest Year is 2009



# Mean stock Price of IBM

mean(IBM$StockPrice, na.rm= TRUE)

# Min Stock Price of GE

GE$StockPrice[which.min(GE$StockPrice)]

# Max Stock Price of CocaCola

CocaCola$StockPrice[which.max(CocaCola$StockPrice)]

# Median Stock Price of Boeing 

median(Boeing$StockPrice, na.rm= TRUE)

# Standard Dev of ProcterGamble Stock Prices
sd(ProcterGamble$StockPrice)


# Plotting a Scatter plot for Coca Cola (Scatter Plots helps to find the corelation between 2 variables)

# plot(variable on x axis, variable on y axis)

plot(CocaCola$Date, CocaCola$StockPrice)

# We see scatter dots. If we want to see a line. We use 'l' as an argument.

plot(CocaCola$Date, CocaCola$StockPrice, 'l')


# We can see in which year we had max Stock price of CocaCola 

# 1973

# Min stock price year of CocaCola = 1980

lines(ProcterGamble$Date, ProcterGamble$StockPrice)
# lines() function is same as plot but it only creates a line plot.

# Adding colors to both the lines.


plot(CocaCola$Date, CocaCola$StockPrice, 'l', col = 'red')

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue', lty = 2)

# lty = 2 makes the line dashed

# In 2000, Which copamny's stock price dropped more? = ProcterGamble

# This command adds a verticle line on that date.

abline(v=as.Date(c("2000-03-01")), lwd=2)


abline(v=as.Date(c("1983-01-01")), lwd=2)

# Using these lines we can pin point an increase/decrease in the data.
# lwd is for making line thicker.



CocaCola$Date[301:432]
# This slice has the data from 1995- 2005

# We want to plot the stock prices in this date range for CocaCola.
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], "l", col="red", ylim=c(0,210))


# NOTE - lines() adds lines to existing scatter/line plot. It 
# doesnot creates new line plot.

lines(IBM$Date[301:432],IBM$StockPrice[301:432], col = 'black')
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432], col = 'blue')
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432], col = 'orange')
lines(GE$Date[301:432],GE$StockPrice[301:432], col = 'green')


# In October of 1997, there was a global stock market crash that was caused by an economic crisis
# in Asia. Comparing September 1997 to November 1997, which companies saw a decreasing trend in 
# their stock price?

# Blue line = ProcterGamble
# Orange line = Boeing



abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)


# In the last two years of this time period (2004 and 2005) which stock seems to be performing 
# the best, in terms of increasing stock price?


abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-01-01")), lwd=2)



# Boeing


# We want to now know the mean stock prices of IBM sorted month wise.

# Since we have to apply mean function after grouping and not just grouping we use tapply().

# We dont have a month vector. So we need to extract it from Date and Store it in our IBM Dataframe.

IBM$Month = months(IBM$Date)
tapply(IBM$StockPrice, IBM$Month, mean, na.rm = TRUE)

mean(IBM$StockPrice)

# Now lets find out which Month has Stock Prices greater than Average IBM Stock price?
# April, Feb, Jan, March, May.


# Lets find the Highest Stock price of CocaCoal and GE as they both have the same month for this.
GE$Month = months(GE$Date)
CocaCola$Month = months(CocaCola$Date)

tapply(GE$StockPrice, GE$Month, mean, na.rm = TRUE)

# So in April they have max avg Stock Price value.

