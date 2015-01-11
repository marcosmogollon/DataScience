setwd("//dbg.ads.db.com/BER-LRC-U/wagnale/data/R/Courses/DataAnalyticsEdge")
# give R memory limit
memory.limit()
# libraries
library(parallel)
library(zoo)
library(caTools)
library(ROCR)
library(mice)
library(randomForest)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(tm)
library(SnowballC)
library(RTextTools)
library(flexclust)


######## Week 1: An Introduction to Analytics

###### Working with Data: An Introduction to R 

#### Video 3: Vectors and Data Frames

Country <- c("Brazil","China","India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)

Sequence <- seq(from=1,to=100,by=2)
Sequence

Data <- data.frame(Country,LifeExpectancy)
Data

Population <- c(199000,1390000,1240000,7997,318000)

Data2 <- cbind(Data,Population)
Data2

Country <- c("australia","Greece")
LifeExpectancy = c(82,81)
Population <- c(23050,11125)
NewData <- data.frame(Country,LifeExpectancy,Population)
NewData

Data3 <- rbind(Data2,NewData)
Data3


#### Video 4: Loading Data Files

# set working directory
setwd("//dbg.ads.db.com/BER-LRC-U/wagnale/data/R/Courses/DataAnalyticsEdge")
# check working directory
getwd()

# read in data
WHO <- read.csv("WHO.csv")

# look at the data (number, levels, name, type, first obs)
str(WHO)

# get summary statistics
summary(WHO)

# subset data
WHO_Europe <- subset(WHO, Region=="Europe")
str(WHO_Europe)

# write data to csv
write.csv(WHO_Europe,"WHO_Europe.csv")

# see varaibles in work space and remove Europe
ls()
rm(WHO_Europe)
ls()

#### Video 5: Data Analysis

# access variable in data frame
WHO$Under15

mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)

# returns row of observation which has min/max of Under15 variable
which.min(WHO$Under15)
WHO$Country[which.min(WHO$Under15)]
WHO$Country[which.max(WHO$Under15)]

# plot
plot(WHO$GNI, WHO$Fertility, col="blue", pch=1)

# look at outliers
Outliers <- subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]

# histogram
hist(WHO$CellularSubscribers, col="blue")

# boxplot
boxplot(WHO$LifeExpectancy ~ WHO$Region, col="blue", xlab="Region",
        ylab="Life Expectancy", main="Life Expectancy of Countries by Region")

# create table for factor variables
table(WHO$Region)

# tapply for numeric variables
tapply(X=WHO$Over60, INDEX=WHO$Region, FUN=mean, simplify=T)
tapply(WHO$LiteracyRate, WHO$Region, min)
# remove NAs
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=T)

#### Quick Question 5 (3 points possible)
# 1.
mean(WHO$Over60)

# 2.
WHO$Country[which.min(WHO$Over60)]

# 3.
WHO$Country[which.max(WHO$LiteracyRate)]


###### Understanding Food: Nutritional Education with Data 

#### Video 1: The Importance of Food and Nutrition

#### Video 2: Working with Data in R

# set working directory
setwd("//dbg.ads.db.com/BER-LRC-U/wagnale/data/R/Courses/DataAnalyticsEdge")
# check working directory
getwd()

# Looking at data
USDA <- read.csv("USDA.csv")
str(USDA)
summary(USDA)

#### Video 3: Data Analysis

names(USDA)
USDA$Description[which.max(USDA$Sodium)]

# check highest sodium
HighSodium <- subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description

# check caviar sodium level
USDA$Sodium[match("CAVIAR", USDA$Description)]
# check caviar sodium level but case insensitive
USDA$Sodium[grep("caviar", USDA$Description, ignore.case = TRUE)]

# look at basic statistics
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm=T)


#### Video 4: Creating Plots in R

# plot protein vs fat
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", 
     main="Protein vs Fat", col="blue")

# histogram of vitamin C
hist(USDA$VitaminC, xlab="Vitamin C (mg)", 
     main="Histogram of Vitamin C Levels", xlim=c(0, 100), 
     breaks=2000, col="blue")

# boxplot for sugar
boxplot(USDA$Sugar, main="Boxplot of Sugar", ylab="Sugar (g)")


#### Video 5: Adding Variables

USDA$Sodium[1] > mean(USDA$Sodium, na.rm=T)
HighSodium <- USDA$Sodium > mean(USDA$Sodium, na.rm=T)
str(HighSodium)
# change data type to numeric to have logical exression to numeric i.e. 1 or 0
HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
str(HighSodium)

# add to data frame
USDA$HighSodium <- as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=T))
USDA$HighProtein <- as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=T))
USDA$HighFat <- as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=T))
USDA$HighCarbs <- as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=T))
str(USDA)


#### Video 6: Summary Tables

# how many foods have lower (0) or higher (1) than average
table(USDA$HighSodium)
table(USDA$HighSodium, USDA$HighFat)

# average amount of iron sorted by low or high protein
tapply(USDA$Iron, USDA$HighProtein, FUN=mean, na.rm=T)

# maximum level of vitamin C in foods with high or low carbs
tapply(USDA$VitaminC, USDA$HighCarbs, FUN=max, na.rm=T)

# in general is it true that foods with hich carbs have high vitamin C
tapply(USDA$VitaminC, USDA$HighCarbs, FUN=summary, na.rm=T)


###### Week 1 Assignment  

####  Problem 1 - Loading the Data 

mvt <- read.csv("mvtWeek1.csv")
# How many rows of data (observations) are in this dataset?
# How many variables are in this dataset?
str(mvt)
# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)
# What is the minimum value of the variable "Beat"?
min(mvt$Beat)
# How many observations have value TRUE in the Arrest variable
length(mvt$Arrest[mvt$Arrest == TRUE])

# How many observations have value a LocationDescription value of ALLEY?
# Watch out for 3 cases of "Bowling Alley"
unique(mvt$LocationDescription)
mvt$LocationDescription[(grep("ALLEY", mvt$LocationDescription, ignore.case = F))]
length(grep("ALLEY", mvt$LocationDescription, ignore.case = F)) - length(grep("BOWLING ALLEY", mvt$LocationDescription, ignore.case = F))


####  Problem 2 - Understanding Dates in R

str(mvt)
# In what format are the entries in the variable Date?
head(mvt$Date)
DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

# What is the month and year of the median date in our dataset?
summary(DateConvert)

# Now, let's extract the month and the day of the week, and add these variables to our data frame mvt.
mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)
mvt$Date = DateConvert

# In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Month, mvt$Arrest)


####  Problem 3 - Visualizing Crime Trends

# add an extra argument, to specify the number of bars we want in our histogram

hist(mvt$Date, breaks=100)

# Does it look like crime increases or decreases from 2002 - 2012?
# -> Decrease

# Does it look like crime increases or decreases from 2005 - 2008?
# -> Decrease

# Does it look like crime increases or decreases from 2009 - 2011?
# -> Increase

# Does it look like there were more crimes for which arrests were made 
# in the first half of the time period or the second half of the time period? 
# (Note that the time period is from 2001 to 2012, so the middle of the time 
# period is the beginning of 2007.)
boxplot(mvt$Date ~ mvt$Arrest, main="Arrests over time", ylab="Time")
# -> first half

# For what proportion of motor vehicle thefts in 2001 was an arrest made? 
prop.table(table(mvt$Year[mvt$Year==2001], mvt$Arrest[mvt$Year==2001]))

# For what proportion of motor vehicle thefts in 2007 was an arrest made? 
prop.table(table(mvt$Year[mvt$Year==2007], mvt$Arrest[mvt$Year==2007]))

# For what proportion of motor vehicle thefts in 2012 was an arrest made? 
prop.table(table(mvt$Year[mvt$Year==2012], mvt$Arrest[mvt$Year==2012]))

# all in one table
prop.table(table(mvt$Year, mvt$Arrest), margin=1)


####  Problem 4 - Popular Locations 

# We want to find the top five locations where motor vehicle thefts occur.
# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
sort(table(mvt$LocationDescription))

# How many observations are in Top5?
# Create vector with five top locations
top5 <- c("STREET","PARKING LOT/GARAGE(NON.RESID.)","GAS STATION","ALLEY",
          "DRIVEWAY - RESIDENTIAL")
Top5 <- mvt[mvt$LocationDescription %in% top5]
length(Top5$ID)

# To make our tables a bit nicer to read, we can refresh this factor variable
Top5$LocationDescription <- factor(Top5$LocationDescription)
str(Top5)

# One of the locations has a much higher arrest rate than the other locations. Which is it?
prop.table(table(Top5$LocationDescription, Top5$Arrest), margin=1)

# On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription, Top5$Weekday)

# On which day of the week do the fewest motor vehicle thefts in residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)


######  Stock Dynamics

IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")
str(IBM)

#### Problem 1 - Summary Statistics

# create date object for each data frame
IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
GE$Date <- as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")

# Our five datasets all have the same number of observations. How many observations are there in each data set? 
str(IBM)

# What is the earliest year in our datasets?
head(IBM$Date)

# What is the latest year in our datasets?
tail(IBM$Date)

# What is the mean stock price of IBM over this time period?
summary(IBM)

# What is the minimum stock price of General Electric (GE) over this time period?
summary(GE)

# What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola)

# What is the median stock price of Boeing over this time period?
summary(Boeing)

# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)


####  Problem 2 - Visualizing Stock Dynamics 

# plot the Date on the x-axis and the StockPrice on the y-axis, for Coca-Cola
plot(CocaCola$Date, CocaCola$StockPrice, col="red", type="l")

# Around what year did Coca-Cola has its highest stock price in this time period?
# -> 1973

# Around what year did Coca-Cola has its lowest stock price in this time period?
# -> 1980

# add the line for Procter & Gamble too
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")

# In March of 2000, the technology bubble burst, and a stock market crash occurred. According to this plot, which company's stock dropped more? 
# To answer this question and the ones that follow, you may find it useful to draw a vertical line at a certain date.
abline(v=as.Date(c("2000-03-01")), lwd=1, lty=2)
# -> ProcterGamble

# Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) was going up, while the other was going down. Which one was going up?
# -> Coca Cola

# In the time period shown in the plot, which stock generally has lower values?
# -> Coca Cola


#### Problem 3 - Visualizing Stock Dynamics 1995-2005

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], 
     type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="black")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")

# Which stock fell the most right after the technology bubble burst in March 2000?
abline(v=as.Date(c("2000-03-01")), lwd=1, lty=2)
# -> GE

# Which stock reaches the highest value in the time period 1995-2005?
# -> IBM

# Comparing September 1997 to November 1997, which companies saw a decreasing trend in their stock price?
abline(v=as.Date(c("1997-09-01")), lwd=1, lty=2)
abline(v=as.Date(c("1997-11-30")), lwd=1, lty=2)
# -> Procter and Gamble & Boeing

# In the last two years of this time period (2004 and 2005) which stock seems to be performing the best, in terms of increasing stock price?
# -> Boeing


####  Problem 4 - Monthly Trends 

# Lastly, let's see if stocks tend to be higher or lower during certain months. 
# Use the tapply command to calculate the mean stock price of IBM, sorted by months. 
# To sort by months, use months(IBM$Date) as the second argument of the tapply function.
# For IBM, compare the monthly averages to the overall average stock price. In which months has IBM historically had a higher stock price (on average)?
tapply(IBM$StockPrice, months(IBM$Date), FUN=mean, na.rm=T) - 
  mean(IBM$StockPrice, na.rm=T)

# General Electric and Coca-Cola both have their highest average stock price in the same month. Which month is this?
tapply(GE$StockPrice, months(GE$Date), FUN=mean, na.rm=T) - 
  mean(GE$StockPrice, na.rm=T)
tapply(CocaCola$StockPrice, months(CocaCola$Date), FUN=mean, na.rm=T) - 
  mean(CocaCola$StockPrice, na.rm=T)

# For the months of December and January, every company's average stock is higher in one month and lower in the other. In which month are the stock prices lower?
# -> December

###### Internet privacy poll

#### Problem 1 - Loading and Summarizing the Dataset

poll <- read.csv("AnonymityPoll.csv")

# How many people participated in the poll?
str(poll)

# How many interviewees responded that they use a smartphone?
# How many interviewees responded that they don't use a smartphone?
# How many interviewees did not respond to the question, resulting in a missing value, or NA?
table(poll$Smartphone, exclude=NULL)

# Which of the following are states in the Midwest census region?
# Which was the state in the South census region with the largest number of interviewees?
table(poll$State, poll$Region)

#### Problem 2 - Internet and Smartphone Users

# How many interviewees reported neither Internet use nor smartphone use?
# How many interviewees reported both Internet use and smartphone use?
# How many interviewees reported Internet use but no smartphone use?
# How many interviewees reported smartphone use but no Internet use?
# How many interviewees have a missing value for their Internet use?
# How many interviewees have a missing value for their smartphone use?
table(poll$Internet.Use, poll$Smartphone, exclude=NULL)

# How many interviewees are in the new data frame?
limited <- subset(poll, Internet.Use==1 | Smartphone==1)
str(limited)


#### Problem 3 - Summarizing Opinions about Internet Privacy

# Which variables have missing values in the limited data frame?
summary(limited)

# What is the average number of pieces of personal information on the Internet, according to the Info.On.Internet variable?
summary(limited)

# How many interviewees reported a value of 0 for Info.On.Internet?
length(limited$Info.On.Internet[limited$Info.On.Internet==0])

# How many interviewees reported the maximum value of 11 for Info.On.Internet?
length(limited$Info.On.Internet[limited$Info.On.Internet==11])

# What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet?
prop.table(table(limited$Worry.About.Info))

# What proportion of interviewees who answered the Anonymity.Possible question who think it is possible to be completely anonymous on the Internet?
prop.table(table(limited$Anonymity.Possible))

# What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
prop.table(table(limited$Tried.Masking.Identity))

# What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
prop.table(table(limited$Privacy.Laws.Effective))


#### Problem 4 - Relating Demographics to Polling Results

# Build a histogram of the age of interviewees. What is the best represented age group in the population?
hist(limited$Age)
# -> People aged about 60 years old 

# What is the largest number of interviewees that have exactly the same value in their Age variable AND the same value in their Info.On.Internet variable? 
plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Info.On.Internet, limited$Age))

# Experimenting with the command jitter(c(1, 2, 3)), what appears to be the functionality of the jitter command?
jitter(c(1, 2, 3))
# -> jitter adds or subtracts a small amount of random noise to the values passed to it, and two runs will yield different results 

# Now, plot Age against Info.On.Internet with plot(jitter(limited$Age), jitter(limited$Info.On.Internet)). What relationship to you observe between Age and Info.On.Internet?
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
# -> Older age seems moderately associated with a smaller value for Info.On.Internet

# Use the tapply() function to obtain the summary of the Info.On.Internet value, broken down by whether an interviewee is a smartphone user.
# What is the average Info.On.Internet value for smartphone users?
# What is the average Info.On.Internet value for non-smartphone users?
tapply(limited$Info.On.Internet, limited$Smartphone, FUN=mean, na.rm=T)
# using aggregate
aggregate(limited$Info.On.Internet, by=list(limited$Smartphone), 
          FUN=mean, na.rm=T, simplify=T)

# Similarly use tapply to break down the Tried.Masking.Identity variable for smartphone and non-smartphone users.
# What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
# What proportion of non-smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
tapply(limited$Tried.Masking.Identity, limited$Smartphone, FUN=mean, na.rm=T) 
# using aggregate
aggregate(limited$Tried.Masking.Identity, by=list(limited$Smartphone), 
          FUN=mean, na.rm=T, simplify=T)


###### Demographics and employment in the united states

#### Problem 1 - Loading and Summarizing the Dataset

CPS <- read.csv("CPSData.csv")

# How many interviewees are in the dataset?
str(CPS)

# Among the interviewees with a value reported for the Industry variable, what is the most common industry of employment? 
table(CPS$Industry)

# Which state has the fewest interviewees?
sort(table(CPS$State), decreasing=F) 

# Which state has the largest number of interviewees?
sort(table(CPS$State), decreasing=T) 

# What proportion of interviewees are citizens of the United States?
prop.table(table(CPS$Citizenship, exclude=NULL))
prop.table(table(CPS$Citizenship, exclude=NULL))[1] + 
  prop.table(table(CPS$Citizenship, exclude=NULL))[2]


####  Problem 2 - Evaluating Missing Values

# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic)

# Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)

# We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
# -> The Married variable being missing is related to the Age value for the interviewee.

# How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? For this question, treat the District of Columbia as a state (even though it is not technically a state).
prop.table(table(CPS$State, is.na(CPS$MetroAreaCode)), margin=1)

# Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)), margin=1)

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
prop.table(table(CPS$State, is.na(CPS$MetroAreaCode)), margin=1)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))


#### Problem 3 - Integrating Metropolitan Area Data

# Read these two dictionaries into data frames MetroAreaMap and CountryMap.
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

# How many metropolitan areas are stored in MetroAreaMap?
str(MetroAreaMap)

# How many countries are stored in CountryMap?
str(CountryMap)

# To merge in the metropolitan areas, we want to connect the field MetroAreaCode from the CPS data frame with the field Code in MetroAreaMap. 
# left outer join since x is CPS
CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# Review the new version of the CPS data frame with the summary() and str() functions. What is the name of the variable that was added to the data frame by the merge() operation?
str(CPS)

# How many interviewees have a missing value for the new metropolitan area variable?
summary(CPS)

# Which of the following metropolitan areas has the largest number of interviewees?
table(CPS$MetroArea)

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
# -> Laredo, TX

# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether an interviewee is Asian, determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
prop.table(table(CPS$MetroArea, CPS$Race), margin=1)
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

# Passing na.rm=TRUE to the tapply function, determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T), decreasing=T)


####  Problem 4 - Integrating Country of Birth Data 

# Just as we did with the metropolitan area information, merge in the country of birth information from the CountryMap data frame, replacing the CPS data frame with the result.
str(CountryMap)
str(CPS)
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

# What is the name of the variable added to the CPS data frame by this merge operation?
str(CPS)

# How many interviewees have a missing value for the new country of birth variable?
summary(CPS)

# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(tapply(CPS$Country != "United States", CPS$MetroArea, mean, na.rm=T), decreasing=T)

# Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=T), decreasing=F)

# In Brazil?
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=T), decreasing=F)

# In Somalia?
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=T), decreasing=F)


######## Week 2: Linear Regression

###### The Statistical Sommelier: An Introduction to Linear Regression 

#### Video 4: Linear Regression in R

wine <- read.csv("wine.csv")
str(wine)
summary(wine)

# one variable linear regression
model1 <- lm(Price ~ AGST, data=wine)
summary(model1)
model1$residuals
SSE <- sum(model1$residuals^2)
SSE

# multiple variable model
model2 <- lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)
SSE <- sum(model2$residuals^2)
SSE

# model with all variables
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain + 
               Age + FrancePop, data=wine)
summary(model3)
SSE <- sum(model3$residuals^2)
SSE

# Quick Question 4
model4 <- lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model4)
SSE <- sum(model4$residuals^2)
SSE


#### Video 5: Understanding the Model

# model with all variables
summary(model3)

# removing some variables
model4 <- lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
SSE <- sum(model4$residuals^2)
SSE

# compute correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

# let's see what happens if we remove two variables at a time (which we should not due to multicolinearity)
model5 <- lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)

#  Quick Question 5 
cor(wine$HarvestRain, wine$WinterRain)
model6 <- lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model6)


#### Video 6: Making Predictions

wineTest <- read.csv("wine_test.csv")
str(wineTest)

# make predictions for the two observations in the test set
predictTest <- predict(model4, newdata=wineTest)
predictTest

# verify prediction
SSE <- sum((wineTest$Price - predictTest)^2)
SST <- sum((wineTest$Price - mean(wine$Price))^2)
R_squared <- 1 - SSE/SST
# out-of-sample R squared (this could be negative, when model does worse on test data than on baseline in original set!!)
R_squared


###### Moneyball: The Power of Sports Analytics

#### Video 2: Making it to the Playoffs

# read in data
baseball <- read.csv("baseball.csv")

# look at data
str(baseball)

# subset data to only include years before 2002
moneyball <- subset(baseball, Year < 2002)
str(moneyball)

# create run difference variable
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)

# build first linear regression model to predict wins
WinsReg <- lm(W ~ RD, data=moneyball)
summary(WinsReg)
# the model therefore is: Wins = 90.8314 + 0.1058*RD

# now confirm moneyball claim that a team needs at least 135 more runs than they allow to win at leats 95 games
# regression equation:
# Wins = 80.8814 + 0.1058*RD
# Wins >= 95
# It follows: 80.8814 + 0.1058*RD >= 95
# It follows: RD >= (95 - 80.8814) / 0.1058 -> RD >= 133.4
# Conclusion: If the run difference of a team is >= 133.4, they will win at least 95 games

# Quick Question 2
80.8814 + 0.1058*(713-614)


#### Video 3: Predicting Runs

str(moneyball)

# build new model
RunsReg <- lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
# coefficient for average batting power is negative which is counterintuitive and suggests colinearity

# regression without BA coefficient
RunsReg <- lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
# Runs = -804.63 + 2737.77*OBP + 1584.91*SLG
# it shows that on base percentage (OBP) since beign on a similar scal but much higher is worth more than slugging percentage

# Quick Question 3
# Runs scored = -804.63 + 2737.77*OBP + 1584.91*SLG
-804.63 + 2737.77*0.311 + 1584.91*0.405
# Runs allowedd = -837.38 + 2913.60*OOBP + 1514.29*OSLG
-837.38 + 2913.60*0.297 + 1514.29*0.370


# Quick Question 4
teamRank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,97,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
# What is the correlation between teamRank and wins2012?
cor(teamRank,wins2012)
# What is the correlation between teamRank and wins2013?
cor(teamRank,wins2013)


###### Recitation: Playing Moneball in the NBA

#### VIDEO 1: THE DATA

NBA <- read.csv("NBA_train.csv")
str(NBA)


#### VIDEO 2: PLAYOFFS AND WINS

# How many wins does it take for the playoffs?
table(NBA$W, NBA$Playoffs)
# -> 42

# create variable for points scored and points allowed difference
NBA$PTSdiff <- NBA$PTS - NBA$oppPTS

# make scatterplot to see relationship
plot(NBA$PTSdiff, NBA$W)

# build first model
WinsReg <- lm(W ~ PTSdiff, data=NBA)
summary(WinsReg)
# W = 41 + 0.0326*PTSdiff
# W >= 42
# PTSdiff >= (42-41) / 0.0326 -> 30.67
# We need to score at least 31 more ppoints than we allow to win at least 42 games


#### VIDEO 3: POINTS SCORED

PointsReg <- lm(PTS ~ X2PA + X3PA + FTA + AST+ ORB + 
                  DRB + TOV + STL + BLK, data=NBA)
summary(PointsReg)
PointsReg$residuals
SSE <- sum(PointsReg$residuals^2)
SSE

# calculate root mean squared error
RMSE <- sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)

# remove some of the insignificant variables, one at a time
# TOV was least significant so remove this first
# Multiple R-squared of first model:  0.8992
PointsReg2 <- lm(PTS ~ X2PA + X3PA + FTA + AST+ ORB + 
                  DRB + STL + BLK, data=NBA)
summary(PointsReg2)
# Multiple R-squared of this model:  0.8991
# so removal was justified, try another one

PointsReg3 <- lm(PTS ~ X2PA + X3PA + FTA + AST+ ORB + 
                   STL + BLK, data=NBA)
summary(PointsReg3)
# Multiple R-squared of this model:  0.8991
# so removal was justified, try another one

PointsReg4 <- lm(PTS ~ X2PA + X3PA + FTA + AST+ ORB + 
                   STL, data=NBA)
summary(PointsReg4)
# Multiple R-squared of this model:  0.8991
# so removal was justified, try another one

# compute SSE and RMSE
# first model SSE: 28394314
# first model RMSE: 184.4049
SSE <- sum(PointsReg4$residuals^2)
SSE
RMSE <- sqrt(SSE/nrow(NBA))
RMSE
# this model SSE: 28421465
# this model RMSE: 184.493
# -> RMSE stayed essentially the same but model is much simpler so we can go with this one


#### VIDEO 4: MAKING PREDICTIONS

# load test set to test model
NBA_test <- read.csv("NBA_test.csv")
PointsPrediction <- predict(PointsReg4, newdata = NBA_test)

# compute out-of-sample R_squared to see how good prediction is
# in-sample-R_quared was 0.8991 for last model
SSE <- sum((PointsPrediction - NBA_test$PTS)^2)
SST <- sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 <- 1 - SSE/SST
R2
# R_squared is 0.8127
RMSE <- sqrt(SSE/nrow(NBA_test))
RMSE
# Root mean squared error is 196.3723
# Both are slightly worse than in training set but still not bad


###### Week 2 Assignment

###### State Data

# Load the dataset and convert it to a data frame by running the following two commands in R:
  
data(state)
statedata <- cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

# Inspect the data set using the command: str(statedata)
str(statedata)

# For more information about this data set, type ?state in the R console.

####  Problem 1 - Data Exploration

# In the R command you used to generate this plot, which variable name did you use as the first argument?
plot(statedata$x, statedata$y)

# Using the tapply command, determine which region of the US (West, North Central, South, or Northeast) has the highest average high school graduation rate of all the states in the region:
tapply(statedata$HS.Grad, statedata$state.region, mean, na.rm=T)
aggregate(statedata$HS.Grad, by=list(statedata$state.region), 
          FUN=mean, na.rm=T, simplify=T)

# Now, make a boxplot of the murder rate by region
boxplot(statedata$Murder ~ statedata$state.region, col="blue")

# You should see that there is an outlier in the Northeast region of the boxplot you just generated. Which state does this correspond to?
statedata$state.name[statedata$Murder==max(statedata$Murder[statedata$state.region=="Northeast"])]


#### Problem 2 - Predicting Life Expectancy - An Initial Model

model1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + 
               HS.Grad + Frost + Area, data=statedata)

# What is coefficient for income?
summary(model1)

# Call the coefficient for income x (the answer to Problem 2.1). What is the interpretation of the coefficient x?
# -> For a one unit increase in income, predicted life expectancy decreases by |x|

# Now plot a graph of life expectancy vs. income using the command:
plot(statedata$Income, statedata$Life.Exp)

# Visually observe the plot. What appears to be the relationship?
# -> Life expectancy is somewhat positively correlated with income. 

# Which of the following explanations seems the most reasonable? 
# -> Multicollinearity


#### Problem 3 - Predicting Life Expectancy - Refining the Model and Analyzing Predictions 

model1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + 
               HS.Grad + Frost + Area, data=statedata)

summary(model1)
# R_squared: 0.7362
# remove Area
model2 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + 
               HS.Grad + Frost, data=statedata)

summary(model2)
# R_squared: 0.7361
# remove Illiteracy
model3 <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost,
             data=statedata)

summary(model3)
# R_squared: 0.7361
# remove Income
model4 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)

summary(model4)
# R_squared: 0.736

# You should be able to find a good model with only 4 independent variables, instead of the original 7. Which variables does this model contain?
# -> Population, Murder, Frost, HS.Grad

# Which of the following correctly explains the change in the Multiple R-squared value?
summary(model1)
summary(model4)
# -> We expect the "Multiple R-squared" value of the simplified model to be slightly worse than that of the initial model. It can't be better than the "Multiple R-squared" value of the initial model. 

# Which state do we predict to have the lowest life expectancy?
sort(predict(model4), decreasing=F)

# Which state actually has the lowest life expectancy?
statedata$state.name[which.min(statedata$Life.Exp)]

# Which state do we predict to have the highest life expectancy?
sort(predict(model4), decreasing=T)

# Which state actually has the highest life expectancy?
statedata$state.name[which.max(statedata$Life.Exp)]

# For which state do we make the smallest absolute error?
sort(model4$residuals, decreasing=F)


###### Climate Change

climate <- read.csv("climate_change.csv")
str(climate)

#### Problem 1 - Creating Our First Model

# Then, split the data into a training set, consisting of all the observations up to and including 2006, and a testing set consisting of the remaining years
climateTrain <- subset(climate, Year <= 2006)
climateTest <- subset(climate, Year > 2006)

# Next, build a linear regression model using all of the independent variables (except Year and Month) to predict the dependent variable Temp
model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
             data=climateTrain)

# Enter the model R2:
summary(model1)

# Which variables are significant in the model? We will consider a variable signficant only if the p-value is below 0.05.
summary(model1)


####  Problem 2 - Understanding the Model

# Which of the following is the simplest correct explanation for this contradiction?
# -> All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set.

# Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)?
cor(climateTrain)


#### Problem 3 - Simplifying the Model

# Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and N2O. Remember to use the training set to build the model.
model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=climateTrain)

# Enter the coefficient of N2O in this reduced model:
# Enter the model R2:
summary(model2)


#### Problem 4 - Automatically Building the Model

# Enter the R2 value of the model produced by the step function:
model3 <- step(model1)
summary(model3)


####  Problem 5 - Testing on Unseen Data 
predictTest <- predict(model3, newdata=climateTest)
SSE <- sum((climateTest$Temp - predictTest)^2)
SST <- sum((climateTest$Temp - mean(climateTrain$Temp))^2)
R_squared <- 1 - SSE/SST
# out-of-sample R squared (this could be negative, when model does worse on test data than on baseline in original set!!)
R_squared


###### Reading Test Scores

# Load the training and testing sets using the read.csv() function, and save them as variables with the names pisaTrain and pisaTest.
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")

# How many students are there in the training set?
str(pisaTrain)

# Using tapply() on pisaTrain, what is the average reading test score of males?
# Of females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean, na.rm=T)
aggregate(pisaTrain$readingScore, by=list(pisaTrain$male), 
          FUN=mean, na.rm=T, simplify=T)

# Which variables are missing data in at least one observation in the training set?
summary(pisaTrain)

# Type the following commands into your R console to remove observations with any missing value from pisaTrain and pisaTest:
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# How many observations are now in the training set?
str(pisaTrain)

# How many observations are now in the testing set?
str(pisaTest)


#### Problem 2.1 - Factor variables

# Which of the following variables is an unordered factor with at least 3 levels?
# -> raceeth

# Which of the following variables is an ordered factor with at least 3 levels?
# -> grade

# Which binary variables will be included in the regression model?
# -> all but raceethWhite

# For a student who is Asian, which binary variables would be set to 0? All remaining variables will be set to 1.
# -> all but raceethAsian

# For a student who is white, which binary variables would be set to 0? All remaining variables will be set to 1.
# -> All


#### Problem 3 - Building a model

# Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Now, build a linear regression model (call it lmScore) using the training set to predict readingScore using all the remaining variables.
lmScore <- lm(readingScore ~ . , data=pisaTrain)

# What is the Multiple R-squared value of lmScore on the training set?
summary(lmScore)

# What is the training-set root-mean squared error (RMSE) of lmScore?
SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisaTrain))
RMSE

# What is the predicted reading score of student A minus the predicted reading score of student B?
summary(lmScore)

# What is the meaning of the coefficient associated with variable raceethAsian?
# -> Predicted difference in the reading score between an Asian student and a white student who is otherwise identical

# Based on the significance codes, which variables are candidates for removal from the model?
summary(lmScore)


#### Problem 4 - Predicting on unseen data

predTest <- predict(lmScore, newdata=pisaTest)

# What is the range between the maximum and minimum predicted reading score on the test set?
summary(predTest)
637.7-353.2

# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE <- sum((predTest - pisaTest$readingScore)^2)
SSE

# What is the root-mean squared error (RMSE) of lmScore on the testing set?
RMSE <- sqrt(SSE/nrow(pisaTest))
RMSE

# What is the predicted test score used in the baseline model?
mean(predict(lmScore, newdata=pisaTrain))

# What is the sum of squared errors of the baseline model on the testing set?
SST <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
SST

# What is the test-set R-squared value of lmScore?
SSE <- sum((predTest - pisaTest$readingScore)^2)
R2 <- 1 - SSE/SST
R2


###### Detecting Flu Epidemics via Search Engine Query Data 

#### Problem 1 - Understanding the Data

FluTrain <- read.csv("FluTrain.csv")
str(FluTrain)

# Which week corresponds to the highest percentage of ILI-related physician visits? Select the day of the month corresponding to the start of this week.
FluTrain$Week[which.max(FluTrain$ILI)]

# Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain$Week[which.max(FluTrain$Queries)]

# Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(FluTrain$ILI, freq=F, xlab="", main="", xlim=, breaks=, col="blue")

# When handling a skewed dependent variable, it is often useful to predict the logarithm of dependent variable instead of the dependent variable itself -- this prevents the small number of unusually large or small observations from having an undue influence on the sum of squared errors of predictive models. In this problem, we will predict the natural log of the ILI variable, which can be computed in R using the log() function.

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?. 
plot(FluTrain$Queries, log(FluTrain$ILI))


#### Problem 2 - Linear Regression Model

# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?
# -> log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

# Let's call the regression model from the previous problem (Problem 2.1) FluTrend1 and run it
FluTrend1 <- lm(log(ILI) ~ Queries, data=FluTrain)

# What is the training set R-squared value for FluTrend1 model?
summary(FluTrend1)

# What is the relationship we infer from our problem?
cor(FluTrain$Queries, log(FluTrain$ILI))^2


#### Problem 3 - Performance on the Test Set

FluTest <- read.csv("FluTest.csv")
PredTest1 <- exp(predict(FluTrend1, newdata=FluTest))

# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
PredTest1
which(FluTest$Week == "2012-03-11 - 2012-03-17")
strftime(as.POSIXlt("2012-03-11"),format="%U")
format(as.POSIXct("2012-03-11"), "%U")

# What is the relative error betweeen the estimate and the observed for that week? Note that the relative error is calculated as 
(FluTest$ILI[FluTest$Week=="2012-03-11 - 2012-03-17"] - PredTest1[11]) / FluTest$ILI[FluTest$Week=="2012-03-11 - 2012-03-17"]

# What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits?
SSE <- sum((PredTest1 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))
RMSE


#### Problem 4 - Training a Time Series Model

# work with zoo library
library(zoo)

# After installing and loading the zoo package, run the following commands to create the ILILag2 variable in the training set:
# In these commands, the value of -2 passed to lag means to return 2 observations before the current one; a positive value would have returned future observations. The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset, where we can't compute the data from 2 weeks earlier.
ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)

# How many values are missing in the new ILILag2 variable?
sum(is.na(FluTrain$ILILag2))

# Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these two variables?
plot(FluTrain$ILI, log(FluTrain$ILILag2))


# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as well as the log of the ILILag2 variable.
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)

# Which coefficients are significant at the p=0.05 level in this regression model?
# What is the R^2 value of the FluTrend2 model?
summary(FluTrend2)

# On the basis of R-squared value and significance of coefficients, which statement is the most accurate?
summary(FluTrend1)
# -> FluTrend2 is a stronger model than FluTrend1 on the training set.


#### Problem 5 - Evaluating the Time Series Model in the Test Set 

# Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame. 
ILILag2 <- lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <- coredata(ILILag2)

# How many missing values are there in this new variable?
sum(is.na(FluTest$ILILag2))

# In this problem, the training and testing sets are split sequentially -- the training set contains all observations from 2004-2011 and the testing set contains all observations from 2012. There is no time gap between the two datasets, meaning the first observation in FluTest was recorded one week after the last observation in FluTrain. From this, we can identify how to fill in the missing values for the ILILag2 variable in FluTest.

# Which value should be used to fill in the ILILag2 variable for the first observation in FluTest?
# -> The ILI value of the second-to-last observation in the FluTrain data frame. 

# Which value should be used to fill in the ILILag2 variable for the second observation in FluTest?
# -> The ILI value of the last observation in the FluTrain data frame. 

# Fill in the missing values for ILILag2 in FluTest.
FluTest$ILILag2[1] <- FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] <- FluTrain$ILI[nrow(FluTrain)]

# What is the new value of the ILILag2 variable in the first row of FluTest?
FluTrain$ILI[nrow(FluTrain)-1]

# What is the new value of the ILILag2 variable in the second row of FluTest?
FluTrain$ILI[nrow(FluTrain)]

# Obtain test set predictions of the ILI variable from the FluTrend2 model, again remembering to call the exp() function on the result of the predict() function to obtain predictions for ILI instead of log(ILI).
PredTest2 <- exp(predict(FluTrend2, newdata=FluTest))

# What is the test-set RMSE of the FluTrend2 model?
SSE_Trend2 <- sum((PredTest2 - FluTest$ILI)^2)
RMSE_Trend2 <- sqrt(SSE_Trend2/nrow(FluTest))
RMSE_Trend2

# Which model obtained the best test-set RMSE?
SSE_Trend1 <- sum((PredTest1 - FluTest$ILI)^2)
RMSE_Trend1 <- sqrt(SSE_Trend1/nrow(FluTest))
RMSE_Trend1


######## Week 3: Logistic Regression

###### Modeling the Expert: An Introduction to Logistic Regression 

#### Video 3: Logistic Regression

#  Quick Question 3
log(exp(-1.5+3*1-0.5*5))
exp(-1.5+3*1-0.5*5)
y <- 1/(1+exp(-(-1.5+3*1-0.5*5)))
y


#### Video 4: Logistic Regression in R

quality <- read.csv("quality.csv")
str(quality)

# poor care = 1 if poor care and 0 if good care
table(quality$PoorCare)

# simple baseline method: predict most frequent outcome for all observations, in this case assuming all patients receive good care
# accuracy level of baseline model computed by actual good cares over assumption of all being good care i.e. all observations
98/131
# -> accuracy level of baseline model is 74.8%; this we try to beat with logistic model

# randomnly split into train and test set
# use package caTools (depends on bitops)
# install.packages("caTools")

# load package
library(caTools)

set.seed(88)
# split function randomly samples but also  makes sure that dependent variable is equally
# represented in train and test set i.e. 75% in our case!!!
split <- sample.split(quality$PoorCare, SplitRatio=0.75)
split
# TRUE = training set

qualityTrain <- subset(quality, split==T)
qualityTest <- subset(quality, split==F)
nrow(qualityTrain)
nrow(qualityTest)

set.seed(88)
# split function randomly samples but also  makes sure that dependent variable is equally
# represented in train and test set i.e. 75% in our case!!!
split <- sample.split(quality$PoorCare, SplitRatio=0.75)
split
# TRUE = training set

qualityTrain <- subset(quality, split==T)
qualityTest <- subset(quality, split==F)
nrow(qualityTrain)
nrow(qualityTest)

# first model
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)
# AIC is measure of quality of model, can only be compared between models based on same data. It is a vital means for model selection

predictTrain <- predict(QualityLog, type="response")
# response to receive probabilities

summary(predictTrain)

# let's check whether we are predicting higher probabilities for the actual poor care cases which we would expect
tapply(predictTrain, qualityTrain$PoorCare, mean)
# we are predicting higher probabilities for the actual poor care cases

# Quick Question 4.1
QualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
# What is the coefficient for "StartedOnCombination"?
summary(QualityLog2)

# threshold value of t = 0.5 is neither preferring good or bad care classification
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
# Model with higher threshold (> 0.5) will have a lower sensitivity and higher specificity
# Model with lower threshold (> 0.5) will have a higher sensitivity and lower specificity

# compute confusion matrices in R with different threshold value t
# table(True Outcome, predictions > threshold)
table(qualityTrain$PoorCare, predictTrain > 0.5)
# this will return true if prediction is greater 0.5 i.e. we predict poor care, and false otherwise
# for 70 cases we predict good care and they actually get good care = True Negative
# for 10 cases we predict poor care and they actually get poor care = True Positive
# for 4 cases we predict poor care and they actually get good care = False Positives
# for 15 cases we predict good care and they actually get poor care = False Negatives

# sensitivity
10/25

# Specificity
70/74

# increase threshold
table(qualityTrain$PoorCare, predictTrain > 0.7)

# sensitivity
8/25

# Specificity
73/74

# -> Increasing threshold leads to lower sensitivity and higher specificity

# decrease threshold
table(qualityTrain$PoorCare, predictTrain > 0.2)

# sensitivity
16/25

# Specificity
54/74

# -> Decreasing threshold leads to higher sensitivity and lower specificity

# Quick Question 5.1: This question asks about the following two confusion matrices:
TruePositivesM1 <- 20
FalseNegativesM1 <- 5
TrueNegativesM1 <- 15
FalsePositivesM1 <- 10

TruePositivesM2 <- 15
FalseNegativesM2 <- 10
TrueNegativesM2 <- 20
FalsePositivesM2 <- 5

# What is the sensitivity of Confusion Matrix #1?
TruePositivesM1 / (TruePositivesM1 + FalseNegativesM1)
# What is the specificity of Confusion Matrix #1?
TrueNegativesM1 / (TrueNegativesM1 + FalsePositivesM1)

# To go from Confusion Matrix #1 to Confusion Matrix #2, did we increase or decrease the threshold value?
# Check if sensitivity decreased, if so threshold was increased
(TruePositivesM1 / (TruePositivesM1 + FalseNegativesM1)) > (TruePositivesM2 / (TruePositivesM2 + FalseNegativesM2))


#### Video 6: ROC Curves
# Instal ROCR package: http://cran.r-project.org/web/packages/ROCR/index.html
# depends on gplots: http://cran.r-project.org/web/packages/gplots/index.html
library(ROCR)

ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
# Receiver Operator Characteristic Curve
plot(ROCRperf)

# add colors
plot(ROCRperf, colorize=T)

# add threshold labels
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Given this ROC curve, which threshold would you pick if you wanted to correctly identify a small group of patients who are receiving the worst care with high confidence?
# -> 0.7

# Which threshold would you pick if you wanted to correctly identify half of the patients receiving poor care, while making as few errors as possible?
# -> 0.3


#### Video 7: Interpreting the Model

# Outcome Measures
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
# Overall Error Rate = (False Positives + False Negatives) / N -> terms off the diagonal of confusion matrix divided by N
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
# False Negative Error Rate = False Negatives / (True Positives + False Negatives)
# False Positive Error Rate = False Positives / (True Negatives + False Positives)

# Quick Question 7
# Compute the test set predictions in R by running the command:
predictTest <- predict(QualityLog, type="response", newdata=qualityTest)
# You can compute the test set AUC by running the following two commands in R:
ROCRpredTest <- prediction(predictTest, qualityTest$PoorCare)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# The AUC of a model has the following nice interpretation: given a random patient who actually received poor care, and a random patient who actually received good care, the AUC is the perecentage of time that our model will classify which is which correctly. 


###### The Framingham Heart Study: Evaluating Risk Factors to Save Lives

#### Video 3: A Logistic Regression Model

framingham <- read.csv("framingham.csv")
str(framingham)

# load caTools library
library(caTools)

# set seed and create split
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio=0.65)
train <- subset(framingham, split==T)
test <- subset(framingham, split==F)

# build logistic model
# use all independent variables
framinghamLog <- glm(TenYearCHD ~ ., data=train, family=binomial)
summary(framinghamLog)

# make prediction
predictTest <- predict(framinghamLog, type="response", newdata=test)

# confusion matrix
table(test$TenYearCHD, predictTest > 0.5)
# with threshold of 0.5 we predict outcome of 1 in TRUE column (True Positive) very rarely
# This means that model rarely predicts a 10 year CHD rest above 50%

# What is model accuracy
(1069+11) / (1069+6+187+11)

# compare to accuracy of simple baseline model
# more frequent outcome is zero so baseline model would always predict 0
# total number of true negatives / N
(1069+6) / (1069+6+187+11)
# Our model barely beats baseline in terms of accuracy

# Do we still have vulauable model?
# Compute out-of-sample AUC
library(ROCR)
ROCRpred <- prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
# -> model can distinguish between low and high risk patient in 74% of cases -> pretty good

# Quick Question 3
# What is the sensitivity of our logistic regression model on the test set, using a threshold of 0.5? 
TruePositives <- 11
FalseNegatives <- 187
TrueNegatives <- 1069
FalsePositives <- 6
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
TruePositives / (TruePositives + FalseNegatives)

# What is the specificity of our logistic regression model on the test set, using a threshold of 0.5?
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
TrueNegatives / (TrueNegatives + FalsePositives)


###### Recitation: Election Forecasting: Predicting the Winner Before any Votes are Cast 
# for cross check PollingData_Imputed.csv


#### Video 2: Dealing with Missing Data

polling <- read.csv("PollingData.csv")
str(polling)

# breakdown of year variable
table(polling$Year)

# check missing variables
summary(polling)

# Deal with missing data:
# - delete missing obs
# - delete variables with missing values
# - Fill missing data points with average

# Package: Multiple Imputation by Chained Equations (mice)
# Imports: 	lattice, MASS, nnet, randomForest, rpart
library(mice)

# limit data frame to only 4 polling related variables
simple <- polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

set.seed(144)
imputed <- complete(mice(simple))
# output shows 5 rounds of imputation have been run and now all values have been filled i.e. no NAs
summary(imputed)

# copy imputed variables back into original dataframe
polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)


#### Video 3: A Sophisticated Baseline Method

# split data into train and test
Train <- subset(polling, Year==2004 | Year==2008)
Test <- subset(polling, Year==2012)

# breakdown of dependent variable in training set
table(Train$Republican)
# In 47 observations democrat won but in 53 Republican won
# baseline prediciton is always the more frequent so Republican i.e. 1
# A smarter baseline model would be to take one of the polls (here Rasmussen) and
# make a prediction based on what the poll said will win in this state
# So if Republican was ahead in polls we also assume that for baseline and vice versa, if they were tied, model would not know what to select

# sign function returns 1 if positive number and -1 if negative number and zero for 0
# So if we give Rasmussen variable into sign, whenever Republican would win i.e. Rasmussen = positive number, sign returns 1
table(sign(Train$Rasmussen))
# in 56 of 100 training obeservations smart baseline predicted Republican to win

# What we really want to see is the breakdown of how the smart baseline model compares to actual result
table(Train$Republican, sign(Train$Rasmussen))
# in this table rows are true outcome i.e. actual outcome (1=Republican) and columns are smart baseline prediction (as in confusion matrix)
# top left says 42 obs where Rasmussen smart baseline predicted democrat to win and this happened = True negative
# lower right says 52 obs where Rasmussen smart baseline predicted republican to win and this happened = True positive
# upper right says 4 obs where Rasmussen smart baseline predicted republican to win and this did not happen = False positive
# lower left says 0 obs where Rasmussen smart baseline predicted democrat to win and this did not happen = False negative


#### Video 4: Logistic Regression Models

# consider there is multicollinearity because they all measure the same thing
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# if we built one-variable model, we should use the one with highest correlation to dependent variable Republican
mod1 <- glm(Republican ~ PropR, data=Train, family="binomial")
summary(mod1)

# prediction (using train set)
pred1 <- predict(mod1, type="response")

# use 0.5 threshold to see how prediction does
table(Train$Republican, pred1 >= 0.5)
# model makes 4 mistakes (off-diagonal of confusion matrix) which is about the same as smart baseline

# try improving model by adding variable by looking in correlation matrix for pair with low correlation because they might work together
# try pair Rasmussen/DiffCount and SurveyUSA/DiffCount
mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data=Train, family="binomial")
summary(mod2)
# AIC has smaller value than smart baseline and model 1

# prediction (using train set)
pred2 <- predict(mod2, type="response")

# use 0.5 threshold to see how prediction does
table(Train$Republican, pred2 >= 0.5)
# model makes 3 mistakes (off-diagonal of confusion matrix) which is slightly better than smart baseline


#### Video 5: Test Set Predictions

# check smart baseline on test set
table(Test$Republican, sign(Test$Rasmussen))
# four mistakes and 2 inconclusive

# use our model on test
TestPrediction <- predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5)
# model makes one mistake

# pull out the mistake we made and try understanding it
subset(Test, TestPrediction >= 0.5 & Republican==0)


###### Week 3 Assignment 

##### Popularity of music records

#### Problem 1.1 - Understanding the Data

songs <- read.csv("songs.csv")
str(songs)

# How many observations (songs) are from the year 2010?
length(songs$year[songs$year==2010])

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
length(songs$year[songs$artistname=="Michael Jackson"])

# Which of these songs by Michael Jackson made it to the Top 10?
songs$songtitle[songs$artistname=="Michael Jackson" & songs$Top10 ==1]

# What are the values of timesignature that occur in our dataset? Select all that apply.
unique(songs$timesignature)

# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)

# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
songs$songtitle[which.max(songs$tempo)]


#### Problem 2 - Creating Our Prediction Model

# use the subset function to split the data into a training set "SongsTrain" consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases

SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year > 2009)

# How many observations (songs) are in the training set?
str(SongsTrain)

# In this problem, our outcome variable is "Top10" - we are trying to predict whether or not a song will make it to the Top 10 of the Billboard Hot 100 Chart. Since the outcome variable is binary, we will build a logistic regression model. We'll start by using all song attributes as our independent variables, which we'll call Model 1.
# We will only use the variables in our dataset that describe the numerical attributes of the song in our logistic regression model. So we won't use the variables "year", "songtitle", "artistname", "songID" or "artistID".
# We have seen in the lecture that, to build the logistic regression model, we would normally explicitly input the formula including all the independent variables in R. However, in this case, this is a tedious amount of work since we have a large number of independent variables.
# However, in our case, we want to exclude some of the variables in our dataset from being used as independent variables ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. First define a vector of variables names called nonvars - these are the variables that we won't use in our model.
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
# To remove these variables from your training and testing sets, type the following commands in your R console:
SongsTrain <- SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest <- SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# Now, use the glm function to build a logistic regression model to predict Top10 using all of the other variables as the independent variables. You should use SongsTrain to build the model. 
SongsLog1 <- glm(Top10 ~ ., data=SongsTrain, family=binomial)

# Looking at the summary of your model, what is the value of the Akaike Information Criterion (AIC)?
summary(SongsLog1)
# -> The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10 

# In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. What does Model 1 suggest in terms of complexity?
# -> Mainstream listeners tend to prefer less complex songs

# By inspecting the coefficient of the variable "loudness", what does Model 1 suggest?
# -> Mainstream listeners prefer songs with heavy instrumentation

# By inspecting the coefficient of the variable "energy", do we draw the same conclusions as above?
# -> No


####  Problem 3.1 - Beware of Multicollinearity Issues!

# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness, SongsTrain$energy)
cor(SongsTrain)

# Given that these two variables are highly correlated, Model 1 suffers from multicollinearity. To avoid this issue, we will omit one of these two variables and rerun the logistic regression. In the rest of this problem, we'll build two variations of our original model: Model 2, in which we keep "energy" and omit "loudness", and Model 3, in which we keep "loudness" and omit "energy".

# Create Model 2, which is Model 1 without the independent variable "loudness". This can be done with the following command:
SongsLog2 <- glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
# We just subtracted the variable loudness. We couldn't do this with the variables "songtitle" and "artistname", because they are not numeric variables, and we might get different values in the test set that the training set has never seen. But this approach will work when you want to remove variables that you could feasibly use in your model.

# Look at the summary of SongsLog2, and inspect the coefficient of the variable "energy". What do you observe?
summary(SongsLog2)
# -> Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1

# Now, create Model 3, which should be exactly like Model 1, but without the variable "energy". 
SongsLog3 <- glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

# Look at the summary of Model 3 and inspect the coefficient of the variable "loudness". Remembering that higher loudness and energy both occur in songs with heavier instrumentation, do we make the same observation about the popularity of heavy instrumentation as we did with Model 2?
summary(SongsLog1)
summary(SongsLog2)
summary(SongsLog3)
# -> Yes

# In the remainder of this problem, we'll just use Model 3.


####  Problem 4 - Validating Our Model 

# Make predictions on the test set using Model 3. 
TestPrediction <- predict(SongsLog3, newdata=SongsTest, type="response")

# What is the accuracy of Model 3 on the test set, using a threshold of 0.45? (Compute the accuracy as a number between 0 and 1.)
table(SongsTest$Top10, TestPrediction >= 0.45)
(309+19) / (309+5+40+19)

# Let's check if there's any incremental benefit in using Model 3 instead of a baseline model. Given the difficulty of guessing which song is going to be a hit, an easier model would be to pick the most frequent outcome (a song is not a Top 10 hit) for all songs. 
# -> accuracy level of baseline model computed by actual non top 10 hits over assumption of all being non top 10 i.e. all observations
table(SongsTest$Top10)

# What would be the fraction of accuracy using this baseline model for the test set?
314 / (314+59)

# How many songs does Model 3 correctly predict as Top 10 hits in 2010, using a threshold of 0.45?
table(SongsTest$Top10, TestPrediction >= 0.45)

# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
TruePositives <- 19
FalseNegatives <- 40
TrueNegatives <- 309
FalsePositives <- 5
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
TruePositives / (TruePositives + FalseNegatives)

# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
TrueNegatives / (TrueNegatives + FalsePositives)

# What conclusions can you make about our model (select all that apply)?
# -> Model 3 favors specificity over sensitivity.
# -> Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely. So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 10 hits. 


##### Predicting the baseball world series champion

baseball <- read.csv("baseball.csv")

#    Team: A code for the name of the team
#    League: The Major League Baseball league the team belongs to, either AL (American League) or NL (National League)
#    Year: The year of the corresponding record
#    RS: The number of runs scored by the team in that year
#    RA: The number of runs allowed by the team in that year
#    W: The number of regular season wins by the team in that year
#    OBP: The on-base percentage of the team in that year
#    SLG: The slugging percentage of the team in that year
#    BA: The batting average of the team in that year
#    Playoffs: Whether the team made the playoffs in that year (1 for yes, 0 for no)
#    RankSeason: Among the playoff teams in that year, the ranking of their regular season records (1 is best)
#    RankPlayoffs: Among the playoff teams in that year, how well they fared in the playoffs. The team winning the World Series gets a RankPlayoffs of 1.
#    G: The number of games a team played in that year
#    OOBP: The team's opponents' on-base percentage in that year
#    OSLG: The team's opponents' slugging percentage in that year

# Each row in the baseball dataset represents a team in a particular year.


#### Problem 1 - Limiting to Teams Making the Playoffs

# How many team/year pairs are there in the whole dataset?
str(baseball)

# Using the table() function, identify the total number of years included in this dataset.
length(unique(baseball$Year))

# Because we're only analyzing teams that made the playoffs, use the subset() function to replace baseball with a data frame limited to teams that made the playoffs (so your subsetted data frame should still be called "baseball").
baseball <- subset(baseball, Playoffs==1)

# How many team/year pairs are included in the new dataset?
length(baseball)

# Which of the following has been the number of teams making the playoffs in some season?
length(unique(baseball$Team))

# Through the years, different numbers of teams have been invited to the playoffs. Which of the following has been the number of teams making the playoffs in some season?
unique(table(baseball$Year))
table(table(baseball$Year))


#### Problem 2.1 - Adding an Important Predictor

# It's much harder to win the World Series if there are 10 teams competing for the championship versus just two. Therefore, we will add the predictor variable NumCompetitors to the baseball data frame. NumCompetitors will contain the number of total teams making the playoffs in the year of a particular team/year pair. For instance, NumCompetitors should be 2 for the 1962 New York Yankees, but it should be 8 for the 1998 Boston Red Sox.

# We start by storing the output of the table() function that counts the number of playoff teams from each year:

PlayoffTable <- table(baseball$Year)
PlayoffTable

# We will use this stored table to look up the number of teams in the playoffs in the year of each team/year pair.

# Just as we can use the names() function to get the names of a data frame's columns, we can use it to get the names of the entries in a table. 
# What best describes the output of names(PlayoffTable)?
names(PlayoffTable)
# -> Vector of years stored as strings (type chr) 

# Given a vector of names, the table will return a vector of frequencies. 
# Which function call returns the number of playoff teams in 1990 and 2001?
PlayoffTable[c("1990","2001")]

# Putting it all together, we want to look up the number of teams in the playoffs for each team/year pair in the dataset, and store it as a new variable named NumCompetitors in the baseball data frame. 
# Which of the following function calls accomplishes this?
PlayoffTable[as.character(baseball$Year)]

# Add the NumCompetitors variable to your baseball data frame.
baseball$NumCompetitors <- PlayoffTable[as.character(baseball$Year)]

# How many playoff team/year pairs are there in our dataset from years where 8 teams were invited to the playoffs?
length(baseball$NumCompetitors[baseball$NumCompetitors == 8])


#### Problem 3.1 - Bivariate Models for Predicting World Series Winner

# In this problem, we seek to predict whether a team won the World Series; in our dataset this is denoted with a RankPlayoffs value of 1. Add a variable named WorldSeries to the baseball data frame, by typing the following command in your R console:
baseball$WorldSeries <- as.numeric(baseball$RankPlayoffs == 1)

# WorldSeries takes value 1 if a team won the World Series in the indicated year and a 0 otherwise. 
# How many observations do we have in our dataset where a team did NOT win the World Series?
length(baseball$WorldSeries[baseball$WorldSeries != 1])
length(baseball$WorldSeries[baseball$WorldSeries != T])
length(baseball$WorldSeries[baseball$WorldSeries != TRUE])

# When we're not sure which of our variables are useful in predicting a particular outcome, it's often helpful to build bivariate models, which are models that predict the outcome using a single independent variable. 
# Which of the following variables is a significant predictor of the WorldSeries variable in a bivariate logistic regression model? To determine significance, remember to look at the stars in the summary output of the model.
# Note that you have to build 12 models to answer this question! Use the entire dataset baseball to build the models.
baseballLog1 <- glm(WorldSeries ~ Year, data=baseball, family=binomial)
baseballLog2 <- glm(WorldSeries ~ RS, data=baseball, family=binomial)
baseballLog3 <- glm(WorldSeries ~ RA, data=baseball, family=binomial)
baseballLog4 <- glm(WorldSeries ~ W, data=baseball, family=binomial)
baseballLog5 <- glm(WorldSeries ~ OBP, data=baseball, family=binomial)
baseballLog6 <- glm(WorldSeries ~ SLG, data=baseball, family=binomial)
baseballLog7 <- glm(WorldSeries ~ BA, data=baseball, family=binomial)
baseballLog8 <- glm(WorldSeries ~ RankSeason, data=baseball, family=binomial)
baseballLog9 <- glm(WorldSeries ~ OOBP, data=baseball, family=binomial)
baseballLog10 <- glm(WorldSeries ~ OSLG, data=baseball, family=binomial)
baseballLog11 <- glm(WorldSeries ~ NumCompetitors, data=baseball, family=binomial)
baseballLog12 <- glm(WorldSeries ~ League, data=baseball, family=binomial)
summary(baseballLog1)
summary(baseballLog2)
summary(baseballLog3)
summary(baseballLog4)
summary(baseballLog5)
summary(baseballLog6)
summary(baseballLog7)
summary(baseballLog8)
summary(baseballLog9)
summary(baseballLog10)
summary(baseballLog11)
summary(baseballLog12)


#### Problem 4 - Multivariate Models for Predicting World Series Winner

# In this section, we'll consider multivariate models that combine the variables we found to be significant in bivariate models. Build a model using all of the variables that you found to be significant in the bivariate models. 
baseballLog13 <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family="binomial")

# How many variables are significant in the combined model?
summary(baseballLog13)

# Often, variables that were significant in bivariate models are no longer significant in multivariate analysis due to correlation between the variables. 
# Which of the following variable pairs have a high degree of correlation (a correlation greater than 0.8 or less than -0.8)?
cor(baseball[sapply(baseball, is.numeric)])
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

# Build all six of the two variable models listed in the previous problem. Together with the four bivariate models, you should have 10 different logistic regression models.
baseballLog1 <- glm(WorldSeries ~ Year, data=baseball, family=binomial)
baseballLog3 <- glm(WorldSeries ~ RA, data=baseball, family=binomial)
baseballLog8 <- glm(WorldSeries ~ RankSeason, data=baseball, family=binomial)
baseballLog11 <- glm(WorldSeries ~ NumCompetitors, data=baseball, family=binomial)
baseballLog14 <- glm(WorldSeries ~ Year + RA, data=baseball, family="binomial")
baseballLog15 <- glm(WorldSeries ~ Year + RankSeason, data=baseball, family="binomial")
baseballLog16 <- glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family="binomial")
baseballLog17 <- glm(WorldSeries ~ RA + RankSeason, data=baseball, family="binomial")
baseballLog18 <- glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family="binomial")
baseballLog19 <- glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family="binomial")

# Which model has the best AIC value (the minimum AIC value)?
summary(baseballLog1)
summary(baseballLog3)
summary(baseballLog8)
summary(baseballLog11)
summary(baseballLog14)
summary(baseballLog15)
summary(baseballLog16)
summary(baseballLog17)
summary(baseballLog18)
summary(baseballLog19)
# -> NumCompetitors -> summary(baseballLog11)


##### Predicting parole violators

parole <- read.csv("parole.csv")

#    male: 1 if the parolee is male, 0 if female
#    race: 1 if the parolee is white, 2 otherwise
#    age: the parolee's age in years at release from prison
#    state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
#    time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
#    max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
#    multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
#    crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
#    violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.


#### Problem 1 - Loading the Dataset

# How many parolees are contained in the dataset?
str(parole)

# How many of the parolees in the dataset violated the terms of their parole?
length(parole$violator[parole$violator == 1])

# Which variables in this dataset are unordered factors with at least three levels?
# -> state, crime


#### Problem 2 - Preparing the Dataset

# In the last subproblem, we identified variables that are unordered factors with at least 3 levels, so we need to convert them to factors for our prediction problem (we introduced this idea in the "Reading Test Scores" problem last week). Using the as.factor() function, convert these variables to factors. Keep in mind that we are not changing the values, just the way R understands them (the values are still numbers). 
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

# How does the output of summary() change for a factor variable as compared to a numerical variable?
summary(parole)
# -> The output becomes similar to that of the table() function applied to that variable

# Why are we taking this step of preparing the variables before splitting the data into a training and testing set?
# -> Preparing the data before splitting the dataset saves work: we only need to do these steps once instead of twice


#### Problem 3 - Splitting into a Training and Testing Set

# To ensure consistent training/testing set splits, run the following 5 lines of code (do not include the line numbers at the beginning):
set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)

# Roughly what proportion of parolees have been allocated to the training and testing sets?
# -> 70% train

# Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?
# -> The exact same training/testing set split as the first execution of [1]-[5]

# If you instead ONLY re-ran lines [3]-[5], what would you expect?
# -> A different training/testing set split from the first execution of [1]-[5]

# If you instead called set.seed() with a different number and then re-ran lines [3]-[5] of Problem 3.1, what would you expect?
# -> A different training/testing set split from the first execution of [1]-[5] 


#### Problem 4 - Building a Logistic Regression Model

# Using glm (and remembering the parameter family="binomial"), train a logistic regression model on the training set. Your dependent variable is "violator", and you should use all of the other variables as independent variables.
paroleLog1 <- glm(violator ~ ., data = train, family="binomial")

# What variables are significant in this model?
summary(paroleLog1)

# What can we say based on the coefficient of the multiple.offenses variable?
# The following two properties might be useful to you when answering this question:
# 1) If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c for a unit increase in the variable.
# 2) If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit increase in the variable.
# -> Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical. 

# Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny. Answer the following questions based on the model's predictions for this individual. (HINT: You should use the coefficients of your model, the Logistic Response Function, and the Odds equation to solve this problem.)
# According to the model, what are the odds this individual is a violator?
# According to the model, what is the probability this individual is a violator?
# -> From the logistic regression equation, we have log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4. This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0. We conclude that log(odds) = -1.700629.
# -> Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted probability of violation is 1/(1+exp(1.700629)) = 0.154. 


#### Problem 5 - Evaluating the Model on the Testing Set

# Use the predict() function to obtain the model's predicted probabilities for parolees in the testing set, remembering to pass type="response".
predictTest <- predict(paroleLog1, newdata=test, type="response")
max(predictTest)

# In the following questions, evaluate the model's predictions on the test set using a threshold of 0.5.
table(test$violator, predictTest >= 0.5)
TruePositives <- 12
FalseNegatives <- 11
TrueNegatives <- 167
FalsePositives <- 12

# What is the model's sensitivity?
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
TruePositives / (TruePositives + FalseNegatives)

# What is the model's specificity?
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
TrueNegatives / (TrueNegatives + FalsePositives)

# What is the model's accuracy?
(TruePositives + TrueNegatives) / (TruePositives + TrueNegatives + FalsePositives + FalseNegatives)

# What is the accuracy of a simple model that predicts that every parolee is a non-violator? 
(TrueNegatives + FalseNegatives) / (TruePositives + TrueNegatives + FalsePositives + FalseNegatives)

# Consider a parole board using the model to predict whether parolees will be violators or not. The job of a parole board is to make sure that a prisoner is ready to be released into free society, and therefore parole boards tend to be particularily concerned with releasing prisoners who will violate their parole. Which of the following most likely describes their preferences and best course of action?
# threshold value of t = 0.5 is neither preferring good or bad care classification
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
# Model with higher threshold (> 0.5) will have a lower sensitivity and higher specificity
# Model with lower threshold (> 0.5) will have a higher sensitivity and lower specificity
# -> The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.

# Which of the following is the most accurate assessment of the value of the logistic regression model with a cutoff 0.5 to a parole board, based on the model's accuracy as compared to the simple baseline model?
# -> The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value.
# Explanation: The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model has 0 false positives and 23 false negatives. Because a parole board is likely to assign more cost to a false negative, the model at cutoff 0.5 is likely of value to the board.
# Explanation: From the previous question, the parole board would likely benefit from decreasing the logistic regression cutoffs, which decreases the false negative rate while increasing the false positive rate.
table(test$violator, predictTest >= 0.5)

# Using the ROCR package, what is the AUC value for the model?
ROCRpredTest <- prediction(predictTest, test$violator)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# Describe the meaning of AUC in this context.
# -> The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator. 
# Explanation: The AUC deals with differentiating between a randomly selected positive and negative example. It is independent of the regression cutoff selected.


####  Problem 6 - Identifying Bias in Observational Data 

# How could we improve our dataset to best address selection bias?
# -> We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term. 


##### Predicting loan repayment

loans <- read.csv("loans.csv")

#    credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
#    purpose: The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
#    int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
#    installment: The monthly installments ($) owed by the borrower if the loan is funded.
#    log.annual.inc: The natural log of the self-reported annual income of the borrower.
#    dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
#    fico: The FICO credit score of the borrower.
#    days.with.cr.line: The number of days the borrower has had a credit line.
#    revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
#    revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
#    inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
#    delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
#    pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).
#    "not_fully_paid" indicates that the loan was not paid back in full

#### Problem 1 - Preparing the Dataset

str(loans)

# What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
length(loans$not.fully.paid[loans$not.fully.paid == 1]) / length(loans$not.fully.paid)

# Which of the following variables has at least one missing observation?
summary(loans)

# Which of the following is the best reason to fill in the missing values for these variables instead of removing observations with missing data?
str(loans[complete.cases(loans),])
summary(loans[complete.cases(loans),])
# -> We want to be able to predict risk for all borrowers, instead of just the ones with all data reported. 

#install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed

# compare with given set
temp <- read.csv("loans_imputed.csv")
summary(loans[vars.for.imputation])
summary(temp)
loans[vars.for.imputation] <- temp

# What best describes the process we just used to handle missing values?
# -> We predicted missing variable values using the available independent variables for each observation.
# Explanation: Imputation predicts missing variable values for a given observation using the variable values that are reported. We called the imputation on a data frame with the dependent variable not.fully.paid removed, so we predicted the missing values using only other independent variables. 


#### Problem 2 - Prediction Models

# set the random seed to 144 (even though you already did so earlier in the problem) and use the sample.split function to select the 70% of observations for the training set (the dependent variable for sample.split is not.fully.paid)
#install.packages("caTools")
library(caTools)
split <- sample.split(loans$not.fully.paid, SplitRatio=0.7)
# TRUE = training set

Train <- subset(loans, split==T)
Test <- subset(loans, split==F)
nrow(Train)
nrow(Test)

# Now, use logistic regression trained on the training set to predict the dependent variable not.fully.paid using all the independent variables.
loansLog1 <- glm(not.fully.paid ~ ., data = Train)

# Which independent variables are significant in our model? (Significant variables have at least one star, or a Pr(>|z|) value less than 0.05.)
summary(loansLog1)

# Consider two loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700 while the borrower in Application B has FICO credit score 710. 
# Let Logit(A) be the log odds of loan A not being paid back in full, according to our logistic regression model, and define Logit(B) similarly for loan B. What is the value of Logit(A) - Logit(B)?
# 0.09317
# Explanation: Because Application A is identical to Application B other than having a FICO score 10 lower, its predicted log odds differ by -0.009317 * -10 = 0.09317 from the predicted log odds of Application B. 

# Now, let O(A) be the odds of loan A not being paid back in full, according to our logistic regression model, and define O(B) similarly for loan B. What is the value of O(A)/O(B)? (HINT: Use the mathematical rule that exp(A + B + C) = exp(A)*exp(B)*exp(C). Also, remember that exp() is the exponential function in R.)
# 1.0976
# Explanation: Using the answer from the previous question, the predicted odds of loan A not being paid back in full are exp(0.09317) = 1.0976 times larger than the predicted odds for loan B. Intuitively, it makes sense that loan A should have higher odds of non-payment than loan B, since the borrower has a worse credit score. 

# Predict the probability of the test set loans not being paid back in full (remember type="response" for the predict function). Store these predicted probabilities in a variable named predicted.risk and add it to your test set (we will use this variable in later parts of the problem). Compute the confusion matrix using a threshold of 0.5.
predicted.risk <- predict(loansLog1, newdata=Test, type="response")
table(Test$not.fully.paid, predicted.risk >= 0.5)
TruePositives <- 1
FalseNegatives <- 459
TrueNegatives <- 2410
FalsePositives <- 3


# pick the most frequent outcome for all baseline predicitons. 
# -> accuracy level of baseline model computed by actual fully paid over assumption of all being fully paid i.e. all observations
table(Test$not.fully.paid)

# What is the accuracy of the logistic regression model?
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(Test)

# What is the accuracy of the baseline model?
2413 / (2413+460)

# Use the ROCR package to compute the test set AUC
#install.packages("ROCR")
library(ROCR)
ROCRpredTest <- prediction(predicted.risk, Test$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


#### Problem 3 - A "Smart Baseline"

# In the previous problem, we built a logistic regression model that has an AUC significantly higher than the AUC of 0.5 that would be obtained by randomly ordering observations.

# However, LendingClub.com assigns the interest rate to a loan based on their estimate of that loan's risk. This variable, int.rate, is an independent variable in our dataset. In this part, we will investigate using the loan's interest rate as a "smart baseline" to order the loans according to risk.

# Using the training set, build a bivariate logistic regression model (aka a logistic regression model with a single independent variable) that predicts the dependent variable not.fully.paid using only the variable int.rate.

loansLog2 <- glm(not.fully.paid ~ int.rate, data = Train)

# The variable int.rate is highly significant in the bivariate model, but it is not significant at the 0.05 level in the model trained with all the independent variables. What is the most likely explanation for this difference?
summary(loansLog2)
# -> int.rate is correlated with other risk-related variables, and therefore does not incrementally improve the model when those other variables are included. 

# Make test set predictions for the bivariate model. What is the highest predicted probability of a loan not being paid in full on the testing set?
predictTest2 <- predict(loansLog2, newdata=Test, type="response")
max(predictTest2)

# With a logistic regression cutoff of 0.5, how many loans would be predicted as not being paid in full on the testing set?
table(Test$not.fully.paid, predictTest2 >= 0.5)

# What is the test set AUC of the bivariate model?
ROCRpredTest <- prediction(predictTest2, Test$not.fully.paid)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


#### Problem 4 - Computing the Profitability of an Investment

# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?
10*exp(0.06*3)

# What is the profit to the investor if the investment is paid back in full?
# -> c * exp(rt) - c

# Now, consider the case where the investor made a $c investment, but it was not paid back in full. Assume, conservatively, that no money was received from the borrower (often a lender will receive some but not all of the value of the loan, making this a pessimistic assumption of how much is received). What is the profit to the investor in this scenario?
# -> -c


#### Problem 5.1 - A Simple Investment Strategy

# In order to evaluate the quality of an investment strategy, we need to compute this profit for each loan in the test set. For this variable, we will assume a $1 investment (aka c=1). To create the variable, we first assign to the profit for a fully paid loan, exp(rt)-1, to every observation, and we then replace this value with -1 in the cases where the loan was not paid in full. All the loans in our dataset are 3-year loans, meaning t=3 in our calculations. Enter the following commands in your R console to create this new variable:
     
Test$profit <- exp(Test$int.rate*3) - 1

Test$profit[Test$not.fully.paid == 1] <- -1

# What is the maximum profit of a $10 investment in any loan in the testing set (do not include the $ sign in your answer)?
10 * Test$profit[which.max(Test$profit)]


#### Problem 6.1 - An Investment Strategy Based on Risk

# To meet this objective, we will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. We will model an investor who invests $1 in each of the most promising 100 loans.

# First, use the subset() function to build a data frame called highInterest consisting of the test set loans with an interest rate of at least 15%.
highInterest <- subset(Test, Test$int.rate >= 0.15)
str(highInterest)

# What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
summary(highInterest$profit)
# -> different answer: $0.2251

# What proportion of the high-interest loans were not paid back in full?
length(highInterest$not.fully.paid[highInterest$not.fully.paid == 1]) / length(highInterest$not.fully.paid)
# -> different answer: 0.2517

# Next, we will determine the 100th smallest predicted probability of not paying in full by sorting the predicted risks in increasing order and selecting the 100th element of this sorted list. Find the highest predicted risk that we will include by typing the following command into your R console:

cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]

# Use the subset() function to build a data frame called selectedLoans consisting of the high-interest loans with predicted risk not exceeding the cutoff we just computed. Check to make sure you have selected 100 loans for investment.
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)

# What is the profit of the investor, who invested $1 in each of these 100 loans (do not include the $ sign in your answer)?
sum(selectedLoans$profit)
# -> ???

# How many of 100 selected loans were not paid back in full?
table(selectedLoans$not.fully.paid) 
# -> ???



###### Week 4: Trees

##### Judge, Jury, and Classifier: An Introduction to Trees

#### Video 4: CART in R

stevens <- read.csv("stevens.csv")
str(stevens)

library(caTools)

set.seed(3000)

split <- sample.split(stevens$Reverse, SplitRatio = 0.7)
Train <- subset(stevens, split == T)
Test <- subset(stevens, split == F)

# Rpart package
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# build model
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                       Unconst, data=Train, method="class", 
                     control=rpart.control(minbucket=25))

prp(StevensTree)

# Prediction
PredictCART <- predict(StevensTree, newdata=Test, type="class")
# threshold of 0.5

table(Test$Reverse, PredictCART)
# accuracy = (41+71)/(41+36+22+71)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)
PredictROC <- predict(StevensTree, newdata=Test)
PredictROC
# interpret column 2 as probability that this test set observation has outcome 1
# we use second column when thresholding

pred <- prediction(PredictROC[,2], Test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# Quick Question 4
# What is the AUC?
as.numeric(performance(pred, "auc")@y.values)

# First build a CART model that is similar to the one we built in Video 4, except change the minbucket parameter to 5. Plot the tree.
StevensTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                       Unconst, data=Train, method="class", 
                     control=rpart.control(minbucket=5))

prp(StevensTree2)

# How many splits does the tree have?
# -> 16

# Now build a CART model that is similar to the one we built in Video 4, except change the minbucket parameter to 100. Plot the tree.
# How many splits does the tree have?
StevensTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
                        Unconst, data=Train, method="class", 
                      control=rpart.control(minbucket=100))

prp(StevensTree3)
# -> 1


#### Video 5: Random Forests

#install.packages("randomForest")
library(randomForest)

# build model
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner +
                                Respondent + LowerCourt + Unconst,
                              data=Train, nodesize=25, ntree=200)

# since we want classification problem, we have to convert result into factor variable
Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)

# build model again
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner +
                                Respondent + LowerCourt + Unconst,
                              data=Train, nodesize=25, ntree=200)

# make prediction
PredictForest <- predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
# Accuracy (subject to randomness) = (40+74) / (40+37+19+74)
(40+74) / (40+37+19+74)
# random forest improved accuracy a little above CART

# Quick Question 5
set.seed(100)
# build model
StevensForest2 <- randomForest(Reverse ~ Circuit + Issue + Petitioner +
                                Respondent + LowerCourt + Unconst,
                              data=Train, nodesize=25, ntree=200)

# make prediction
PredictForest2 <- predict(StevensForest2, newdata=Test)
table(Test$Reverse, PredictForest2)
# Accuracy (subject to randomness) = (40+74) / (40+37+19+74)
(43+74) / (43+34+19+74)
set.seed(200)
# build model
StevensForest3 <- randomForest(Reverse ~ Circuit + Issue + Petitioner +
                                 Respondent + LowerCourt + Unconst,
                               data=Train, nodesize=25, ntree=200)

# make prediction
PredictForest3 <- predict(StevensForest3, newdata=Test)
table(Test$Reverse, PredictForest3)
# Accuracy (subject to randomness) = (40+74) / (40+37+19+74)
(44+76) / (44+33+17+76)


#### Video 6: Cross-Validation

# Important Note about this video
# In this video, we install and load two new packages so that we can perform cross-validation: "caret", and "e1071". You may need to additionally install and load the following packages for cross-validation to work on your computer: "class" and "ggplot2". If you receive an error message after trying to load caret and e1071, please try installing and loading these two additional packages.

#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)

# set model parameters
fitControl <- trainControl(method="cv", number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + 
        Unconst, data=Train, method="rpart", trControl=fitControl,
      tuneGrid=cartGrid)

# build model
StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                         Respondent + LowerCourt + Unconst, 
                       method="class", data=Train, 
                       control=rpart.control(cp=0.18))

# make predictiton
PredictCV <- predict(StevensTreeCV, newdata=Test, type="class")
table(Test$Reverse, PredictCV)
accuracy <- (59+64) / (59+18+29+64)
accuracy

# Quick Question 6
# Plot the tree that we created using cross-validation. How many splits does it have?
prp(StevensTreeCV)


##### Recitation - Location, Location, Location: Regression Trees for Housing Data

boston <- read.csv("boston.csv")
str(boston)

plot(boston$LON, boston$LAT)

# show points on the river 
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], 
       col="blue", pch=19)

# show census track for MIT
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531],
       col="red", pch=19)

# look at air pollution
summary(boston$NOX)

# look at above average pollution
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55],
       col="green", pch=19)

# reset plot
plot(boston$LON, boston$LAT)

# look at prices
summary(boston$MEDV)

# look at above average prices
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2],
       col="red", pch=19)


#### Video 3: Geographical Predictions

plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

# strange linear model
latlonlm <- lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2],
       col="red", pch=19)

latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2], boston$LAT[latlonlm$fitted.values>=21.2],
       col="blue", pch="$")


#### Video 4: Regression Trees

library(rpart)
library(rpart.plot)

# model
latlontree <- rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)
# number shows average of median house prices in that leave

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2],
       col="red", pch=19)

fittedvalues <- predict(latlontree)
points(boston$LON[fittedvalues>=21.2], boston$LAT[fittedvalues>=21.2],
       col="blue", pch="$")

# build new tree (model) with higher minbucket size to prevent overfitting
latlontree <- rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree)

plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2],
       col="red", pch=19)


#### Video 5: Putting it all Together

library(caTools)

set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio=0.7)
train <- subset(boston, split==T)
test <- subset(boston, split==F)
linreg <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + 
               CHAS + NOX + RM + AGE + DIS + RAD + TAX + 
               PTRATIO, data=train)

summary(linreg)

# since we deal with continous variables, compute sum of squared errors
linreg.pred <- predict(linreg, newdata=test)
linreg.sse <- sum((linreg.pred - test$MEDV)^2)
linreg.sse
# Can we beat this using regression trees?

# build tree
tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + 
               CHAS + NOX + RM + AGE + DIS + RAD + TAX + 
               PTRATIO, data=train)

prp(tree)

tree.pred <- predict(tree, newdata=test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse
# -> worse than linear regression


#### Video 7: Cross-Validation

library(caret)
library(e1071)

tr.control <- trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp = (0:10)*0.001)
0:10 * 0.001

tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + 
              CHAS + NOX + RM + AGE + DIS + RAD + TAX + 
              PTRATIO, data=train, method="rpart", 
            trControl=tr.control, tuneGrid=cp.grid)
tr

best.tree <- tr$finalModel
prp(best.tree)

best.tree.pred <- predict(best.tree, newdata=test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
# better than old tree model but worse than linear regression
linreg.sse

###### Week 4 - Assignment

##### understanding why people vote

# The researchers grouped the 344,000 voters into different groups randomly - 191,000 voters were a "control" group, and the rest were categorized into one of four "treatment" groups. These five groups correspond to five binary variables in the dataset.

#    "Civic Duty" (variable civicduty) group members were sent a letter that simply said "DO YOUR CIVIC DUTY - VOTE!"
#    "Hawthorne Effect" (variable hawthorne) group members were sent a letter that had the "Civic Duty" message plus the additional message "YOU ARE BEING STUDIED" and they were informed that their voting behavior would be examined by means of public records.
#    "Self" (variable self) group members received the "Civic Duty" message as well as the recent voting record of everyone in that household and a message stating that another message would be sent after the election with updated records.
#    "Neighbors" (variable neighbors) group members were given the same message as that for the "Self" group, except the message not only had the household voting records but also that of neighbors - maximizing social pressure.
#    "Control" (variable control) group members were not sent anything, and represented the typical voting situation.

# Additional variables include sex (0 for male, 1 for female), yob (year of birth), and the dependent variable voting (1 if they voted, 0 otherwise).

gerber <- read.csv("gerber.csv")
str(gerber)

#### Problem 1 - Exploration and Logistic Regression

# What proportion of people in this dataset voted in this election?
prop.table(table(gerber$voting))

# Which of the four "treatment groups" had the largest fraction of voters?
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

# Build a logistic regression model for voting using the four treatment group variables as the independent variables (civicduty, hawthorne, self, and neighbors). Use all the data to build the model (DO NOT split the data into a training set and testing set).
gerberLog1 <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")

# Which of the following coefficients are significant in the logistic regression model?
summary(gerberLog1)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predict1 <- predict(gerberLog1, newdata=gerber, type="response")
table(gerber$voting, predict1 >= 0.3)
TruePositives <- 51966
FalseNegatives <- 56730
TrueNegatives <- 134513
FalsePositives <- 100875
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(gerber)

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, predict1 >= 0.5)
TruePositives <- 0
FalseNegatives <- 108696
TrueNegatives <- 235388
FalsePositives <- 0
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(gerber)

# Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) and compute the AUC of the model. What is happening here?
prop.table(table(gerber$voting))
ROCRpredTest <- prediction(predict1, gerber$voting)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
# -> Even though all of the variables are significant, this is a weak predictive model. 


#### Problem 2 - Trees

# We will now try out trees. Build a CART tree for voting using all data and the same four treatment variables we used before. Don't set the option method="class" - we are actually going to create a regression tree here. We are interested in building a tree to explore the fraction of people who vote, or the probability of voting. We’d like CART to split our groups if they have different probabilities of voting. If we used method=‘class’, CART would only split if one of the groups had a probability of voting above 50% and the other had a probability of voting less than 50% (since the predicted outcomes would be different). However, with regression trees, CART will split even if both groups have probability less than 50%.

# Leave all the parameters at their default values. You can use the following command in R to build the tree:
CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

# Plot the tree. What happens, and if relevant, why?
prp(CARTmodel)
# -> No variables are used (the tree is only a root node) - none of the variables make a big enough effect to be split on. 

# Now build the tree using the command:
CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

# Then plot the tree. What do you observe about the order of the splits?
prp(CARTmodel2)

# Using only the CART tree plot, determine what fraction (a number between 0 and 1) of "Civic Duty" people voted:
prp(CARTmodel2)
# -> 0.31

# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a split that is of secondary importance to the treatment group.
CARTmodel3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

# In the control group, which gender is more likely to vote?
prp(CARTmodel3)
# -> Men

# In the "Civic Duty" group, which gender is more likely to vote?
prp(CARTmodel3)
# -> Men


#### Problem 3 - Interaction Terms

# Let's just focus on the "Control" treatment group. Create a regression tree using just the "control" variable, then create another tree with the "control" and "sex" variables, both with cp=0.0.
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)

# In the "control" only tree, what is the absolute value of the difference in the predicted probability of voting between being in the control group versus being in a different group? You can use the absolute value function to get answer, i.e. abs(Control Prediction - Non-Control Prediction). Add the argument "digits = 6" to the prp command to get a more accurate estimate.
prp(CARTmodel4, digits = 6)
abs(0.296638-0.34)

# Now, using the second tree (with control and sex), determine who is affected more by NOT being in the control group (being in any of the four treatment groups):
prp(CARTmodel5, digits = 6)
abs(0.290456-0.334176)
abs(0.302795-0.345818)
# -> They are affected about the same (change in probability within 0.001 of each other).

# Going back to logistic regression now, create a model using "sex" and "control". Interpret the coefficient for "sex":
gerberLog2 <- glm(voting ~ sex + control, data=gerber, family="binomial")
summary(gerberLog2)
# -> Coefficient is negative, reflecting that women are less likely to vote

# The regression tree calculated the percentage voting exactly for every one of the four possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control). Logistic regression has attempted to do the same, although it wasn't able to do as well because it can't consider exactly the joint possibility of being a women and in the control group.

# We can quantify this precisely. Create the following dataframe (this contains all of the possible values of sex and control), and evaluate your logistic regression using the predict function (where "LogModelSex" is the name of your logistic regression model that uses both control and sex):
Possibilities <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerberLog2, newdata=Possibilities, type="response")

# The four values in the results correspond to the four possibilities in the order they are stated above ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ). 
# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case? Give an answer with five numbers after the decimal point.
abs(0.2908065-0.290456)

# So the difference is not too big for this dataset, but it is there. We're going to add a new term to our logistic regression now, that is the combination of the "sex" and "control" variables - so if this new variable is 1, that means the person is a woman AND in the control group. We can do that with the following command:
LogModel2 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

# How do you interpret the coefficient for the new variable in isolation? That is, how does it relate to the dependent variable?
summary(LogModel2)
# -> If a person is a woman and in the control group, the chance that she voted goes down. 

# Run the same code as before to calculate the average for each group:
predict(LogModel2, newdata=Possibilities, type="response")

# Now what is the difference between the logistic regression model and the CART model for the (Woman, Control) case? Again, give your answer with five numbers after the decimal point.
abs(0.2904558-0.290456)

# !! This example has shown that trees can capture nonlinear relationships that logistic regression can not, but that we can get around this sometimes by using variables that are the combination of two variables. 

# Should we always include all possible interaction terms of the independent variables when building a logistic regression model?
# -> No
# Explanation: We should not use all possible interaction terms in a logistic regression model due to overfitting. Even in this simple problem, we have four treatment groups and two values for sex. If we have an interaction term for every treatment variable with sex, we will double the number of variables. In smaller data sets, this could quickly lead to overfitting.


##### letter recognition

# Variables:
#    letter = the letter that the image corresponds to (A, B, P or R)
#    xbox = the horizontal position of where the smallest box covering the letter shape begins.
#    ybox = the vertical position of where the smallest box covering the letter shape begins.
#    width = the width of this smallest box.
#    height = the height of this smallest box.
#    onpix = the total number of "on" pixels in the character image
#    xbar = the mean horizontal position of all of the "on" pixels
#    ybar = the mean vertical position of all of the "on" pixels
#    x2bar = the mean squared horizontal position of all of the "on" pixels in the image
#    y2bar = the mean squared vertical position of all of the "on" pixels in the image
#    xybar = the mean of the product of the horizontal and vertical position of all of the "on" pixels in the image
#    x2ybar = the mean of the product of the squared horizontal position and the vertical position of all of the "on" pixels
#    xy2bar = the mean of the product of the horizontal position and the squared vertical position of all of the "on" pixels
#    xedge = the mean number of edges (the number of times an "off" pixel is followed by an "on" pixel, or the image boundary is hit) as the image is scanned from left to right, along the whole vertical length of the image
#    xedgeycor = the mean of the product of the number of horizontal edges at each vertical position and the vertical position
#    yedge = the mean number of edges as the images is scanned from top to bottom, along the whole horizontal length of the image
#    yedgexcor = the mean of the product of the number of vertical edges at each horizontal position and the horizontal position

letters <- read.csv("letters_ABPR.csv")


#### Problem 1 - Predicting B or not B

# Then, create a new variable isB in the dataframe, which takes the value "yes" if the observation corresponds to the letter B, and "no" if it does not. You can do this by typing the following command into your R console:
letters$isB <- as.factor(letters$letter == "B")

# Now split the data set into a training and testing set, putting 50% of the data in the training set. Set the seed to 1000 before making the split. The first argument to sample.split should be the dependent variable "letters$isB". Remember that TRUE values from sample.split should go in the training set.
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio=0.5)
train <- subset(letters, split==T)
test <- subset(letters, split==F)

# Before building models, let's consider a baseline method that always predicts the most frequent outcome, which is "not B".
table(letters$isB)

# What is the accuracy of this baseline method on the test set?
2350 / (2350+766)

# Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model. Remember to remove the variable "letter" out of the model, as this is related to what we are trying to predict! To just remove one variable, you can either write out the other variables, or remember what we did in the Billboards problem in Week 3, and use the following notation:
CARTmodel1 <- rpart(isB ~ . - letter, data=train, method="class")
# We are just using the default parameters in our CART model, so we don't need to add the minbucket or cp arguments at all. We also added the argument method="class" since this is a classification problem.

# What is the accuracy of the CART model on the test set? (Use type="class" when making predictions on the test set.)
PredictCART <- predict(CARTmodel1, newdata=test, type="class")
# threshold of 0.5
table(test$isB, PredictCART)
TruePositives <- 340
FalseNegatives <- 43
TrueNegatives <- 1118
FalsePositives <- 57
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

set.seed(1000)
lettersForest1 <- randomForest(isB ~ . - letter, data=train)
predictForest1 <- predict(lettersForest1, newdata=test)
table(test$isB, predictForest1)
# Accuracy (subject to randomness) = (1165+377) / (1165+10+6+377)
(1165+377) / (1165+10+6+377)
# In lecture, we noted that random forests tends to improve on CART in terms of predictive accuracy. Sometimes, this improvement can be quite significant, as it is here.

####  Problem 2 - Predicting the letters A, B, P, R 

# The variable in our data frame which we will be trying to predict is "letter". Start by converting letter in the original data set (letters) to a factor by running the following command in R:
letters$letter = as.factor(letters$letter)

# Now, generate new training and testing sets of the letters data frame using letters$letter as the first input to the sample.split function. Before splitting, set your seed to 2000. Again put 50% of the data in the training set. 
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio=0.5)
train <- subset(letters, split==T)
test <- subset(letters, split==F)

# What is the baseline accuracy on the testing set?
table(letters$letter)
803 / (789 + 766 + 803 + 758)

# Now build a classification tree to predict the letter, using the training set to build your model.
CARTmodel2 <- rpart(letter ~ . - isB, data=train, method="class")
PredictCART2 <- predict(CARTmodel2, newdata=test, type="class")
# threshold of 0.5
table(test$letter, PredictCART2)
(348+318+363+340)/nrow(test)

# Now estimate a random forest model on the training data -- again, don't forget to remove the isB variable. Set the seed to 1000 right before building your model.
set.seed(1000)
lettersForest2 <- randomForest(letter ~ . - isB, data=train)
predictForest2 <- predict(lettersForest2, newdata=test)
table(test$letter, predictForest2)
# Accuracy (subject to randomness)
(390+380+393+364) / (390+3+2+380+1+2+393+5+3+364+3+12)

# !! You should find this value rather striking, for several reasons. The first is that it is significantly higher than the value for CART, highlighting the gain in accuracy that is possible from using random forest models. The second is that while the accuracy of CART decreased significantly as we transitioned from the problem of predicting B/not B (a relatively simple problem) to the problem of predicting the four letters (certainly a harder problem), the accuracy of the random forest model decreased by a tiny amount.


##### State data Revisited

# Load the dataset into R and convert it to a data frame by running the following two commands in R:
data(state)
statedata <- data.frame(state.x77)

# Inspect the data set using the command:
str(statedata)


#### Problem 1 - Linear Regression Models

# First, predict Life.Exp using all of the other variables as the independent variables (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area ). Use the entire dataset to build the model.
lm1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)

# What is the adjusted R-squared of the model?
summary(lm1)

# Calculate the sum of squared errors (SSE) between the predicted life expectancies using this model and the actual life expectancies:
predictLM1 <- predict(lm1)
SSE <- sum((statedata$Life.Exp - predictLM1)^2)
SSE
SSE <- sum(LinReg1$residuals^2)
SSE

# Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables (the best 4 variable model from the previous homework). 
lm2 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)

# What is the adjusted R-squared for this model?
summary(lm2)

# Calculate the sum of squared errors again, using this reduced model:
SSE <- sum(lm2$residuals^2)
SSE

# Which of the following is correct?
# -> Trying different combinations of variables in linear regression is like trying different numbers of splits in a tree - this controls the complexity of the model. 


#### Problem 2 - CART Models

# Let's now build a CART model to predict Life.Exp using all of the other variables as independent variables (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area). We'll use the default minbucket parameter, so don't add the minbucket argument. Remember that in this problem we are not as interested in predicting life expectancies for new observations as we are understanding how they relate to the other variables we have, so we'll use all of the data to build our model. You shouldn't use the method="class" argument since this is a regression tree.
CARTmodel1 <- rpart(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, 
                    data=statedata)

# Plot the tree. Which of these variables appear in the tree?
prp(CARTmodel1)

# Use the regression tree you just built to predict life expectancies (using the predict function), and calculate the sum-of-squared-errors (SSE) like you did for linear regression. 
predictTree1 <- predict(CARTmodel1)

# What is the SSE?
predictTree1.sse <- sum((predictTree1 - statedata$Life.Exp)^2)
predictTree1.sse

# The error is higher than for the linear regression models. One reason might be that we haven't made the tree big enough. Set the minbucket parameter to 5, and recreate the tree.
CARTmodel2 <- rpart(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, 
                    data=statedata, minbucket=5)

# Which variables appear in this new tree?
prp(CARTmodel2)

# Do you think the default minbucket parameter is smaller or larger than 5 based on the tree that was built?
# -> Larger

# What is the SSE of this tree?
predictTree2 <- predict(CARTmodel2)
predictTree2.sse <- sum((predictTree2 - statedata$Life.Exp)^2)
predictTree2.sse

# Can we do even better? Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1. 
CARTmodel3 <- rpart(Life.Exp ~ Area, data=statedata, minbucket=1)

# What is the SSE of this newest tree?
predictTree3 <- predict(CARTmodel3)
predictTree3.sse <- sum((predictTree3 - statedata$Life.Exp)^2)
predictTree3.sse

# This is the lowest error we have seen so far. What would be the best interpretation of this result?
# -> We can build almost perfect models given the right parameters, even if they violate our intuition of what a good model should be. 
# Explanation: The correct answer is the second one. By making the minbucket parameter very small, we could build an almost perfect model using just one variable, that is not even our most significant variable. However, if you plot the tree using prp(CARTmodel3), you can see that the tree has 22 splits! This is not a very interpretable model, and will not generalize well.


#### Problem 3 - Cross-validation

# Load the caret library, and set the seed to 111. 
library(caret)
set.seed(111)

# Set up the controls with cp varying over the range 0.01 to 0.50 in increments of 0.01. 
tr.control <- trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp = (1:50)*0.01)

# Use the train function to determine the best cp value.
tr <- train(Life.Exp ~ Population + Income + Illiteracy + Murder + 
            HS.Grad + Frost + Area, data=statedata, method="rpart", 
            trControl=tr.control, tuneGrid=cp.grid)

# What value of cp does the train function recommend?
tr

library(e1071)

# Create a tree with this value of cp. You'll notice that this is actually quite similar to the first tree we created with the initial model.
best.tree <- tr$finalModel
prp(best.tree)

# Interpret the tree: we predict the life expectancy to be 70 if the murder rate is greater than or equal to
# -> 6.6

# and is less than
# -> 11

# Calculate the SSE of this tree:
best.tree.pred <- predict(best.tree)
best.tree.sse <- sum((best.tree.pred - statedata$Life.Exp)^2)
best.tree.sse

# Recall the first tree (default parameters), second tree (minbucket = 5), and the third tree (selected with cross validation) we made. Given what you have learned about cross-validation, which of the three models would you expect to be better if we did use it for prediction on a test set? 
# -> The model we just made with the "best" cp
# Explanation: The purpose of cross-validation is to pick the tree that will perform the best on a test set. So we would expect the model we made with the "best" cp to perform best on a test set.

# At the end of Part 2 we made a very complex tree using just Area. Use train with the same parameters as before but just using Area as an independent variable to find the best cp value (set the seed to 111 first).
set.seed(111)
tr.control <- trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp = (1:50)*0.01)
tr <- train(Life.Exp ~ Area, data=statedata, method="rpart", 
            trControl=tr.control, tuneGrid=cp.grid)

# How many splits does the tree have?
best.tree <- tr$finalModel
prp(best.tree)
# -> 4

# The lower left leaf (or bucket) corresponds to the lowest predicted Life.Exp, (70). Observations in this leaf correspond to states with area greater than
# -> 9579

# and area less than
# -> 51000

# Calculate the SSE of the cross-validated "Area tree", and select the correct statements:
best.tree.pred <- predict(best.tree)
best.tree.sse <- sum((best.tree.pred - statedata$Life.Exp)^2)
best.tree.sse
# -> The Area variable is not as predictive as Murder rate
# Explanation: The original Area tree was overfitting the data - it was uninterpretable. Area is not as useful as Murder - if it was, it would have been in the cross-validated tree. Cross-validation will never improve the fit on the training data, but it won't necessarily make it significantly worse. Cross-validation cannot guarantee improving the SSE on unseen data, although it often helps.


##### Predicting Earnings from census data


#    age = the age of the individual in years
#    workclass = the classification of the individual's working status (does the person work for the federal government, work for the local government, work without pay, and so on)
#    education = the level of education of the individual (e.g., 5th-6th grade, high school graduate, PhD, so on)
#    maritalstatus = the marital status of the individual
#    occupation = the type of work the individual does (e.g., administrative/clerical work, farming/fishing, sales and so on)
#    relationship = relationship of individual to his/her household
#    race = the individual's race
#    sex = the individual's sex
#    capitalgain = the capital gains of the individual in 1994 (from selling an asset such as a stock or bond for more than the original purchase price)
#    capitalloss = the capital losses of the individual in 1994 (from selling an asset such as a stock or bond for less than the original purchase price)
#    hoursperweek = the number of hours the individual works per week
#    nativecountry = the native country of the individual
#    over50k = whether or not the individual earned more than $50,000 in 1994

census <- read.csv("census.csv")
str(census)

# Then, split the data randomly into a training set and a testing set, setting the seed to 2000 before creating the split. Split the data so that the training set contains 60% of the observations, while the testing set contains 40% of the observations.
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio=0.6)
train <- subset(census, split==T)
test <- subset(census, split==F)


#### Problem 1 - A Logistic Regression Model

# Next, build a logistic regression model using all of the independent variables to predict the dependent variable "over50k", and use the training set to build the model.
censusLog1 <- glm(over50k ~ ., data=train, family="binomial")

# Which variables are significant, or have factors that are significant? (Use 0.1 as your significance threshold, so variables with a period or dot in the stars column should be counted too. You might see a warning message here - ignore it.) 
summary(censusLog1)

# What is the accuracy of the model on the testing set? Use a threshold of 0.5.
censusPredict1 <- predict(censusLog1, newdata=test, type="response")
table(test$over50k, censusPredict1 >= 0.5)
TruePositives <- 1888
FalseNegatives <- 1190
TrueNegatives <- 9051
FalsePositives <- 662
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# What is the baseline accuracy for the testing set?
table(test$over50k)
9713 / nrow(test)

# What is the area-under-the-curve (AUC) for this model on the test set?
ROCRpredTest <- prediction(censusPredict1, test$over50k)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


#### Problem 2 - A CART Model

# Using the same training set, fit a CART model, and plot the tree. Use the default parameters, so don't set a value for minbucket or cp. Remember to specify method="class" as an argument to rpart, since this is a classification problem.
censusCART1 <- rpart(over50k ~ ., data=train, method="class")

# How many splits does the tree have in total?
prp(censusCART1)
# -> 4

# Which variable does the tree split on at the first level (the very first split of the tree)?
# -> relationship

# Which variables does the tree split on at the second level (immediately after the first split of the tree)?
# -> capitalgain, education

# What is the accuracy of the model on the testing set? (Use a threshold of 0.5, so add the argument type="class".)
censusPredict2 <- predict(censusCART1, newdata=test, type="class")
# threshold of 0.5
table(test$over50k, censusPredict2)
TruePositives <- 1596
FalseNegatives <- 1482
TrueNegatives <- 9243
FalsePositives <- 470
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)
# Explanation: This highlights a very regular phenomenon when comparing CART and logistic regression. CART often performs a little worse than logistic regression in out-of-sample accuracy. However, as is the case here, the CART model is often much simpler to describe and understand. 

# Let us now consider the ROC curve and AUC for the CART model. Plot the ROC curve for the CART model you have estimated.
PredictROC <- predict(censusCART1, newdata=test)
# interpret column 2 as probability that this test set observation has outcome 1
# we use second column when thresholding
pred <- prediction(PredictROC[,2], test$over50k)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
# Observe that compared to the logistic regression ROC curve, the CART ROC curve is less smooth than the logistic regression ROC curve. 

# Which of the following explanations for this behavior is most correct? (HINT: Think about what the ROC curve is plotting and what changing the threshold does.)
# -> The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold to one of those values. 
# Explanation: The breakpoints of the curve correspond to the false and true positive rates when the threshold is set to the five possible probability values. 

# What is the AUC of the CART model on the test set?
as.numeric(performance(pred, "auc")@y.values)


#### Problem 3 - A Random Forest Model

# Before building a random forest model, we'll down-sample our training set. While some modern personal computers can build a random forest model on the entire training set, others might run out of memory when trying to train the model since random forests is much more computationally intensive than CART or Logistic Regression. For this reason, before continuing we will define a new training set to be used when building our random forest model, that contains 2000 randomly selected obervations from the original training set.

# Do this by running the following commands in your R console (assuming your training set is called "train"):
set.seed(1)
trainSmall <- train[sample(nrow(train), 2000), ]

# Go ahead and attempt to build a random forest model. You should get an error that random forest "can not handle categorical predictors with more than 32 categories". This means that we have a factor variable with more than 32 different possible values.
censusForest1 <- randomForest(over50k ~ ., data=train)

# Which one of your variables is causing this error?
str(trainSmall)

# Now, build your random forest model without the problematic variable identified in the previous problem.
set.seed(1)
censusForest2 <- randomForest(over50k ~ . - nativecountry, data=trainSmall)

# Then, make predictions using this model on the entire test set. 
censusPredict3 <- predict(censusForest2, newdata=test)

# What is the accuracy of the model on the test set? (Remember that you don't need a "type" argument when making predictions with a random forest model.)
# Accuracy (subject to randomness)
table(test$over50k, censusPredict3)
TruePositives <- 2035
FalseNegatives <- 1043
TrueNegatives <- 8883
FalsePositives <- 830
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# One metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, that a certain variable is selected for a split. To view this metric, run the following lines of R code 
vu <- varUsed(censusForest2, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusForest2$forest$xlevels[vusorted$ix]))
# This code produces a chart that for each variable measures the number of times that variable was selected for splitting (the value on the x-axis). 

# Which of the following variables is the most important in terms of the number of splits?
# -> age

# A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. Therefore, one way to measure the importance of a variable is to average the reduction in impurity, taken over all the times that variable is selected for splitting in all of the trees in the forest. To compute this metric, run the following command in R
varImpPlot(censusForest2)

# Which one of the following variables is the most important in terms of mean reduction in impurity?
# -> occupation


#### Problem 4 - Selecting cp by Cross-Validation

# Let us select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds. Do this by using the train function. Set the seed beforehand to 2. Test cp values from 0.002 to 0.1 in 0.002 increments
set.seed(2)
tr.control <- trainControl(method="cv", number=10)
cp.grid <- expand.grid(.cp = seq(0.002,0.1,0.002))
tr <- train(over50k ~ . - nativecountry, data=train, method="rpart", 
            trControl=tr.control, tuneGrid=cp.grid)

# Which value of cp does the train function recommend?
tr

# Fit a CART model to the training data using this value of cp. 
censusCART2 <- rpart(over50k ~ ., data=train, cp=0.002)
censusPredict4 <- predict(censusCART2, newdata=test, type="class")

# What is the prediction accuracy on the test set?
table(test$over50k, censusPredict4)
TruePositives <- 1838
FalseNegatives <- 1240
TrueNegatives <- 9178
FalsePositives <- 535
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# Plot the CART tree for this model. How many splits are there? 
prp(censusCART2)
# -> 18

# !! This highlights one important tradeoff in building predictive models. By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more complicated. In some applications, such an improvement in accuracy would be worth the loss in interpretability. In others, we may prefer a less accurate model that is simpler to understand and describe over a more accurate -- but more complicated -- model.


###### Week 5: Text Analytics

##### Turning Tweets into Knowledge: An Introduction to Text Analytics 

#### Video 5: Pre-Processing in R

tweets <- read.csv("tweets.csv", stringsAsFactors=F)
str(tweets)

tweets$Negative <- as.factor(tweets$Avg <= -1)
table(tweets$Negative)

#install.packages("tm")
#install.packages("SnowballC")
library(tm)
library(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]

# change to lower case
corpus <- tm_map(corpus, tolower)
corpus[[1]]

# remover punctuation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]

# remove stop words and apple
stopwords("english")[1:10]
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

# stemming documents
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]


#### Video 6: Bag of Words in R

# generate matrix where rows are documents and columns correspond to words in tweets
frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])

# most popular terms
findFreqTerms(frequencies, lowfreq=20)

# make sparse matrix e.g. 0.995 means only keep words that appear at least in 0.5% of tweets i.e. every 6th tweet
sparse <- removeSparseTerms(frequencies, 0.995)
sparse

# convert sparse matrix into dataframe
tweetsSparse <- as.data.frame(as.matrix(sparse))

# avoid numbers in variable names
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

# add dependent variable
tweetsSparse$Negative <- tweets$Negative

# split into train and test
library(caTools)
set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse <- subset(tweetsSparse, split==T)
testSparse <- subset(tweetsSparse, split==F)

# Quick Question 6
# In the previous video, we showed a list of all words that appear at least 20 times in our tweets. Which of the following words appear at least 100 times?
findFreqTerms(frequencies, lowfreq=100)


#### Video 7: Predicting Sentiment

library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)
# if "freak" is in tweet, we predict True (negative sentiment)
# if "freak" is not but "hate" is in tweet, we predict True (negative sentiment)
# if "freak" and "hate" are not but "wtf" is in tweet, we predict True (negative sentiment)
# else we predict false (non-negative sentiment)

# make prediction
predictCART <- predict(tweetCART, newdata=testSparse, type="class")

# accuracy
table(testSparse$Negative, predictCART)
TruePositives <- 18
FalseNegatives <- 37
TrueNegatives <- 294
FalsePositives <- 6
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(testSparse)

# baseline model accuracy
table(testSparse$Negative)
300 / (300+55)

# try random forest
library(randomForest)
set.seed(123)

# random forest model
tweetRF <- randomForest(Negative ~ ., data=trainSparse) 

# predict
predictRF <- predict(tweetRF, newdata=testSparse)

# accuracy
table(testSparse$Negative, predictRF)
TruePositives <- 22
FalseNegatives <- 33
TrueNegatives <- 293
FalsePositives <- 7
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(testSparse)

# Quick Question 7
tweetLog <- glm(Negative ~ ., data=trainSparse, family="binomial")
# Now, make predictions using the logistic regression model:
predictions <- predict(tweetLog, newdata=testSparse, type="response")
# Build a confusion matrix (with a threshold of 0.5) and compute the accuracy of the model. 
table(testSparse$Negative, predictions >= 0.5)
TruePositives <- 33
FalseNegatives <- 22
TrueNegatives <- 253
FalsePositives <- 47
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(testSparse)
# Explanation: The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. If you were to compute the accuracy on the training set instead, you would see that the model does really well on the training set - this is an example of over-fitting. The model fits the training set really well, but does not perform well on the test set. A logistic regression model with a large number of variables is particularly at risk for overfitting.
# Explanation: Note that you might have gotten a different answer than us, because the glm function struggles with this many variables. The warning messages that you might have seen in this problem have to do with the number of variables, and the fact that the model is overfitting to the training set. We'll discuss this in more detail in the Homework Assignment.


##### Recitation: Predictive Coding: Bringing Text Analytics to the Courtroom


#### Video 2: The Data

emails <- read.csv("energy_bids.csv", stringsAsFactors=F)
str(emails)

emails$email[1]
strwrap(emails$email[1])

# mail is not responsive i.e. not relevant so value is zero
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]

# get frequencies
table(emails$responsive)


#### Video 3: Pre-Processing

library(tm)

corpus <- Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

# convert to lower case
corpus <- tm_map(corpus, tolower)

# removing punctuation
corpus <- tm_map(corpus, removePunctuation)

# remove stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# stem document
corpus <- tm_map(corpus, stemDocument)

# take a look at second mail again
strwrap(corpus[[1]])


#### Video 4: Bag of Words

# document term matrix
dtm <- DocumentTermMatrix(corpus)

# summary statistics
dtm

# remove any term that does not appear in at least 3% of the cases
dtm <- removeSparseTerms(dtm, 0.97)
dtm

# build dataframe with frequencies
labeledTerms <- as.data.frame(as.matrix(dtm))

# add dependent/outcome variable
labeledTerms$responsive <- emails$responsive

# take a look again
str(labeledTerms)


#### Video 5: Building Models

# split data into train and test
library(caTools)
set.seed(144)
split <- sample.split(labeledTerms$responsive, 0.7)
train <- subset(labeledTerms, split==T)
test <- subset(labeledTerms, split==F)

# build simple CART model
library(rpart)
library(rpart.plot)
emailCART <- rpart(responsive ~ ., data=train, method="class")
prp(emailCART)
# if California appears at least twice in the mail we take right pati.e. 1 so mail is responsive i.e. relevant


#### Video 6: Evaluating the Model

# predict probabilities for each class
pred <- predict(emailCART, newdata=test)

# we want to extract the predicted probability of document being responsive i.e. 1, so second column
pred[1:10,]
pred.prob <- pred[,2]

# accuracy at 0.5
table(test$responsive, pred.prob >= 0.5)
TruePositives <- 25
FalseNegatives <- 17
TrueNegatives <- 195
FalsePositives <- 20
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# accuracy of baseline
table(test$responsive)
215 / nrow(test)
# only small improvement of CART which is common in unbalanced datasets

# !!there are uneven costs for different types of erros
# - if we have a false positive, then this will be identified in later human revision when a person classifies all falsely labeled responsive mails to actually non-responsive
# - but if we have a false negative, then this won't be identified, since these mails do not go into hunam review and error will not be rectified. So the costs for thiss error should be higher. So look at other cut-offs of ROC curve.
# !! Recall:
# threshold value of t = 0.5 is neither preferring good or bad care classification
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
# Model with higher threshold (> 0.5) will have a lower sensitivity and higher specificity
# Model with lower threshold (> 0.5) will have a higher sensitivity and lower specificity
# We want to increase sensitivity (favor false positives to false negatives) --> lower threshold


#### Video 7: The ROC Curve

library(ROCR)

# ROCR prediction object
predROCR <- prediction(pred.prob, test$responsive)

# use performance function to extract True Positive Rate and False Positive Rate
perfROCR <- performance(predROCR, "tpr", "fpr")

# plot ROC curve
plot(perfROCR, colorize=T)
# cut-off to select is determined by costs (see explanation of costs above)
# We want to increase sensitivity (favor false positives to false negatives) --> lower threshold
# looking at blue color of curve shows we look at threshold of around 0.15

# compute AUC value
performance(predROCR, "auc")@y.values
# model can differentiate between a randomly selected responsive and non-responsive document about 80% of the time.


###### Week 5 Assignment

##### Detecting vandalism on wikipedia

# As a result of this preprocessing, some common processing tasks have already been done, including lower-casing and punctuation removal. The columns in the dataset are:

#    Vandal = 1 if this edit was vandalism, 0 if not.
#    Minor = 1 if the user marked this edit as a "minor edit", 0 if not.
#    Loggedin = 1 if the user made this edit while using a Wikipedia account, 0 if they were not.
#    Added = The unique words added.
#    Removed = The unique words removed.

# Notice the repeated use of unique. The data we have available is not the bag of words - rather it is the set of words that were removed or added. For example, if a word was removed multiple times in a revision it will only appear one time in the "Removed" column.

#### Problem 1 - Bags of Words

# Load the data wiki.csv with the option stringsAsFactors=FALSE, calling the data frame "wiki". Convert the "Vandal" column to a factor using the command wiki$Vandal = as.factor(wiki$Vandal).
wiki <- read.csv("wiki.csv", stringsAsFactors=F)
str(wiki)
wiki$Vandal <- as.factor(wiki$Vandal)
str(wiki)

# How many cases of vandalism were detected in the history of this page?
length(wiki$Vandal[wiki$Vandal==1])

# Create the corpus for the Added column, and call it "corpusAdded"
corpusAdded <- Corpus(VectorSource(wiki$Added))

# Remove the English-language stopwords.
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))

# Stem the words. 
corpusAdded <- tm_map(corpusAdded, stemDocument)

# Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded <- DocumentTermMatrix(corpusAdded)

# check - should return 174
length(stopwords("english"))

# How many terms appear in dtmAdded?
dtmAdded

# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded.
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)

# How many terms appear in sparseAdded?
sparseAdded

# Convert sparseAdded to a data frame called wordsAdded
wordsAdded <- as.data.frame(as.matrix(sparseAdded))

# then prepend all the words with the letter A, by using the command:
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

# Now repeat all of the steps we've done so far (create a corpus, remove stop words, stem the document, create a sparse document term matrix, and convert it to a data frame) to create a Removed bag-of-words dataframe, called wordsRemoved, except this time, prepend all of the words with the letter R:
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

# How many words are in the wordsRemoved data frame?
str(wordsRemoved)

# Combine the two dataframes (using cbind) into a data frame called wikiWords then add the Vandal column
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

# Set the random seed to 123 and then split the data set using sample.split from the "caTools" package to put 70% in the training set.
library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio=0.7)
train <- subset(wikiWords, split==T)
test <- subset(wikiWords, split==F)

# What is the accuracy on the test set of a baseline method that always predicts "not vandalism"
table(test$Vandal)
618 / nrow(test)

# Build a CART model to predict Vandal, using all of the other variables as independent variables. Use the training set to build the model and the default parameters (don't set values for minbucket or cp). 
wikiCART <- rpart(Vandal ~ ., data=train)

# What is the accuracy of the model on the test set, using a threshold of 0.5?
predCART <- predict(wikiCART, newdata=test, type="class")
table(test$Vandal, predCART)
TruePositives <- 12
FalseNegatives <- 533
TrueNegatives <- 618
FalsePositives <- 0
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)

# Given the performance of the CART model relative to the baseline, what is the best explanation of these results?
# -> Although it beats the baseline, bag of words is not very predictive for this problem.


#### Problem 2 - Problem-specific Knowledge

# We can search for the presence of a web address in the words added by searching for "http" in the Added column. 
# The grepl function returns TRUE if a string is found in another string, e.g.
grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE

# Create a copy of your dataframe from the previous question:
wikiWords2 <- wikiWords

# Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

# Based on this new column, how many revisions added a link?
length(wikiWords2$HTTP[wikiWords2$HTTP==1])

# Use the split that we created before to make new training and testing sets:
wikiTrain2 <- subset(wikiWords2, split==T)
wikiTest2 <- subset(wikiWords2, split==F)

# Then create a new CART model using this new variable as one of the independent variables.
wikiCART2 <- rpart(Vandal ~ ., data=wikiTrain2)

# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
predCART2 <- predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predCART2)
TruePositives <- 57
FalseNegatives <- 488
TrueNegatives <- 609
FalsePositives <- 9
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(wikiTest2)

# Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual words themselves. We already have a word count available in the form of the document-term matrices (DTMs).
# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 (called NumWordsAdded and NumWordsRemoved) by using the following commands:
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))

# What is the average number of words added?
mean(wikiWords2$NumWordsAdded)

# Then, use your orginal split (do not generate the split again) to split the data into a training set and testing set, like we did in Problem 2.2
wikiTrain2 <- subset(wikiWords2, split==T)
wikiTest2 <- subset(wikiWords2, split==F)

# Then create a new CART model using this new variable as one of the independent variables.
wikiCART3 <- rpart(Vandal ~ ., data=wikiTrain2)

# What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
predCART3 <- predict(wikiCART3, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predCART3)
TruePositives <- 248
FalseNegatives <- 297
TrueNegatives <- 514
FalsePositives <- 104
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(wikiTest2)


#### Problem 3 - Using Non-Textual Data

# We have two pieces of "metadata" (data about data) that we haven't yet used. 
# Make a copy of wikiWords2, and call it wikiWords3:
wikiWords3 <- wikiWords2

# Then add the two original variables Minor and Loggedin to this new data frame:
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

# Use the original split to subset wikiWords3 into a training and a test set.
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio=0.7)
wikiTrain3 <- subset(wikiWords3, split==T)
wikiTest3 <- subset(wikiWords3, split==F)

# Build a CART model using all the training data. 
wikiCART3 <- rpart(Vandal ~ ., data=wikiTrain3, method="class")

# What is the accuracy of the model on the test set?
predCART4 <- predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predCART4)
TruePositives <- 248
FalseNegatives <- 297
TrueNegatives <- 514
FalsePositives <- 104
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(wikiTest3)
# -> 0.7188306

# Plot the CART tree. How many splits are there in the tree?
prp(wikiCART3)


#### Automating reviews in medicine

# Load clinical_trial.csv into a data frame called trials (remembering to add the argument stringsAsFactors=FALSE)
trials <- read.csv("clinical_trial.csv", stringsAsFactors=F)

# and investigate the data frame with summary() and str().
str(trials)
summary(trials)

# How many characters are there in the longest abstract?
max(nchar(trials$abstract))

# How many search results provided no abstract?
length((trials$abstract[nchar(trials$abstract)==0]))

# What is the shortest title of any article? Include capitalization and punctuation in your response, but don't include the quotes.
trials$title[which.min(nchar(trials$title))]


#### Problem 2 - Preparing the Corpus

# Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

# Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

# Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

# Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
sparseTitle <- removeSparseTerms(dtmTitle, 0.95)
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.95)

# Convert dtmTitle and dtmAbstract to data frames.
trialsTitle <- as.data.frame(as.matrix(sparseTitle))
trialsAbstract <- as.data.frame(as.matrix(sparseAbstract))

# How many terms remain in dtmTitle after removing sparse terms (aka how many columns does it have)?
sparseTitle

# How many terms remain in dtmAbstract?
sparseAbstract

# What is the most likely reason why dtmAbstract has so many more terms than dtmTitle?
# -> Abstracts tend to have many more words than titles

# What is the most frequent word stem across all the abstracts? 
which.max(colSums(trialsAbstract))


#### Problem 3 - Building a model

# We want to combine dtmTitle and dtmAbstract into a single data frame to make predictions. However, some of the variables in these data frames have the same names. To fix this issue, run the following commands:
colnames(trialsTitle) <- paste0("T", colnames(trialsTitle))
colnames(trialsAbstract) <- paste0("A", colnames(trialsAbstract))

# What was the effect of these functions?
str(trialsTitle)
# -> Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names. 

# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm.
dtm <- cbind(trialsTitle, trialsAbstract)

# As we did in class, add the dependent variable "trial" to dtm, copying it from the original data frame called trials. 
dtm$rial <- trials$trial

# How many columns are in this combined data frame?
length(dtm)

# Set the random seed to 144 and use the sample.split function from the caTools package to split dtm into data frames named "train" and "test", putting 70% of the data in the training set.
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio=0.7)
train <- subset(dtm, split==T)
test <- subset(dtm, split==F)

# What is the accuracy of the baseline model on the training set?
table(train$trial)
730 / nrow(train)

# Build a CART model called trialCART, using all the independent variables in the training set to train the model
trialCART <- rpart(train$trial ~ ., data=train, method="class")

# plot the CART model
prp(trialCART)

# What is the name of the first variable the model split on?
# -> Tphase

# Obtain the training set predictions for the model (do not yet predict on the test set). Extract the predicted probability of a result being a trial (recall that this involves not setting a type argument, and keeping only the second column of the predict output). 
predictCART1 <- predict(trialCART, data=train)
pred.prob <- predictCART1[,2]

# What is the maximum predicted probability for any result?
max(pred.prob)

# Without running the analysis, how do you expect the maximum predicted probability to differ in the testing set?
# -> The maximum predicted probability will likely be exactly the same in the testing set. 

# For these questions, use a threshold probability of 0.5 to predict that an observation is a clinical trial.

# What is the training set accuracy of the CART model?
predictCART1 <- predict(trialCART, data=train)
table(train$trial, pred.prob >= 0.5)
TruePositives <- 441
FalseNegatives <- 131
TrueNegatives <- 631
FalsePositives <- 99
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(train)

# What is the training set sensitivity of the CART model?
# Sensitivity = True Positives / (True Positives + False Negatives) = True Positive Rate
TruePositives / (TruePositives + FalseNegatives)

# What is the training set specificity of the CART model?
# Specificity = True Negatives / (True Negatives + False Positives) = True Negative Rate
TrueNegatives / (TrueNegatives + FalsePositives)


#### Problem 4 - Evaluating the model on the testing set

predictCART2 <- predict(trialCART, newdata=test)
pred.prob <- predictCART2[,2]
table(test$trial, pred.prob >= 0.5)
TruePositives <- 162
FalseNegatives <- 83
TrueNegatives <- 261
FalsePositives <- 52
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# Using the ROCR package, what is the testing set AUC of the prediction model?
predROCR <- prediction(pred.prob, test$trial)
performance(predROCR, "auc")@y.values

# EXTRA: plot curve
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=T)


#### Problem 5 - Decision-Maker Tradeoffs

# What is the cost associated with the model in Step 1 making a false negative prediction?
# -> A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.

# What is the cost associated with the model in Step 1 making a false positive prediction?
# -> A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3. 

# Given the costs associated with false positives and false negatives, which of the following is most accurate?
# -> A false negative is more costly than a false positive; the decision maker should use a probability threshold less than 0.5 for the machine learning model.
# Explanation: A false negative might negatively affect the results of the literature review and analysis, while a false positive is a nuisance (one additional paper that needs to be manually checked). As a result, the cost of a false negative is much higher than the cost of a false positive, so much so that many studies actually use no machine learning (aka no Step 1) and have two people manually review each search result in Step 2. As always, we prefer a lower threshold in cases where false negatives are more costly than false positives, since we will make fewer negative predictions. 
# Recall: We want to increase sensitivity (favor false positives to false negatives) --> lower threshold
# looking at blue color of curve shows we look at threshold of around 0.3


##### Separating spam from ham (Part 1)

# The dataset contains just two fields:

#    text: The text of the email.
#    spam: A binary variable indicating if the email was spam.


#### Problem 1 - Loading the Dataset

emails <- read.csv("emails.csv", stringsAsFactors=F)

# How many emails are in the dataset?
str(emails)

# How many of the emails are spam?
length(emails$spam[emails$spam==1])

# Which word appears at the beginning of every email in the dataset? Respond as a lower-case word with punctuation removed.
str(emails)

# Could a spam classifier potentially benefit from including the frequency of the word that appears in every email?
# -> Yes -- the number of times the word appears might help us differentiate spam from ham.

# How many characters are in the longest email in the dataset
max(nchar(emails$text))

# Which row contains the shortest email in the dataset?
which.min(nchar(emails$text))


#### Problem 2 - Preparing the Corpus 

# Build a new corpus variable called corpus.
corpus <- Corpus(VectorSource(emails$text))

# Using tm_map, convert the text to lowercase.
corpus <- tm_map(corpus, tolower)

# Using tm_map, remove all punctuation from the corpus.
corpus <- tm_map(corpus, removePunctuation)

# Using tm_map, remove all English stopwords from the corpus.
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Using tm_map, stem the words in the corpus.
corpus <- tm_map(corpus, stemDocument)

# Build a document term matrix from the corpus, called dtm.
dtm <- DocumentTermMatrix(corpus)

# How many terms are in dtm?
dtm

# To obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents, and store this result as spdtm
spdtm <- removeSparseTerms(dtm, 0.95)

# How many terms are in spdtm?
spdtm

# Build a data frame called emailsSparse from spdtm
emailsSparse <- as.data.frame(as.matrix(spdtm))

# use the make.names function to make the variable names of emailsSparse valid.
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

# What is the word stem that shows up most frequently across all the emails in the dataset?
which.max(colSums(emailsSparse))

# Add a variable called "spam" to emailsSparse containing the email spam labels.
emailsSparse$spam <- emails$spam

# How many word stems appear at least 5000 times in the ham emails in the dataset? 
sort(colSums(subset(emailsSparse, spam == 0)))
# -> 4

# How many word stems appear at least 1000 times in the spam emails in the dataset?
sort(colSums(subset(emailsSparse, spam == 1)))
# -> 3
# Explanation: Note that the variable "spam" is the dependent variable and is not the frequency of a word stem

# The lists of most common words are significantly different between the spam and ham emails. What does this likely imply?
# -> The frequencies of these most common words are likely to help differentiate between spam and ham. 

# Several of the most common word stems from the ham documents, such as "enron", "hou" (short for Houston), "vinc" (the word stem of "Vince") and "kaminski", are likely specific to Vincent Kaminski's inbox. What does this mean about the applicability of the text analytics models we will train for the spam filtering problem?
# -> The models we build are personalized, and would need to be further tested before use as spam filters for other.


#### Problem 3 - Building machine learning models

# First, convert the dependent variable to a factor with 
emailsSparse$spam <- as.factor(emailsSparse$spam)

# split emailsSparse 70/30 into a training set called "train" and a testing set called "test".
set.seed(123)
split <- sample.split(emailsSparse$spam, SplitRatio=0.7)
train <- subset(emailsSparse, split==T)
test <- subset(emailsSparse, split==F)

# Using the training set, train the following three machine learning models. The models should predict the dependent variable "spam", using all other available variables as independent variables. 
# Please be patient, as these models may take a few minutes to train.

# 1) A logistic regression model called spamLog. You may see a warning message here.
spamLog <- glm(spam ~ ., data=train, family="binomial")

# 2) A CART model called spamCART, using the default parameters to train the model (don't worry about adding minbucket or cp). Remember to add the argument method="class" since this is a binary classification problem.
spamCART <- rpart(spam ~ ., data=train, method="class")

# 3) A random forest model called spamRF, using the default parameters to train the model (don't worry about specifying ntree or nodesize). Directly before training the random forest model, set the random seed to 123 (even though we've already done this earlier in the problem, it's important to set the seed right before training the model so we all obtain the same results. Keep in mind though that on certain operating systems, your results might still be slightly different).
set.seed(123)
spamRF <- randomForest(spam ~ ., data=train)

# For each model, obtain the predicted spam probabilities for the training set. Be careful to obtain probabilities instead of predicted classes, because we will be using these values to compute training set AUC values. Recall that you can obtain probabilities for CART models by not passing any type parameter to the predict() function, and you can obtain probabilities from a random forest by adding the argument type="prob". For CART and random forest, you need to select the second column of the output of the predict() function, corresponding to the probability of a message being spam.
# glm
spamLogPredict <- predict(spamLog, newdata=train, type="response")

# CART
spamCARTpredict <- predict(spamCART, newdata=train)
spamCARTpredProb <- spamCARTpredict[,2]

# random Forest
spamRFpredict <- predict(spamRF, newdata=train, type="prob")
spamRFpredProb <- spamRFpredict[,2]

# You may have noticed that training the logistic regression model yielded the messages "algorithm did not converge" and "fitted probabilities numerically 0 or 1 occurred". Both of these messages often indicate overfitting and the first indicates particularly severe overfitting, often to the point that the training set observations are fit perfectly by the model. 
# Let's investigate the predicted probabilities from the logistic regression model.

# How many of the training set predicted probabilities from spamLog are less than 0.00001?
length(spamLogPredict[spamLogPredict < 0.00001])

# How many of the training set predicted probabilities from spamLog are more than 0.99999?
length(spamLogPredict[spamLogPredict > 0.99999])

# How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
length(spamLogPredict[spamLogPredict > 0.00001 & spamLogPredict < 0.99999])

# How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
summary(spamLog)
length(summary(spamLog)$coefficients[,4])[summary(spamLog)$coefficients[,4] < 0.05]

# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
prp(spamCART)
# -> 2

# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, spamLogPredict >= 0.5)
TruePositives <- 954
FalseNegatives <- 4
TrueNegatives <- 3052
FalsePositives <- 0
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(train)
# !! clearly overfitted

# What is the training set AUC of spamLog?
predROCR <- prediction(spamLogPredict, train$spam)
performance(predROCR, "auc")@y.values

# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(train$spam, spamCARTpredProb >= 0.5)
TruePositives <- 889
FalseNegatives <- 69
TrueNegatives <- 2892
FalsePositives <- 160
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(train)

# What is the training set AUC of spamCART?
predROCR <- prediction(spamCARTpredProb, train$spam)
performance(predROCR, "auc")@y.values

# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(train$spam, spamRFpredProb >= 0.5)
TruePositives <- 954
FalseNegatives <- 4
TrueNegatives <- 3051
FalsePositives <- 1
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(train)

# What is the training set AUC of spamRF?
predROCR <- prediction(spamRFpredProb, train$spam)
performance(predROCR, "auc")@y.values

# Which model had the best training set performance, in terms of accuracy and AUC?
# -> Logistic regression


#### Problem 4 - Evaluating on the Test Set

# Obtain predicted probabilities for the testing set for each of the models, again ensuring that probabilities instead of classes are obtained.
# glm
spamLogPredict <- predict(spamLog, newdata=test, type="response")

# CART
spamCARTpredict <- predict(spamCART, newdata=test)
spamCARTpredProb <- spamCARTpredict[,2]

# random Forest
spamRFpredict <- predict(spamRF, newdata=test, type="prob")
spamRFpredProb <- spamRFpredict[,2]

# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, spamLogPredict >= 0.5)
TruePositives <- 370
FalseNegatives <- 40
TrueNegatives <- 1243
FalsePositives <- 65
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# What is the testing set AUC of spamLog?
predROCR <- prediction(spamLogPredict, test$spam)
performance(predROCR, "auc")@y.values

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(test$spam, spamCARTpredProb >= 0.5)
TruePositives <- 376
FalseNegatives <- 34
TrueNegatives <- 1238
FalsePositives <- 70
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# What is the testing set AUC of spamCART?
predROCR <- prediction(spamCARTpredProb, test$spam)
performance(predROCR, "auc")@y.values

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(test$spam, spamRFpredProb >= 0.5)
TruePositives <- 384
FalseNegatives <- 26
TrueNegatives <- 1297
FalsePositives <- 11
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test)

# What is the testing set AUC of spamRF?
predROCR <- prediction(spamRFpredProb, test$spam)
performance(predROCR, "auc")@y.values

# Which model had the best testing set performance, in terms of accuracy and AUC?
# -> Random forest

# Which model demonstrated the greatest degree of overfitting?
# -> Logistic regression


##### Separating Spam from Ham (Part 2)


#### Problem 5 - Assigning weights to different types of errors

# Consider the case of an email provider using the spam filter we have developed. The email provider moves all of the emails flagged as spam to a separate "Junk Email" folder, meaning those emails are not displayed in the main inbox. The emails not flagged as spam by the algorithm are displayed in the inbox. Many of this provider's email users never check the spam folder, so they will never see emails delivered there.
# In this scenario, what is the cost associated with the model making a false negative error?
# -> A spam email will be displayed in the main inbox, a nuisance for the email user. 

# In this scenario, what is the cost associated with our model making a false positive error?
# -> A ham email will be sent to the Junk Email folder, potentially resulting in the email user never seeing that message. 

# Which sort of mistake is more costly (less desirable), assuming that the user will never check the Junk Email folder?
# -> False positive 

# What sort of user might assign a particularly high cost to a false negative result?
# -> A user who is particularly annoyed by spam email reaching their main inbox

# What sort of user might assign a particularly high cost to a false positive result?
# -> A user who never checks his/her Junk Email folder

# Consider another use case for the spam filter, in which messages labeled as spam are still delivered to the main inbox but are flagged as "potential spam." Therefore, there is no risk of the email user missing an email regardless of whether it is flagged as spam. 
# What is the largest way in which this change in spam filter design affects the costs of false negative and false positive results?
#-> The cost of false positive results is decreased

# Consider a large-scale email provider with more than 100,000 customers. Which of the following represents an approach for approximating each customer's preferences between a false positive and false negative that is both practical and personalized?
# -> Automatically collect information about how often each user accesses his/her Junk Email folder to infer preferences


#### Problem 6 - Integrating Word Count Information

# First, we will use the number of words in the each email as an independent variable. We can use the original document term matrix called dtm for this task. The document term matrix has documents (in this case, emails) as its rows, terms (in this case word stems) as its columns, and frequencies as its values. As a result, the sum of all the elements in a row of the document term matrix is equal to the number of terms present in this document. 
# Obtain the word counts for each email with the command:
wordCount <- rowSums(as.matrix(dtm))

# What would have occurred if we had instead created wordCount using spdtm instead of dtm?
# -> wordCount would have only counted some of the words, but would have returned a result for all the emails 

# Use the hist() function to plot the distribution of wordCount in the dataset. 
hist(wordCount)

# What best describes the distribution of the data?
# -> The data is skew right -- there are a large number of small wordCount values and a small number of large values.

# Now, use the hist() function to plot the distribution of log(wordCount) in the dataset. 
hist(log(wordCount))

# What best describes the distribution of the data?
# -> The data is not skewed -- there are roughly the same number of unusually large and unusually small log(wordCount) values. 

# Create a variable called logWordCount in emailsSparse that is equal to log(wordCount). 
emailsSparse$logWordCount <- log(wordCount)

# Use the boxplot() command to plot logWordCount against whether a message is spam. 
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)

# Which of the following best describes the box plot?
# -> logWordCount is slightly smaller in spam messages than in ham messages

# Because logWordCount differs between spam and ham messages, we hypothesize that it might be useful in predicting whether an email is spam. 
# Take the following steps:
# 1) Use the same sample.split output you obtained earlier (do not re-run sample.split) to split emailsSparse into a training and testing set, which you should call train2 and test2.
train2 <- subset(emailsSparse, split==T)
test2 <- subset(emailsSparse, split==F)

# 2) Use train2 to train a CART tree with the default parameters, saving the model to the variable spam2CART.
spam2CART <- rpart(spam ~ ., data=train2, method="class")

# 3) Use train2 to train a random forest with the default parameters, saving the model to the variable spam2RF. Again, set the random seed to 123 directly before training spam2RF.
set.seed(123)
spam2RF <- randomForest(spam ~ ., data=train2, method="class")

# Was the new variable used in the new CART tree spam2CART?
prp(spam2CART)
# -> Yes

# What is the test-set accuracy of spam2CART, using threshold 0.5 for predicting an email is spam?
spam2CARTpredict <- predict(spam2CART, newdata=test2)
spam2CARTprobPred <- spam2CARTpredict[,2]
table(test2$spam, spam2CARTprobPred >= 0.5)
TruePositives <- 367
FalseNegatives <- 43
TrueNegatives <- 1248
FalsePositives <- 60
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test2)
# -> should be 0.9301513

# What is the test-set AUC of spam2CART?
predROCR <- prediction(spam2CARTprobPred, test2$spam)
performance(predROCR, "auc")@y.values

# What is the test-set accuracy of spam2RF, using threshold 0.5 for predicting an email is spam? 
spam2RFpredict <- predict(spam2RF, newdata=test2, type="prob")
spam2RFprobPred <- spam2RFpredict[,2]
table(test2$spam, spam2RFprobPred >= 0.5)
TruePositives <- 387
FalseNegatives <- 23
TrueNegatives <- 1296
FalsePositives <- 12
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(test2)

# What is the test-set AUC of spam2RF?
predROCR <- prediction(spam2RFprobPred, test2$spam)
performance(predROCR, "auc")@y.values

# !! In this case, adding the logWordCounts variable did not result in improved results on the test set for the CART or random forest model.


####  Problem 7 - Using 2-grams to predict spam

# Another source of information that might be extracted from text is the frequency of various n-grams. An n-gram is a sequence of n consecutive words in the document. For instance, for the document "Text analytics rocks!", which we would preprocess to "text analyt rock", the 1-grams are "text", "analyt", and "rock", the 2-grams are "text analyt" and "analyt rock", and the only 3-gram is "text analyt rock". n-grams are order-specific, meaning the 2-grams "text analyt" and "analyt text" are considered two separate n-grams. We can see that so far our analysis has been extracting only 1-grams.
# In this last subproblem, we will add 2-grams to our predictive model. Begin by installing and loading the RTextTools package. 
#install.packages("RTextTools")
library(RTextTools)

# We can create a document term matrix containing all 2-grams in our dataset using (be patient, as this may take a few minutes):
dtm2gram <- create_matrix(as.character(corpus), ngramLength=2)

# How many terms are in dtm2gram?
dtm2gram


# It's clearly more important than ever to remove terms that appear infrequently. 
# Use removeSparseTerms to build a document term matrix spdtm2gram that contains only 2-grams appearing in at least 5% of the emails. 
spdtm2gram <- removeSparseTerms(dtm2gram, 0.95)

# How many terms are in spdtm2gram?
spdtm2gram

# spdtm and spdtm2gram contain all 1-grams and 2-grams, respectively, that appear in at least 5% of the documents in our corpus. In this case, our corpus spdtm contains many more terms than spdtm2gram. 
# Which of the following is true?
# -> For some corpus, spdtm2gram constructed in this way will contain more terms than spdtm.
# Explanation: Consider a corpus containing 6 documents, "A B C", "A C B", "B A C", "B C A", "C A B", and "C B A". Because the corpus contains only 6 documents, and 1-gram or 2-gram in the corpus must appear in more than 5% of documents, so spdtm is the set of all 1-grams and spdtm2gram is the set of all 2-grams. Therefore, spdtm contains terms "A", "B", and "C", while spdtm2gram contains terms "A B", "A C", "B A", "B C", "C A", and "C B".
# Explanation: While we can construct a corpus for which spdtm2gram has more terms than spdtm, in practice spdtm almost always has many more terms. 

# Now, let's include all the 2-grams in our spam/ham prediction models. Complete the following steps:

# 1) Build data frame emailsSparse2gram from spdtm2gram, using as.data.frame() and as.matrix().
emailsSparse2gram <- as.data.frame(as.matrix(spdtm2gram))

# 2) Convert the column names of emailsSparse2gram to valid names using make.names().
colnames(emailsSparse2gram) <- make.names(colnames(emailsSparse2gram))

# 3) Combine the original emailsSparse with emailsSparse2gram into a final data frame with the command
emailsCombined <- cbind(emailsSparse, emailsSparse2gram)

# 4) Use the same sample.split output you obtained earlier (do not re-run sample.split) to split emailsCombined into a training and testing set, which you should call trainCombined and testCombined.
trainCombined <- subset(emailsCombined, split==T)
testCombined <- subset(emailsCombined, split==F)

# 5) Use trainCombined to train a CART tree with the default parameters, saving the model to the variable spamCARTcombined.
spamCARTcombined <- rpart(spam ~ ., data=trainCombined, method="class")

# 6) Use trainCombined to train a random forest with the default parameters, saving the model to the variable spamRFcombined. 
# Again, set the random seed to 123 directly before training the random forest model.
set.seed(123)
spamRFcombined <- randomForest(spam ~ ., data=trainCombined, method="class")

# How many 2-grams were used as splits in spamCARTcombined? 
# A 2-gram is denoted by two words separated by a period or dot. 
# You can pass the "varlen=0" option to the prp() function to display full variable names instead of truncated names.
prp(spamCARTcombined, varlen=0)

# What is the test-set accuracy of spamCARTcombined, using a threshold of 0.5 for predictions?
spamCARTcombinedPredict <- predict(spamCARTcombined, newdata=testCombined)
spamCARTcombinedProdPred <- spamCARTcombinedPredict[,2]
table(testCombined$spam, spamCARTcombinedProdPred >= 0.5)
TruePositives <- 374
FalseNegatives <- 36
TrueNegatives <- 1233
FalsePositives <- 75
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(testCombined)

# What is the test-set AUC of spamCARTcombined?
predROCR <- prediction(spamCARTcombinedProdPred, testCombined$spam)
performance(predROCR, "auc")@y.values

# What is the test-set accuracy of spamRFcombined, using a threshold of 0.5 for predictions? 
spamRFcombinedPredict <- predict(spamRFcombined, newdata=testCombined, type="prob")
spamRFcombinedProdPred <- spamRFcombinedPredict[,2]
table(testCombined$spam, spamRFcombinedProdPred >= 0.5)
TruePositives <- 384
FalseNegatives <- 26
TrueNegatives <- 1296
FalsePositives <- 12
# Overall Accuracy = (True Negatives + True Positives) / N -> terms in the diagonal of confusion matrix divided by N
(TrueNegatives + TruePositives) / nrow(testCombined)

# What is the test-set AUC of spamRFcombined?
predROCR <- prediction(spamRFcombinedProdPred, testCombined$spam)
performance(predROCR, "auc")@y.values

# !! For this problem, adding 2-grams did not dramatically improve our test-set performance. Adding n-grams is most effective in large datasets. Given the billions of emails sent each day, it's reasonable to expect that email providers would be able to construct datasets large enough for n-grams to provide useful predictive power.


######## Week 6

###### Recommendations Worth a Million: An Introduction to Clustering

#### Video 6: Getting the Data

movies <- read.table("movieLens.txt", header=F, sep="|", quote="\"")
str(movies)

# add headings
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

# remove unnecessary columns
movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL

# remove duplicates
movies <- unique(movies)
str(movies)

# Quick Question 6
# How many movies are classified as comedies?
table(movies$Comedy)

# How many movies are classified as westerns?
table(movies$Western)

# How many movies are classified as romance AND drama
table(movies$Romance, movies$Drama)


#### Video 7: Hierarchical Clustering in R

# compute distances (for columns 2-20)
distances <- dist(movies[2:20], method="euclidean")
clusterMovies <- hclust(distances, method="ward")

# plot dondogram
plot(clusterMovies)

# how many clusters to pick? Looks like a nice spot where there are 10 clusters.
clusterGroups <- cutree(clusterMovies, k = 10)

# compute % of movies in each genre and cluster
# by computing average, we compute percentage of movies that fall into that genre
tapply(movies$Action, clusterGroups, FUN=mean)
# in cluster 2 for example 78% of movies are action movies

tapply(movies$Romance, clusterGroups, FUN=mean)
# create table to analyze clusters -> missing here since Excel sheet

# Look at Men in Black
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 <- subset(movies,clusterGroups==2)

# good movies to recommend to s.o. who likes MIB
cluster2$Title[1:10]


## An Advanced Approach to Finding Cluster Centroids

rm(split) 

# In this video, we explain how you can find the cluster centroids by using the function "tapply" for 
# each variable in the dataset. While this approach works and is familiar to us, it can be a little 
# tedious when there are a lot of variables. An alternative approach is to use the colMeans function. 
# With this approach, you only have one command for each cluster instead of one command for each 
# variable. If you run the following command in your R console, you can get all of the column 
# (variable) means for cluster 1:
colMeans(subset(movies[2:20], clusterGroups == 1))

# You can repeat this for each cluster by changing the clusterGroups number. However, if you also have a lot of clusters, this approach 
# is not that much more efficient than just using the tapply function.

# A more advanced approach uses the "split" and "lapply" functions. The following command will 
# split the data into subsets based on the clusters:
spl <- split(movies[2:20], clusterGroups)

# Then you can use spl to access the different clusters, because
spl[[1]]
# is the same as
subset(movies[2:20], clusterGroups == 1)

# so colMeans(spl[[1]]) will output the centroid of cluster 1. But an even easier approach uses 
# the lapply function. The following command will output the cluster centroids for all clusters:
lapply(spl, colMeans)

# The lapply function runs the second argument (colMeans) on each element of the first argument 
# (each cluster subset in spl). So instead of using 19 tapply commands, or 10 colMeans commands, 
# we can output our centroids with just two commands: one to define spl, and then the lapply command.

# Quick Question 7

# Run the cutree function again to create the cluster groups, but this time pick k = 2 clusters. 
# It turns out that the algorithm groups all of the movies that only belong to one specific genre 
# in one cluster (cluster 2), and puts all of the other movies in the other cluster (cluster 1). 

# What is the genre that all of the movies in cluster 2 belong to?
clusterGroups <- cutree(clusterMovies, k = 2)
spl <- split(movies[2:20], clusterGroups)
lapply(spl, colMeans)


###### Recitation: Seeing the Big Picture: Segmenting Images to Create Data

#### Video 2: Clustering Pixels

flower <- read.csv("flower.csv", header=F)
str(flower)

# change to intensity matrix
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

# create intensity vector (make sure first to convert to matrix before converting to vector)
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

# distance matrix (pairwise distances between all intensity values in flower vector)
distance <- dist(flowerVector, method="euclidean")


#### Video 3: Hierarchical Clustering

# cluster intensity values using hierarchical clustering
# ward is minimum variance method which tries to find compact and spherity clusters
# think of it as minimizing variance in each cluster and the distance among clusters
clusterIntensity <- hclust(distance, method="ward")

# plot cluster dendogram
plot(clusterIntensity)

# visualize cuts by plotting rectangles around clusters on the tree
rect.hclust(clusterIntensity, k=3, border="red")

# split data into the 3 clusters
flowerClusters <- cutree(clusterIntensity, k=3)

# a vector that assigns each intensity value in flower vector to a cluster
flowerClusters

# find mean intensity value for each cluster
tapply(flowerVector, flowerClusters, mean)
# first vector has value closest to zero which corresponds to very dark (zero being black and 1 being white)

# see how image was segmented
# convert flower vector into matrix
dim(flowerClusters) <- c(50,50)
image(flowerClusters, axes=F)
image(flowerMatrix, axes=F, col=grey(seq(0,1,length=256)))


#### Video 4: MRI Image

healthy <- read.csv("healthy.csv", header=F)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
# 566x646 resolution

# see MRI image
image(healthyMatrix, axes=F, col=grey(seq(0,1,length=256)))

# create intensity vector
healthyVector <- as.vector(healthyMatrix)

# compute distances
# !! created vector would be too large (around 498 GB)
#distance <- dist(healthyVector, method="euclidean")
# The healthy vector has 365,636 elements. Let's call this number n.
# And remember that for R to calculate the pairwise distances,
# it would actually need to calculate n*(n-1)/2 and then
# store them in the distance matrix.
n <- 365636
n*(n-1)/2
# 67 billion values -> WE CANNOT USE HIERARCHICAL CLUSTERING


#### Video 5: K-Means Clustering

k <- 5
set.seed(1)

# k-means clustering algorithm
# number of clusters = 5, number of iterations maximum of 1000
KMC <- kmeans(healthyVector, centers=k, iter.max=1000)
str(KMC)

# extract cluster vector from KMC
healthyClusters <- KMC$cluster

# mean intensity values for the five clusters also taken from KMC
KMC$centers

# cluster sizes
KMC$size

dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
# number of colors is number of clusters
image(healthyClusters, axes=F, col=rainbow(k))


#### Video 6: Detecting Tumors

tumor <- read.csv("tumor.csv", header=F)
tumorMatrix <- as.matrix(tumor)
tumorMatrix <- t(tumorMatrix)[,nrow(tumorMatrix):1]
tumorVector <- as.vector(tumorMatrix)

# apply k-means clusters from healthy patient to sick patient 
# In other words, we treat the healthy vector as training set and the tumor vector as a testing set.
#install.packages("flexclust")
library(flexclust)

# We need to convert the information from the clustering algorithm to an object of the class KCCA.
# This conversion is needed before we can use the predict function on the test set tumorVector.
KMC.kcca <- as.kcca(KMC, healthyVector)

# cluster the pixels in the tumorVector using the predict function
tumorClusters <- predict(KMC.kcca, newdata=tumorVector)
# now, the tumorClusters is a vector that assigns a value 1 through 5 to each of the intensity
# values in the tumorVector, as predicted by the k-means algorithm

# convert tumor clusters to matrix
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))

# look at image
image(tumorClusters, axes=F, col=rainbow(k))


###### Week 6 Assignment

##### Document clustering with daily kos

# The variable "Document" gives an identifying number to each document. 
# Each of the other variables in the dataset is a word that has appeared 
# in at least 50 different articles (1,545 words in total). The set of  
# words has been trimmed according to the techniques covered in the previous 
# week on text analytics (punctuation has been removed, stop words have been 
# removed, and the words have been stemmed). For each document, the variable 
# values are the number of times that word appeared in the document.

# First, read the data set into R.
kos <- read.csv("dailykos.csv")
str(kos)


#### Problem 1 - Hierarchical Clustering

# Then, compute the distances (using method="euclidean"), 
distance <- dist(kos[2:length(kos)], method="euclidean")

# and use hclust to build the model (using method="ward"). 
# You should cluster on all of the variables EXCEPT the "Document" variable
clusterKos <- hclust(distance, method="ward")

# Running the dist function will probably take you a while. Why? Select all that apply.
# -> We have a lot of observations, so it takes a long time to compute the distance between each pair of observations
# -> We have a lot of variables, so the distance computation is long

# Plot the dendrogram of your hierarchical clustering model. Just looking at the dendrogram, 
# which of the following seem like good choices for the number of clusters? 
plot(clusterKos)
# -> 2
# -> 3

# In this problem, we are trying to cluster news articles or blog posts into groups. This can be used to show readers categories to choose from when trying to decide what to read. 
# Just thinking about this application, what are good choices for the number of clusters?
# -> 7, 8

# Let's pick 7 clusters. This number is reasonable according to the dendrogram, and also seems reasonable for the application. 
# Use the cutree function to split your data into 7 clusters.
clusterGroups <- cutree(clusterKos, k = 7)

# Let's instead use the subset function to subset our data by cluster. 
# Create 7 new datasets, each containing the observations from one of the clusters. 
spl <- split(kos[2:length(kos)], clusterGroups)
str(spl)
# How many observations are in cluster 3?
nrow(spl[[3]])

# Which cluster has the most observations?
which.max(lapply(spl, nrow))

# Which cluster has the fewest observations?
which.min(lapply(spl, nrow))

# Instead of looking at the average value in each variable individually, 
# we'll just look at the top 6 words in each cluster. To do this for cluster 1, 
# type the following (where "HierCluster1" should be replaced with the name of your first cluster subset):
tail(sort(colMeans(spl[[1]][-1])))

# What is the most frequent word in this cluster, in terms of average value? 
tail(sort(colMeans(spl[[1]][-1])))

# Now repeat the command given in the previous problem for each of the other clusters, 
# and answer the following questions.
for (i in spl) {
    print(tail(sort(colMeans(i[-1]))))
}

# Which words best describe cluster 2?
# -> november, poll, vote, challenge

# Which cluster could best be described as the cluster related to the Iraq war?
# -> 5

# In 2004, one of the candidates for the Democratic nomination for the President of the United States was Howard Dean, John Kerry was the candidate who won the democratic nomination, and John Edwards with the running mate of John Kerry (the Vice President nominee). 
# Given this information, which cluster best corresponds to the democratic party?
# -> 7


####  Problem 2 - K-Means Clustering

# Now, run k-means clustering, setting the seed to 1000 right before you run the kmeans function. 
# Again, pick the number of clusters equal to 7. You don't need to add the iters.max argument. 
# Don't forget to exclude the "Document" variable from your clustering.
set.seed(1000)
k <- 7
KMC <- kmeans(kos[2:length(kos)], centers=k)
str(KMC)

# Subset your data into the 7 clusters (7 new datasets) by using the "cluster" variable of your kmeans output.
spl <- split(kos, KMC$cluster)
str(spl)

# How many observations are in Cluster 3?
nrow(spl[[3]])

# Which cluster has the most observations?
which.max(lapply(spl, nrow))

# Which cluster has the fewest number of observations?
which.min(lapply(spl, nrow))

# Now, output the six most frequent words in each cluster, like we did in the previous problem, for each of the k-means clusters.
for (i in spl) {
    print(tail(sort(colMeans(i[-1]))))
}

# Which k-means cluster best corresponds to the Iraq War?
# -> 3

# Which k-means cluster best corresponds to the democratic party?
# -> 3

# For the rest of this problem, we'll ask you to compare how observations were assigned to clusters in the two different methods. Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
# Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
table(clusterGroups, KMC$cluster)
prop.table(table(clusterGroups, KMC$cluster), margin=2)
# -> Hierarchical Cluster 7 (80.6%)

# Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
prop.table(table(clusterGroups, KMC$cluster), margin=2)
# -> Hierarchical Cluster 5 (61.7%)

# Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
prop.table(table(clusterGroups, KMC$cluster), margin=2)
# -> No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7.

# Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
prop.table(table(clusterGroups, KMC$cluster), margin=2)
# -> Hierarchical Cluster 5 (97.3%)


##### Market Segmentation For Airlines

#    Balance = number of miles eligible for award travel
#    QualMiles = number of miles qualifying for TopFlight status
#    BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
#    BonusTrans = number of non-flight bonus transactions in the past 12 months
#    FlightMiles = number of flight miles in the past 12 months
#    FlightTrans = number of flight transactions in the past 12 months
#    DaysSinceEnroll = number of days since enrolled in the frequent flyer program

# Read the dataset AirlinesCluster.csv into R and call it "airlines". 
airlines <- read.csv("AirlinesCluster.csv")
str(airlines)


#### Problem 1 - Normalizing the Data

# Looking at the summary of airlines, which two variables have (on average) the smallest values? 
summary(airlines)

# Why is it important to normalize the data before clustering?
# -> If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale. 

# You can normalize the variables in a data frame by using the preProcess function in the "caret" package.
library(caret)

# Now, create a normalized data frame called "airlinesNorm" by running the following commands:
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
# The first command pre-processes the data, and the second command performs the normalization. If you look at the summary of airlinesNorm, you should see that all of the variables now have mean zero. You can also see that each of the variables has standard deviation 1 by using the sd() function.

# In the normalized data, which variable has the largest maximum value?
# In the normalized data, which variable has the smallest minimum value?
summary(airlinesNorm)


#### Problem 2 - Hierarchical Clustering

# Compute the distances between data points (using euclidean distance) and then run the Hierarchical clustering algorithm (using method="ward") on the normalized data
distance <- dist(airlinesNorm, method="euclidean")
clusterAirline <- hclust(distance, method="ward")

# Then, plot the dendrogram of the hierarchical clustering process. 
plot(clusterAirline)

# Suppose the airline is looking for somewhere between 2 and 10 clusters. 
# According to the dendrogram, which of the following is NOT a good choice for the number of clusters?
# -> 6

# Divide the data points into 5 clusters by using the cutree function. 
clusterGroups <- cutree(clusterAirline, k=5)

# How many data points are in Cluster 1?
table(clusterGroups)

# Now, use tapply to compare the average values in each of the variables for the 5 clusters (the centroids of the clusters). 
# You may want to compute the average values of the unnormalized data so that it is easier to interpret.
str(airlines)
for (i in airlines) {
    print(tapply(i, clusterGroups, mean))
}

# Compared to the other clusters, Cluster 1 has the largest average values in which variables (if any)?
# -> DaysSinceEnroll

# How would you describe the customers in Cluster 1?
# -> Infrequent but loyal customers. 

# Compared to the other clusters, Cluster 2 has the largest average values in which variables (if any)?
# -> QualMiles, FlightMiles, FlightTrans

# How would you describe the customers in Cluster 2?
# -> Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions. 

# Compared to the other clusters, Cluster 3 has the largest average values in which variables (if any)?
# -> Balance, BonusTrans

# How would you describe the customers in Cluster 3?
# -> Customers who have accumulated a large amount of miles, mostly through non-flight transactions.

# Compared to the other clusters, Cluster 4 has the largest average values in which variables (if any)?
# -> none

# How would you describe the customers in Cluster 4?
# -> Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions. 

# Compared to the other clusters, Cluster 5 has the largest average values in which variables (if any)?
# -> none

# How would you describe the customers in Cluster 5?
# -> Relatively new customers who don't use the airline very often.


#### Problem 3 - K-Means Clustering

# Now run the k-means clustering algorithm on the normalized data, again creating 5 clusters. 
# Set the seed to 88 right before running the clustering algorithm, and set the argument iter.max to 1000. 
set.seed(88)
k <- 5
KMC <- kmeans(airlinesNorm, centers=k)
str(KMC)

# How many clusters have more than 1,000 observations?
table(KMC$cluster)

# Now, compare the cluster centroids to each other
KMC$centers

# Do you expect Cluster 1 of the K-Means clustering output to necessarily be similar to Cluster 1 of the Hierarchical clustering output?
# -> No, because cluster ordering is not meaningful in either k-means clustering or hierarchical clustering.


##### Predicting Medical Costs with Cluster-Then-Predict

# The dependent variable, reimbursement2009, represents the total value of all Medicare reimbursements for a patient in 2009, which is the cost of the patient's care to the Medicare system. 
# The following independent variables are available:

#    age: The patient's age in years at the beginning of 2009
#    alzheimers: Binary variable for whether the patient had diagnosis codes for Alzheimer's disease or a related disorder in 2008
#    arthritis: Binary variable for whether the patient had diagnosis codes for rheumatoid arthritis or osteoarthritis in 2008
#    cancer: Binary variable for whether the patient had diagnosis codes for cancer in 2008
#    copd: Binary variable for whether the patient had diagnosis codes for Chronic Obstructive Pulmonary Disease (COPD) in 2008
#    depression: Binary variable for whether the patient had diagnosis codes for depression in 2008
#    diabetes: Binary variable for whether the patient had diagnosis codes for diabetes in 2008
#    heart.failure: Binary variable for whether the patient had diagnosis codes for heart failure in 2008
#    ihd: Binary variable for whether the patient had diagnosis codes for ischemic heart disease (IHD) in 2008
#    kidney: Binary variable for whether the patient had diagnosis codes for chronic kidney disease in 2008
#    osteoporosis: Binary variable for whether the patient had diagnosis codes for osteoporosis in 2008
#    stroke: Binary variable for whether the patient had diagnosis codes for a stroke/transient ischemic attack (TIA) in 2008
#    reimbursement2008: The total amount of Medicare reimbursements for this patient for 2008


#### Problem 1 - Preparing the Dataset

# Load reimbursement.csv into a data frame called claims.
claims <- read.csv("reimbursement.csv")

# How many Medicare beneficiaries are included in the dataset?
str(claims)

# What proportion of patients have at least one of the chronic conditions described in the independent variables alzheimers, arthritis, cancer, copd, depression, diabetes, heart.failure, ihd, kidney, osteoporosis, and stroke?
sum(rowSums(claims[, 2:12], na.rm=T) !=0) / nrow(claims)
# Alternatively (slow but can be generalized easy)
#sum(apply(claims[, 2:12], MARGIN = 1, FUN = function(x) all(x != 0))) / nrow(claims)
# what you can also do with that:
#example[!apply(example[, -1], MARGIN = 1, FUN = function(x) all(x == 0)), ]
# Alternatively
NumConditions <- rowSums(claims[,2:12])
mean(NumConditions > 0)

# What is the maximum correlation between independent variables in the dataset?
max(cor(claims[1:ncol(claims)-1])[cor(claims[1:ncol(claims)-1])!=1])
# Alternatively remove diagonal
corMatrix <- cor(claims[1:ncol(claims)-1])
diag(corMatrix) <- NA
max(corMatrix, na.rm=T)

# Plot the histogram of the dependent variable. What is the shape of the distribution?
hist(claims$reimbursement2009)
# -> Skew right -- there are a large number of observations with a small value, but only a small number of observations with a large value.

# To address the shape of the data identified in the previous problem, we will log transform the two reimbursement variables with the following code:
claims$reimbursement2008 <- log(claims$reimbursement2008+1)
claims$reimbursement2009 <- log(claims$reimbursement2009+1)

# Why did we take the log of the reimbursement value plus 1 instead of the log of the reimbursement 
# value? Hint -- What happens when a patient has a reimbursement cost of $0?
# -> To avoid log-transformed values of infinity
# Explanation: Note that log(1) = 0 so values with one who are actually zero become zero again

# Plot the histogram of the log-transformed dependent variable. The distribution is reasonably balanced, other than a large number of people with variable value 0, corresponding to having had $0 in reimbursements in 2009.
hist(claims$reimbursement2009)

# What proportion of beneficiaries had $0 in reimbursements in 2009?
length(claims$reimbursement2009[claims$reimbursement2009==0]) / nrow(claims)


#### Problem 2 - Initial Linear Regression Model

# Run the following commands to randomly select 70% of the data for the training set and 30% of the data for the testing set:
set.seed(144)
spl <- sample(1:nrow(claims), size=0.7*nrow(claims))
train <- claims[spl,]
test <- claims[-spl,]

# Use the train data frame to train a linear regression model (name it lm.claims) to predict reimbursement2009 using all the independent variables.
lm.claims <- lm(reimbursement2009 ~ ., data=claims)

# What is the training set Multiple R-squared value of lm.claims?
summary(lm.claims)

# Obtain testing set predictions from lm.claims. 
lm.claims.predict <- predict(lm.claims, newdata=test)

# What is the testing set RMSE of the model?
SSE <- sum((lm.claims.predict - test$reimbursement2009)^2)
RMSE <- sqrt(SSE/nrow(test))
RMSE
# Alternative
pred.test <- predict(lm.claims, newdata=test)
rmse.lm <- sqrt(mean((pred.test - test$reimbursement2009)^2)) 
rmse.lm

# What is the "naive baseline model" that we would typically use to compute the R-squared value of lm.claims?
# -> Predict mean(train$reimbursement2009) for every observation
# Explanation: The naive baseline predicts the average of the dependent variable (reimbursement2009) on the training set. Just like our models, the naive baseline is not allowed to learn from the testing set, so it's not allowed to predict mean(test$reimbursement2009). 

# What is the testing set RMSE of the naive baseline model?
baseline.pred <- mean(train$reimbursement2009)
RMSE <- sqrt(mean((baseline.pred - test$reimbursement2009)^2)) 
RMSE

# In Week 4, we saw how D2Hawkeye used a "smart baseline model" that predicted that a patient's medical costs would be equal to their costs in the previous year. For our problem, this baseline would predict reimbursement2009 to be equal to reimbursement2008.
RMSE <- sqrt(mean((test$reimbursement2008 - test$reimbursement2009)^2)) 

# What is the testing set RMSE of this smart baseline model?
RMSE


#### Problem 3 - Clustering Medicare Beneficiaries

# In this section, we will cluster the Medicare beneficiaries. The first step in this process is to remove the dependent variable using the following commands:
train.limited <- train
train.limited$reimbursement2009 <- NULL
test.limited <- test
test.limited$reimbursement2009 <- NULL

# Why do we need to remove the dependent variable in the clustering phase of the cluster-then-predict methodology?
# -> Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology
# Explnation: This is an important point that is sometimes mistakenly overlooked. If you use the outcome value to cluster, you might conclude your method strongly outperforms a non-clustering alternative. However, this is because it is using the outcome to determine the clusters, which is not valid. 

# In cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation of the variables in the training set. We can do this by passing just the training set to the preProcess function:
library(caret)
preproc <- preProcess(train.limited)
train.norm <- predict(preproc, train.limited)
test.norm <- predict(preproc, test.limited)

# What is the mean of the arthritis variable in train.norm?
mean(train.norm$arthritis)

# What is the mean of the arthritis variable in test.norm?
mean(test.norm$arthritis)

# Why is the mean arthritis variable much closer to 0 in train.norm than in test.norm?
# -> The distribution of the arthritis variable is different in the training and testing set

# Set the random seed to 144. Run k-means clustering with 3 clusters on train.norm, storing the result in an object called km.
set.seed(144)
k <- 3
km <- kmeans(train.norm, centers=k)

# The description "older-than-average beneficiaries with below average incidence of stroke and above-average 2008 reimbursements" uniquely describes which cluster center?
km$centers

# Recall from the recitation that we can use the flexclust package to obtain training set and testing set cluster assignments for our observations
library(flexclust)
km.kcca <- as.kcca(km, train.norm)
cluster.train <- predict(km.kcca)
cluster.test <- predict(km.kcca, newdata=test.norm)

# How many test-set observations were assigned to Cluster 2?
table(cluster.test)


####  Problem 4.1 - Cluster-specific predictions

# Using the subset function, build data frames train1, train2, and train3, containing the elements in the train data frame assigned to clusters 1, 2, and 3, respectively (be careful to take subsets of train, not of train.norm). Similarly build test1, test2, and test3 from the test data frame.
train1 <- subset(train, cluster.train==1)
train2 <- subset(train, cluster.train==2)
train3 <- subset(train, cluster.train==3)
test1 <- subset(test, cluster.test==1)
test2 <- subset(test, cluster.test==2)
test3 <- subset(test, cluster.test==3)

# Which training set data frame has the highest average value of the dependent variable?
mean(train1$reimbursement2009)
mean(train2$reimbursement2009)
mean(train3$reimbursement2009)

# Build linear regression models lm1, lm2, and lm3, which predict reimbursement2009 using all the variables. 
lm1 <- lm(reimbursement2009 ~ ., data=train1)
lm2 <- lm(reimbursement2009 ~ ., data=train2)
lm3 <- lm(reimbursement2009 ~ ., data=train3)

# Which variables have a positive sign for the coefficient in at least one of lm1, lm2, and lm3 and a negative sign for the coefficient in at least one of lm1, lm2, and lm3?
summary(lm1)
summary(lm2)
summary(lm3)
# -> age

# Using lm1, make test-set predictions called pred.test1 on data frame test1 etc.
pred.test1 <- predict(lm1, newdata=test1)
pred.test2 <- predict(lm2, newdata=test2)
pred.test3 <- predict(lm3, newdata=test3)

# Which vector of test-set predictions has the smallest average predicted reimbursement amount?
mean(pred.test1)
mean(pred.test2)
mean(pred.test3)

# Obtain the test-set RMSE for each cluster. Which cluster has the largest test-set RMSE?
SSE1 <- sum((pred.test1 - test1$reimbursement2009)^2)
RMSE1 <- sqrt(SSE1/nrow(test1))
RMSE1
SSE2 <- sum((pred.test2 - test2$reimbursement2009)^2)
RMSE2 <- sqrt(SSE2/nrow(test2))
RMSE2
SSE3 <- sum((pred.test3 - test3$reimbursement2009)^2)
RMSE3 <- sqrt(SSE3/nrow(test3))
RMSE3

# To compute the overall test-set RMSE of the cluster-then-predict approach, we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:
all.predictions <- c(pred.test1, pred.test2, pred.test3)
all.outcomes <- c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)

# What is the test-set RMSE of the cluster-then-predict approach?
SSE <- sum((all.predictions - all.outcomes)^2)
RMSE <- sqrt(SSE/length(all.outcomes))
RMSE

# !! Conclusion: We see a modest improvement over the original linear regression model, which is typical in situations where the observations do not cluster strongly into different "types" of observations. However, it is often a good idea to try the cluster-then-predict approach on datasets with a large number of observations to see if you can improve the accuracy of your model.


######## KAGGLE COMPETITION

## Features

# UserID - an anonymous id unique to a given user
# YOB - the year of birth of the user
# Gender - the gender of the user, either Male, Female, or not provided
# Income - the household income of the user. Either not provided, or one of "under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", or "over $150,000".
# HouseholdStatus - the household status of the user. Either not provided, or one of "Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", or "Single (w/kids)".
# EducationLevel - the education level of the user. Either not provided, or one of "Current K-12", "High School Diploma", "Current Undergraduate", "Associate's Degree", "Bachelor's Degree", "Master's Degree", or "Doctoral Degree".
# Party - the political party of the user. Either not provided, or one of "Democrat", "Republican", "Independent", "Libertarian", or "Other".
# Happy - a binary variable, with value 1 if the user said they were happy, and with value 0 if the user said that were neutral or not happy. This is the variable you are trying to predict.
# Q124742, Q124122, . . . , Q96024 - 101 different questions that the users were asked on Show of Hands. If the user didn't answer the question, there is a blank. For information about the question text and possible answers, see the file Questions.pdf.
# votes - the total number of questions that the user responded to, out of the 101 questions included in the data set (this count does not include the happiness question).


#### Load & look at data

# load data and produce NAs
happy <- read.csv("train.csv", stringsAsFactors = FALSE, na.strings=c("",".","NA"))
happy2 <- read.csv("test.csv")

# overview of data
str(happy, list.len = ncol(happy))
summary(happy)
head(happy)
tail(happy)


## Look at graphical representation

boxplot(happy$YOB)
plot(happy$Income)
plot(happy$YOB, happy$Income)
boxplot(happy$YOB ~ happy$Happy)
barplot(table(happy$Happy))
barplot(table(happy$Gender))
hist(happy$YOB)

## Look at frequencies

prop.table(table(happy$Happy))
# -> baseline says happy is more likely
table(happy$YOB)
# -> Some YOB seem unreasonable
table(happy$Income)
prop.table(table(happy$Gender))
# -> slightly more male obs
prop.table(table(happy$Happy, happy$Gender), margin=1)
# -> Gender in line with baseline
prop.table(table(happy$Income, happy$Happy), margin=1)
# -> It seems income < 50k happiness is contrary to baseline
prop.table(table(happy$HouseholdStatus, happy$Happy), margin=1)
# -> Domestic Partners (w/kids) & Single (w/kids) contrary to baseline
prop.table(table(happy$EducationLevel, happy$Happy), margin=1)
# -> Current Undergraduate contrary to baseline
prop.table(table(happy$Party, happy$Happy), margin=1)
# -> Nothing contrary to baseline



#### Data Cleaning & Transformation

## Transform questions to binary responses

# load questions in CS-friendly format removing blanks in responses
questions <- read.csv("Questions.csv", strip.white=TRUE)
str(questions)
head(questions)

# Replace answers with boolean
positive <- unique(questions$Positive)
negative <- unique(questions$Negative)
A <- function(x) ifelse(x %in% positive, 1, ifelse(x %in% negative, 0, x))
replaceResponses <- apply(happy[10:ncol(happy)-1],2,A)
temp <- apply(replaceResponses,2,as.numeric)
temp2 <- as.data.frame(temp)
happy[10:ncol(happy)-1] <- temp2

## Transform remaining variables
str(happy, list.len = ncol(happy))

# YOB
unique(happy$YOB)
happy$YOB <- as.integer(happy$YOB)

# Gender (Male=1)
unique(happy$Gender)
Gender <- ifelse(happy$Gender == "Male", 1, ifelse(happy$Gender == "Female", 0, happy$Gender))
happy$Gender <- as.numeric(Gender)

# Income
unique(happy$Income)
happy$Income <- as.factor(happy$Income)

# HouseholdStatus
unique(happy$HouseholdStatus)
happy$HouseholdStatus <- as.factor(happy$HouseholdStatus)

# EducationLevel
unique(happy$EducationLevel)
happy$EducationLevel <- as.factor(happy$EducationLevel)

# Party
unique(happy$Party)
happy$Party <- as.factor(happy$Party)


#### Imputation

## Automatic Imputation with Mice
# http://cran.r-project.org/web/packages/mice/mice.pdf

library(mice)
set.seed(123)
vars.for.imputation <- setdiff(names(happy), c("Happy", "UserID"))
vars.for.imputation
imputed <- complete(mice(happy[vars.for.imputation], m=5))

happyImputed <- as.data.frame(cbind(happy$Happy, imputed))
colnames(happyImputed)[1] <- "Happy"
str(happyImputed, list.len = ncol(happyImputed))
summary(happyImputed)
#write.csv(happyImputed, "happyImputed.csv", row.names=FALSE)

happyNonImputed <- happy[, c(8,2:7,9:ncol(happy))]
str(happyNonImputed, list.len = ncol(happyNonImputed))
summary(happyNonImputed)

# Imputation for unchanged data set i.e. no NA
str(happy2, list.len = ncol(happy2))
vars.for.imputation <- setdiff(names(happy2), c("Happy"))
#vars.for.imputation <- names(happy2)
Imputed2 <- complete(mice(happy2[vars.for.imputation], m=5))
happy2Imputed <- as.data.frame(cbind(happy2$Happy, Imputed2))
#happy2Imputed <- Imputed2
colnames(happy2Imputed)[1] <- "Happy"


# Create backUp for cleaned happy data set
happyBackUp <- happy
str(happyBackUp, list.len = ncol(happyBackUp))


# assign imputed or non-imputed data set to happy to check model performance difference
# !!!!!!!!!
happy <- happy2Imputed
str(happy, list.len = ncol(happy))

#### Create new intuitive variables

# variable that gives percentage of answered questions
#happy$QuestionsAnswered <- rowSums(!is.na(happy[10:ncol(happy)-1])) / ncol(happy[10:ncol(happy)-1])

# Low income
unique(happy$Income)
lowIncome <- c("$25,001 - $50,000", "under $25,000")
happy$LowIncome <- happy$Income %in% lowIncome

# Presence of kids
unique(happy$HouseholdStatus)
kids <- c("Married (w/kids)", "Domestic Partners (w/kids)", "Single (w/kids)")
happy$Kids <- happy$HouseholdStatus %in% kids


#### Check correlations

# happy with all other "numeric" variables (note dependent variable must be first column)
cor(happy[c(1,8:ncol(happy))],use="pairwise.complete.obs")[1,]

# -> some variables have a correlation of > |15%|

#### Deletion of variables

happy$Gender <- NULL
happy$Income <- NULL
happy$HouseholdStatus <- NULL
happy$Party <- NULL
happy$votes <- NULL
happy$UserID <- NULL
happy$votes <- NULL

#### Split data again into training and testing
library(caTools)
set.seed(123)
split <- sample.split(happy$Happy, SplitRatio=0.7)
train <- subset(happy, split==T)
test <- subset(happy, split==F)


#### Logistic Regression with cross-validation

library(boot)

happyLog1 <- glm(Happy ~ ., family=binomial, data=train)
summary(happyLog1)

# extract significant coefficients
significant1 <- data.frame(summary(happyLog1)$coef[summary(happyLog1)$coef[,4] <= .05, 4])
rownames(significant1)

# cross-validation (K = number of validation sets)
cv.err <- cv.glm(train, happyLog1, K=10)$delta
cv.err

# happyLog2 for Imputed
happyLog2 <- glm(Happy ~ EducationLevel + Q121011 + Q120014 +       
                 Q119334 + Q119851 + Q118237 + Q116441 + Q116197 +       
                 Q115610 + Q108855 + Q107869 + Q102687 + Q102289 +      
                 Q101162 + Q100689 + Q99716 + Q98869,
                 family=binomial, data=train)
# -> All variables significant on 0.05 level
# -> Use significance level of 0.01 to further simplify model
significant2 <- data.frame(summary(happyLog2)$coef[summary(happyLog2)$coef[,4] <= .01, 4])
rownames(significant2)

# happyLog2 for NonImputed
# happyLog2 <- glm(Happy ~ EducationLevel + Q124742 + Q120194 +
#                    Q119650 + Q118237 + Q116441 + Q116197 + 
#                    Q114386 + Q113992 + Q113584 + Q112512 + 
#                    Q111848 + Q108342 + Q108343 + Q101162 + 
#                    Q100680 + Q99480 + Q98197 + Kids, 
#                  family=binomial, data=train)

# happy2Log2
happyLog2 <- glm(Happy ~ Gender + HouseholdStatus + Q122769 +  
                   Q122769 + Q120194 + Q120194 + Q119334 + 
                   Q118237 + Q116441 + Q115777 + Q115777 + 
                   Q111220 + Q111220 + Q110740 + Q110740 + 
                   Q107869 + Q107491 + Q102289 + Q102089 + 
                   Q101162 + Q98869,
                 family=binomial, data=train)

summary(happyLog2)

# extract significant coefficients
significant2 <- data.frame(summary(happyLog2)$coef[summary(happyLog2)$coef[,4] <= .05, 4])
rownames(significant2)

# cross-validation (K = number of validation sets)
cv.err <- cv.glm(train, happyLog2, K=10)$delta
cv.err


# happyLog3 for Imputed
happyLog3 <- glm(Happy ~ Q119334 + Q118237 + Q116441 + Q115610 + Q108855 +     
                   Q107869 + Q102289 + Q101162 + Q100689 + Q99716 + Q98869,  
                   family=binomial, data=train)

# happyLog3 for NonImputed
# happyLog3 <- glm(Happy ~ EducationLevel + 
#                    Q118237 + Q116197 + Q113584 + 
#                    Q111848 + Q108342 + Q101162 + Q98197 + 
#                    Kids, family=binomial, data=train)

# happy2Log3
happyLog3 <- glm(Happy ~ HouseholdStatus + Q122769 +
                    Q122769 + Q119334 + Q118237 + Q118237 +       
                    Q116441 + Q115777 + Q111220 + Q110740 +       
                    Q110740 + Q107869 + Q102289 + Q102089 +       
                    Q101162 + Q101162 + Q98869 + Q98869,
                  family=binomial, data=train)

summary(happyLog3)

# extract significant coefficients
significant3 <- data.frame(summary(happyLog3)$coef[summary(happyLog3)$coef[,4] <= .05, 4])
rownames(significant3)

# cross-validation (K = number of validation sets)
cv.err <- cv.glm(train, happyLog3, K=10)$delta
cv.err


# happyLog4 for Imputed
# happyLog4 <- glm(Happy ~ EducationLevel + Q118237 + Q116197 + 
#                    Q113584 + Q111848 + Q108342 + Q101162 +   
#                    Kids, family=binomial, data=train)

# happyLog4 for NonImputed
happyLog4 <- glm(Happy ~ EducationLevel + 
                   Q118237 + Q116197 + Q113584 + Q111848 + 
                   Q108342 + Q101162 +
                   Kids, family=binomial, data=train)
summary(happyLog4)

# extract significant coefficients
significant4 <- data.frame(summary(happyLog4)$coef[summary(happyLog4)$coef[,4] <= .05, 4])
rownames(significant4)

# cross-validation (K = number of validation sets)
cv.err <- cv.glm(na.omit(train), happyLog4, K=10)$delta
cv.err


# happyLog5 for Imputed
# happyLog5 <- glm(Happy ~ EducationLevel + Q118237 + Q116197 + 
#                    Q111848 + Q108342 + Q101162 +   
#                    Kids, family=binomial, data=train)

# happyLog5 for NonImputed
happyLog5 <- glm(Happy ~ EducationLevel + 
                   Q118237 + Q116197 + Q111848 + Q108342 + 
                   Q101162 + Kids, family=binomial, data=train)
summary(happyLog5)

# extract significant coefficients
significant5 <- data.frame(summary(happyLog5)$coef[summary(happyLog5)$coef[,4] <= .05, 4])
rownames(significant5)

# cross-validation (K = number of validation sets)
cv.err <- cv.glm(na.omit(train), happyLog5, K=10)$delta
cv.err

## Prediction for GLM

## predict for testing set NonImputed
predictLog5 <- predict(happyLog5, type="response", newdata=test)
## check training set accuracy
table(test$Happy, predictLog5 >= 0.5)
TruePositives <- 233
FalseNegatives <- 66
TrueNegatives <- 111
FalsePositives <- 111
(TruePositives + TrueNegatives) / (TruePositives + TrueNegatives + FalsePositives + FalseNegatives)
# -> NonImputed: 0.6602687

## predict for testing set Imputed
predictLog5 <- predict(happyLog3, type="response", newdata=test)
## check training set accuracy
table(test$Happy, predictLog5 >= 0.5)
TruePositives <- 608
FalseNegatives <- 173
TrueNegatives <- 321
FalsePositives <- 284
(TruePositives + TrueNegatives) / (TruePositives + TrueNegatives + FalsePositives + FalseNegatives)
# -> Imputed Log2: 0.6767677
# -> Imputed Log3: 0.6608947
# -> happy2Log1: 0.6594517
# -> happy2Log3: 0.6702742

#resultTable <- cbind(happy$UserID, predictLog5)
#str(resultTable)
#write.table(x=resultTable, file="resultTable.csv", sep=",")


## Error
SSE <- sum((predictLog5 - test$Happy)^2, na.rm=T)
RMSE <- sqrt(SSE/nrow(test))
RMSE
# -> NonImputed: 0.2902666
# -> Imputed Log3: 0.4627513
# -> Imputed Log3: 0.4644824
# -> happy2Log3: 0.462009

# AUC of test set
ROCRpredTest <- prediction(predictLog5, test$Happy)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
# -> NonImputed: NA (Error: Error in prediction(happyLog5, test$Happy) : Number of cross-validation runs must be equal for predictions and labels.)
# -> Imputed happyLog2: 0.7077491
# -> Imputed happyLog3: 0.7013492
# -> happy2Log3: 0.710039

## ROC Curve

ROCRperf <- performance(ROCRpredTest, "tpr", "fpr")
# Receiver Operator Characteristic Curve
plot(ROCRperf)

# add threshold labels and color
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# !!!
summary(predictLog5)



#### Trees

library(rpart)
library(rpart.plot)
library(ROCR)

# happyTree1 Imputed
happyTree1 <- rpart(Happy ~ ., data=train, method="class")
prp(happyTree1)
printcp(happyTree1)
plotcp(happyTree1)

# happyTree2 Imputed
happyTree2 <- rpart(Happy ~ Q118237 + Q101162 + LowIncome,
                    data=train, method="class")
prp(happyTree2)
printcp(happyTree2)

# happy2Tree2
happyTree2 <- rpart(Happy ~ Q118237 + Q101162 + Income +
                      Q123621 + Q119334,
                    data=train, method="class")
prp(happyTree2)
printcp(happyTree2)


happyTree3 <- rpart(Happy ~ Q118237 + Q107869 + Q101162 + Q108856 + 
                      Q116197 + Income, data=train, method="class")
prp(happyTree3)
printcp(happyTree3)

happyTree4 <- rpart(Happy ~ Q118237 + Q107869 + Q101162 + Q108856 + Q116197, 
                    data=train, method="class")
prp(happyTree4)
printcp(happyTree4)


## Prediction
happyPredictTree4 <- predict(happyTree2, newdata=test, type="class")
# threshold of 0.5

# test set accuracy
table(test$Happy, happyPredictTree4)
TruePositives <- 630
FalseNegatives <- 151
TrueNegatives <- 267
FalsePositives <- 338
(TruePositives + TrueNegatives) / (TruePositives + TrueNegatives + FalsePositives + FalseNegatives)
# -> Imputed: 0.6443001
# -> happy2Tree2: 0.6471861


## Change minbucket or cp parameter
# -> didn't change anything
printcp(happyTree4)
plotcp(happyTree4)
summary(happyTree4)
plot(happyTree4)
prp(happyTree4)

PredictROC <- predict(happyTree2, newdata=test)
# interpret column 2 as probability that this test set observation has outcome 1
# we use second column when thresholding
pred <- prediction(PredictROC[,2], test$Happy)
perf <- performance(pred, "tpr", "fpr")
plot(perf)

# AUC
as.numeric(performance(pred, "auc")@y.values)
# -> happy2Tree2: 0.64408

# tune model

library(caret)
library(e1071)
# set model parameters
fitControl <- trainControl(method="cv", number=10)
cartGrid <- expand.grid(.cp=(1:50)*0.01)
train(Happy ~ Q118237 + Q101162 + LowIncome, 
        data=train, method="rpart", trControl=fitControl,
      tuneGrid=cartGrid)

# build model
happyTree5 <- rpart(Happy ~ Q118237 + Q107869 + Q101162 + Q108856 + Q116197,  
                    method="class", data=train, 
                    control=rpart.control(cp=0.01))

# make predictiton
happyPredictTree5 <- predict(happyTree4, newdata=test, type="class")
table(test$Happy, happyPredictTree5)
TruePositives <- 634
FalseNegatives <- 147
TrueNegatives <- 258
FalsePositives <- 347
(TruePositives + TrueNegatives) / (TruePositives + TrueNegatives + FalsePositives + FalseNegatives)

# -> tuning cp did not change outcome so cp was efficient from the start

## Error
SSE <- sum((as.numeric(happyPredictTree2) - test$Happy)^2)
RMSE <- sqrt(SSE/nrow(test))
RMSE


#### Random Forest
library(randomForest)
set.seed(123)
happyRF1 <- randomForest(Happy ~ .,
                         data=train, method="class")

prp(happyRF1)








# possible methods / models
names(getModelInfo())

# tuning parameters for glm
getModelInfo()$glm

# determine cross validation
fitControl <- trainControl(method="cv", number=10)

#  data frame with possible tuning values
cartGrid <- expand.grid(.cp=(1:50)*0.01)

# train glm model
g <- train(as.factor(Happy) ~ Q119334 + Q118237 + Q116441 + Q115610 + Q108855 + 
             Q107869 + Q102289 + Q101162 + Q100689 + Q99716 + Q98869, 
           data=train, method="glm", metric="Accuracy", 
           trControl=fitControl, tuneLength = 10)
g
summary(g)
# -> maximum accuracy: 0.668

# train decision tree model
c <- train(as.factor(Happy) ~ ., data=train, method="C5.0", metric="Accuracy",
           trControl=fitControl)
c
# -> maximum accuracy with model tree: 0.66

# train nearest neighbour model
k <- train(as.factor(Happy) ~ ., data=train, method="knn", metric="Accuracy",
           trControl=fitControl)
k
# -> maximum accuracy: 0.66

# train random forest model
r <- train(as.factor(Happy) ~ ., data=train, method="rf", metric="Accuracy",
           trControl=fitControl)
r





# build model
StevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                         Respondent + LowerCourt + Unconst, 
                       method="class", data=Train, 
                       control=rpart.control(cp=0.18))

# make predictiton
PredictCV <- predict(StevensTreeCV, newdata=Test, type="class")
table(Test$Reverse, PredictCV)
accuracy <- (59+64) / (59+18+29+64)
accuracy




















###############################################################


######## Week 8: Visualization

###### Visualizing the World: An Introduction to Visualization 

#### Video 4: Basic Scatterplots Using ggplot

WHO <- read.csv("WHO.csv")
str(WHO)

# scatterplot
plot(WHO$GNI, WHO$FertilityRate)
library(ggplot2)
# 1. Data object and aesthetic mapping for scatterplot
scatterplot <- ggplot(WHO, aes(x=GNI, y=FertilityRate))
# 2. geometric objects to put into plot
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point()
# blue triangles instead of circles and add title
scatterplot + geom_point(color="blue", size=3, shape=15)
scatterplot + geom_point(color="darkred", size=3, shape=8) +
                           ggtitle("Fertility Rate vs GNI")

fertilityGNIplot <- scatterplot + geom_point(color="darkred", size=3, space=8) + 
  ggtitle("Fertility Rate vs GNI")
pdf("Myplot.pdf")
print(fertilityGNIplot)
dev.off()


#### Video 5: Advanced Scatterplots Using ggplot
# color points according to Region
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point()
# color points according to Life Expectancy
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x=FertilityRate, y=Under15)) + geom_point()
# log transform
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + geom_point()
# add regression model
model <- lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)
# add regression line to plot
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + 
  geom_point() + stat_smooth(method="lm")
# change confidence level
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + 
  geom_point() + stat_smooth(method="lm", level=0.99)
# remove confidence level
ggplot(WHO, aes(x=log(FertilityRate), y=Under15)) + 
  geom_point() + stat_smooth(method="lm", se=F, color="orange")


#### The Analytical Policeman: Visualization for Law and Order 

#### Video 3: A Line Plot

mvt <- read.csv("mvt.csv", stringsAsFactors=F)
str(mvt)

# replace date with formatted time
mvt$Date <- strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekdays <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour
str(mvt)

# line plot
table(mvt$Weekday)
WeekdayCounts <- as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

# make Var1 an ordered factor variable to get days in chronological order
WeekdayCounts$Var1 <- factor(WeekdayCounts$Var1, ordered=T, 
                             levels=c("Sunday", "Monday", "Tuesday", 
                                      "Wednesday", "Thursday", "Friday", 
                                      "Saturday"))

# Quick Question 3
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + 
  xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + 
  xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")


#### Video 4: A Heatmap

table(mvt$Weekday, mvt$Hour)
DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

DayHourCounts$Var1 <- factor(WeekdayCounts$Var1, ordered=T, 
                             levels=c("Monday", "Tuesday", 
                                      "Wednesday", "Thursday", 
                                      "Friday", "Saturday", "Sunday"))

ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq))   
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + 
  scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y=element_blank())
# change color
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) + 
  scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y=element_blank())


##### Video 5: A Geographical Hot Spot Map

library(maps)
library(ggmap)
chicago <- get_map(location="chicago", zoom=11)
ggmap(chicago)
ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))
LatLonCounts <- as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)

LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) + 
scale_color_gradient(low="yellow", high="red")
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")


#### Video 6: A Heatmap on the United States

murders <- read.csv("murders.csv")
str(murders)

statesMap <- map_data("state")
str(statesMap)

ggplot(statesMap, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") + 
  coord_map("mercator")

murders$region <- tolower(murders$State)
murderMap <- merge(statesMap, murders, by="region")
str(murderMap)

ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide="legend")

# Murder rate
murderMap$MurderRate <- murderMap$Murders / murderMap$Population

# plot map of murder rate which looks strange due to small outliers that are red but not visible
# so add limits
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) +
  geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0.9,10))

# remove outliers


###### Recitation: The Good, the Bad, and the Ugly: Visualization

#### Video 3: Bar Charts in R
library(ggplot2)
intl <- read.csv("intl.csv")
str(intl)

# barplot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=PercentOfIntl))

# make region ordered factor in decreasing order
intl <- transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

# multiply all value by 100
intl$PercentOfIntl <- intl$PercentOfIntl * 100

# re-draw plot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity", fill="dark blue") + 
  geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
  ylab("Percentage of International Students") +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(angle=45, hjust=1))


#### Video 5: World Maps in R

library(ggmap)
intlall <- read.csv("intlall.csv", stringsAsFactors=F)
head(intlall)

# make NA entries zero
intlall[is.na(intlall)] <- 0
head(intlall)

world_map <- map_data("world")
str(world_map)

# merge data frames
world_map <- merge(world_map, intlall, by.x="region", by.y="Citizenship")
str(world_map)

# plot map
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black") + 
  coord_map("mercator")
# seems the merger ha re-ordered the data frame

# re-order world_map
world_map <- world_map[order(world_map$group, world_map$order),]

# plot map again
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black") + 
  coord_map("mercator")
# -> China and Russia are missing because they had different names in the merged data frames

table(intlall$Citizenship)

# adjust China
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] <- "China"
table(intlall$Citizenship)

# merge data frames
world_map <- merge(map_data("world"), intlall, by.x="region", by.y="Citizenship")
# re-order world_map
world_map <- world_map[order(world_map$group, world_map$order),]

# plot map again with slightly diferent parameters
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=Total), color="black") + 
  coord_map("mercator")

# orthographic depiction
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(aes(fill=Total), color="black") + 
  coord_map("ortho", orientation=c(20,30,0))


#### Video 7: Using Line Charts Instead

library(reshape2)
households <- read.csv("households.csv")
str(households)
# -> no year group fraction

# convert data frame
households[,1:2]
head(melt(households, id="Year"))
households[,1:3]
melt(households, id="Year")[1:10,]

ggplot(melt(households, id="Year"), aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) +
  geom_point(size=5) + 
  ylab("Percentage of Households")


###### Week 8: Assignment

library(ggplot2)
library(maps)
library(ggmap)

# Then, load the US map and save it to the variable statesMap, like we did during the Crime lecture:
statesMap <- map_data("state")


#### Problem 1 - Drawing a Map of the US

str(statesMap)

# How many different groups are there? 
unique(statesMap$group)

# You can draw a map of the United States by typing the following in your R console:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

# We specified two colors in geom_polygon -- fill and color. 
# Which one defined the color of the outline of the states?
# -> black


#### Problem 2 - Coloring the States by Predictions

polling <- read.csv("PollingImputed.csv")

# split data into train and test
Train <- subset(polling, Year==2004 | Year==2008)
Test <- subset(polling, Year==2012)

# Then, create a logistic regression model and make predictions on the test set using the following commands:
mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data=Train, family="binomial")
TestPrediction <- predict(mod2, newdata=Test, type="response")

# TestPrediction gives the predicted probabilities for each state, but let's also create a vector of Republican/Democrat predictions by using the following command:
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)

# Now, put the predictions and state labels in a data.frame so that we can use ggplot:
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, Test$State)

# For how many states is our binary prediction 1, corresponding to Republican?
table(predictionDataFrame$TestPredictionBinary)

# What is the average predicted probability of our model?
summary(predictionDataFrame)

# Now, we need to merge "predictionDataFrame" with the map data "statesMap", like we did in lecture.
# Before doing so, we need to convert the Test.State variable to lowercase, so that it matches the region variable in statesMap. Do this by typing the following in your R console:
predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)

# Now, merge the two data frames using the following command:
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")

# Lastly, we need to make sure the observations are in order so that the map is drawn properly, by typing the following:
predictionMap <- predictionMap[order(predictionMap$order),]

# How many observations are there in predictionMap?
str(predictionMap)

# How many observations are there in statesMap?
str(statesMap)

# When we merged the data in the previous problem, it caused the number of observations to change. Why? 
# -> Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process. 

# You can color the states according to our binary predictions by typing the following in your R console:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black")

# The states appear light blue and dark blue in this map. Which color represents a Republican prediction?
# -> Light blue 

# We see that the legend displays a blue gradient for outcomes between 0 and 1. However, in our model there are only two possible outcomes: 0 or 1. Let's replot the map with discrete outcomes. We can also change the color scheme to blue and red, to match the blue color associated with the Democratic Party in the US and the red color associated with the Republican Party in the US. This can be done with the following command:
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Change the plot command above to instead color the states by the variable TestPrediction. You should see a gradient of colors ranging from red to blue. 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# How many states look purple in the map?
# -> 1


####  Problem 3 - Understanding the Predictions

# What does it mean when a state appears with a purple "in-between" color on the map?
# -> Our logistic regression classifier is not as confident in its prediction for this state
# Explanation: Our logistic regression classifier did classify each state for which we have data. However, a state will only appear bright red if the logistic regression probability was close to 1, and will only appear bright blue if the logistic regression probability was close to 0. An in-between color signifies less confidence in the prediction. This is a good way to visualize uncertainty. Although Iowa, the state that appears purple here, was a hard state for us to predict, we don't know whether or not it was a close race in the 2012 election. The color only represents what our model thought.

# In the 2012 election, the state of Florida ended up being a very close race. It was ultimately won by the Democratic party. Did we predict this state correctly or incorrectly?
# -> We incorrectly predicted this state by predicting that it would be won by the Republican party. 

# What was our predicted probability for the state of Florida?
predictionDataFrame$TestPrediction[predictionDataFrame$region=="florida"]

# What does this imply?
# -> Our prediction model did not do a very good job of correctly predicting the state of Florida, and we were very confident in our incorrect prediction. 


#### Problem 4 - Parameter Settings

#?geom_polygon

# What is the name of the parameter we set to have value 3 to create plot (1)? 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", linetype=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# What is the name of the parameter we set to have value 3 to create plot (2)?
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", linetype=1, size=3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# Plot (3) was created by changing the value of a different geom_polygon parameter to have value 0.3. 
# Which parameter did we use?
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black", linetype=1, size=2, alpha=0.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


###### Visualizing Attributes of Parole Violators


#    male = 1 if the parolee is male, 0 if female
#    race = 1 if the parolee is white, 2 otherwise
#    age = the parolee's age in years at the time of release from prison
#    state = a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. These three states were selected due to having a high representation in the dataset.
#    time.served = the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
#    max.sentence = the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
#    multiple.offenses = 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
#    crime = a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
#    violator = 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.


#### Problem 1 - Loading the Data

parole <- read.csv("parole.csv")
str(parole)

# Since male, state, and crime are all unordered factors, convert them to factor variables using the following commands:
parole$male <- as.factor(parole$male)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

# What fraction of parole violators are female?
prop.table(table(parole$male[parole$violator==1]))

# In this dataset, which crime is the most common in Kentucky?
table(parole$crime[parole$state==2])
# -> Drug-related crime 


####  Problem 2 - Creating a Basic Histogram 

# Recall from lecture that in ggplot, we need to specify the dataset, the aesthetic, and the geometry. 
# To create a histogram, the geometry will be geom_histogram. The data we'll use is parole, and the 
# aesthetic will be the map from a variable to the x-axis of the histogram. 

# Create a histogram to find out the distribution of the age of parolees, by typing the following command in your R console:
ggplot(data = parole, aes(x = age)) + geom_histogram()

# By default, geom_histogram divides the data into 30 bins. Change the width of the bins to 5 years by adding the argument "binwidth = 5" to geom_histogram. 
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5)

# Note that by default, histograms create bins where the left endpoint is included in the bin, 
# but the right endpoint isn't. So the first bin in this histogram represents parolees who are 
# between 15 and 19 years old. The last bin in this histogram represents parolees who are 
# between 65 and 69 years old.

# What is the age bracket with the most parolees?
# -> 20-24

# Redo the histogram, adding the following argument to the geom_histogram function: color="blue". 
# What does this do? Select all that apply.
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color="blue")
# -> Changes the outline color of the bars


####  Problem 3 - Adding Another Dimension

# Now suppose we are interested in seeing how the age distribution of male parolees compares to the age distribution of female parolees.
# ggplot has the ability to do this automatically using the facet_grid command.

# To create separate histograms for male and female, type the following command into your R console:
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)

# What is the age bracket with the most female parolees?
# -> 35-39

# Now change the facet_grid argument to be ".~male" instead of "male~.". What does this do?
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(. ~ male)

# Run the following command in your R console to produce a histogram where data points are colored by group:
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)

# Since we didn't specify colors to use, ggplot will use its default color selection. What color is the histogram for the female parolees?
# -> Salmon

# An alternative to a single, stacked histogram is to create two histograms and overlay them on top of each other. This is a simple adjustment to our previous command.

# 1) Tell ggplot not to stack the histograms by adding the argument position="identity" to the geom_histogram function.
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position="identity")

# 2) Make the bars semi-transparent so we can see both colors by adding the argument alpha=0.5 to the geom_histogram function.
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position="identity", alpha=0.5)

# Which of the following buckets contain no female paroles? 
# -> 15-19, 55-59, 65-69


#### Problem 4 - Time Served

# Create a basic histogram like the one we created in Problem 2, but this time with time.served on the x-axis. Set the bin width to one month.
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1)

# What is the most common length of time served, according to this histogram?
# -> Between 4 and 5 months 

# Change the binwidth to 0.1 months. 
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1)

# Now what is the most common length of time served, according to the histogram?
# -> Between 3.0 and 3.1 months 
# !!! Be careful when choosing the binwidth - it can significantly affect the interpretation of a histogram! When visualizing histograms, it is always a good idea to vary the bin size in order to understand the data at various granularities. 

# To visualize this, change the binwidth back to 1 month, and use facet_grid to 
# create a separate histogram of time.served for each value of the variable crime. 
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1) + facet_grid(. ~ crime)

# Which crime type has no observations where time served is less than one month? 
# Recall #2 is larceny, #3 is drug-related, #4 is driving-related, and #1 other .
# -> Driving-related 

# For which crime does the frequency of 5-6 month prison terms exceed the frequencies of each other term length?
# -> Drug-related

# Now, instead of faceting the histograms, overlay them. Remember to set the position and alpha parameters so that the histograms are not stacked. 
# Also, make sure to indicate that the fill aesthetic should be "crime".
ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1, position="identity", alpha=0.5)

# In this case, faceting seems like a better alternative. Why?
# -> With four different groups, it can be hard to tell them apart when they are overlayed. 


###### Visualizing Network Data

# id: A unique identifier for this user; this is the value that appears in the rows of edges.csv
# gender: An identifier for the gender of a user taking the values A and B. Because the data is anonymized, we don't know which value refers to males and which value refers to females.
# school: An identifier for the school the user attended taking the values A and AB (users with AB attended school A as well as another school B). Because the data is anonymized, we don't know the schools represented by A and B.
# locale: An identifier for the locale of the user taking the values A and B. Because the data is anonymized, we don't know which value refers to what locale.


####  Problem 1.1 - Summarizing the Data

# Load the data from edges.csv into a data frame called edges, and load the data from users.csv into a data frame called users.
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")

# How many Facebook users are there in our dataset?
str(edges)
str(users)
146*2/59
# Explanation: From str(edges) or nrow(edges), we see that there are 146 pairs of users in our dataset who are Facebook friends. However, each pair (A, B) must be counted twice, because B is a friend of A and A is a friend of B. To think of this in simpler terms, consider a network with just new people, A and B, and a single edge (A, B). Even though there are two vertices and one edge, each user has on average one friend.
# !! Finally, note that in all likelihood these users have a much higher number of Facebook friends. We are computing here the average number of people in this dataset who are their friends, instead of the average total number of Facebook friends. 

# Out of all the students who listed a school, what was the most common locale?
table(users$locale, users$school)

# Is it possible that either school A or B an all-girls or all-boys school?
table(users$gender, users$school)
# -> No


#### Problem 2 - Creating a Network

library(igraph)

# Based on ?graph.data.frame, which of the following commands will create a graph g describing our social network, with the attributes of each user correctly loaded?
# ->  g = graph.data.frame(edges, FALSE, users)

# Use the correct command from Problem 2.1 to load the graph g.
g <- graph.data.frame(edges, FALSE, users)

# Now, we want to plot our graph. By default, the vertices are large and have text labels of a user's identifier. 
# Because this would clutter the output, we will plot with no text labels and smaller vertices:
plot(g, vertex.size=5, vertex.label=NA)

# How many connected components with at least 2 nodes are there in the graph?
# -> 4

# How many users are there with no friends in the network?
# -> 7

# We can use degree(g) to compute the degree of all the nodes in our graph g.
# How many users are friends with 10 or more other Facebook users in this network?
d <- degree(g) >= 10
sum(d[d==T])

# To visually draw attention to these nodes, we will change the size of the vertices so the vertices with high degrees are larger. To do this, we will change the "size" attribute of the vertices of our graph to be an increasing function of their degrees:
V(g)$size <- degree(g)/2+2

# Now that we have specified the vertex size of each vertex, we will no longer use the vertex.size parameter when we plot our graph:
plot(g, vertex.label=NA)

# What is the largest size we assigned to any node in our graph?
max(V(g)$size)

# What is the smallest size we assigned to any node in our graph?
min(V(g)$size)


####  Problem 3 - Coloring Vertices

# To change the color, we will update the attribute V(g)$color.

# We can update the colors by setting the color to black for all vertices, than setting it to red for the vertices with gender A and setting it to gray for the vertices with gender B:
V(g)$color <- "black"
V(g)$color[V(g)$gender == "A"] <- "red"
V(g)$color[V(g)$gender == "B"] <- "gray"

# Plot the resulting graph. 
plot(g, vertex.label=NA)

# What is the gender of the users with the highest degree in the graph?
# -> Gender B

# Now, color the vertices based on the school that each user in our network attended.
table(V(g)$school)
V(g)$color <- "black"
V(g)$color[V(g)$school == "AB"] <- "red"
V(g)$color[V(g)$school == "A"] <- "gray"

# Are the two users who attended both schools A and B Facebook friends with each other?
plot(g, vertex.label=NA)
# -> Yes

# What best describes the users with highest degree?
# -> Some, but not all, of the high-degree users attended school A 

# Now, color the vertices based on the locale of the user.
table(V(g)$locale)
V(g)$color <- "black"
V(g)$color[V(g)$locale == "A"] <- "red"
V(g)$color[V(g)$locale == "B"] <- "gray"

# The large connected component is most associated with which locale?
plot(g, vertex.label=NA)
# -> Locale B

# The 4-user connected component is most associated with which locale?
# -> Locale A


#### Problem 4 - Other Plotting Options

#?igraph.plotting

# Which igraph plotting function would enable us to plot our graph in 3-D?
# -> rglplot

# What parameter to the plot() function would we use to change the edge width when plotting g?
# -> edge.width


###### Visualizing Text Data Using Word CLouds

# Tweet -- the text of the tweet
# Avg -- the sentiment of the tweet, as assigned by users of Amazon Mechanical Turk. The score ranges on a scale from -2 to 2, where 2 means highly positive sentiment, -2 means highly negative sentiment, and 0 means neutral sentiment.


####  Problem 1 - Preparing the Data 

library(tm)
library(SnowballC)
library(RTextTools)

tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
str(tweets)

# Next, perform the following pre-processing tasks (like we did in Week 5), noting that we don't stem the words in the document or remove sparse terms: 

# 1) Create a corpus using the Tweet variable
corpus <- Corpus(VectorSource(tweets$Tweet))

# 2) Convert the corpus to lowercase
corpus <- tm_map(corpus, tolower)

# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)

# 4) Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# 5) Build a document-term matrix out of the corpus
dtm <- DocumentTermMatrix(corpus)

# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(dtm))

# How many unique words are there across all the documents?
str(allTweets)
# -> 3780

# Although we typically stem words during the text preprocessing step, we did not do so here. 
# What is the most compelling rationale for skipping this step when visualizing text data?
# -> It will be easier to read and understand the word cloud if it includes full words instead of just the word stems 


#### Problem 2 - Building a Word Cloud

library(wordcloud)
?wordcloud

# Which function can we apply to allTweets to get a vector of the words in our dataset, which we'll pass as the first argument to wordcloud()?
colnames(allTweets)

# Which function should we apply to allTweets to obtain the frequency of each word across all tweets?
colSums(allTweets)

# From ?wordcloud, we can see that the "scale" parameter controls the sizes of the plotted words. By default, the sizes range from 4 for the most frequent words to 0.5 for the least frequent, as denoted by the parameter "scale=c(4, 0.5)". We could obtain a much smaller plot with, for instance, parameter "scale=c(2, 0.25)".
wordcloud(words=colnames(allTweets),freq=colSums(allTweets),scale=c(4,.5),min.freq=3,max.words=Inf,random.order=TRUE, random.color=FALSE, 
	    rot.per=.1,colors="black",ordered.colors=FALSE,use.r.layout=FALSE,fixed.asp=TRUE)

# What is the most common word across all the tweets (it will be the largest in the outputted word cloud)? 
# -> apple

# In the previous subproblem, we noted that there is one word with a much higher frequency than the other words. 
# Repeat the steps to load and pre-process the corpus, this time removing the most frequent word in addition to all elements of stopwords("english") in the call to tm_map with removeWords.

# Replace allTweets with the document-term matrix of this new corpus -- we will use this updated corpus for the remainder of the assignment.

# 1) Create a corpus using the Tweet variable
corpus <- Corpus(VectorSource(tweets$Tweet))

# 2) Convert the corpus to lowercase
corpus <- tm_map(corpus, tolower)

# 3) Remove punctuation from the corpus
corpus <- tm_map(corpus, removePunctuation)

# 4) Remove all English-language stopwords
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "apple"))

# 5) Build a document-term matrix out of the corpus
dtm <- DocumentTermMatrix(corpus)

# 6) Convert the document-term matrix to a data frame called allTweets
allTweets <- as.data.frame(as.matrix(dtm))

# Create a word cloud with the updated corpus. What is the most common word in this new corpus?
wordcloud(words=colnames(allTweets),freq=colSums(allTweets),scale=c(4,.5),min.freq=3,max.words=Inf,random.order=TRUE, random.color=FALSE, 
	    rot.per=.1,colors="black",ordered.colors=FALSE,use.r.layout=FALSE,fixed.asp=TRUE)
# -> iphone


#### Problem 3 - Size and Color

# Which word cloud is based only on the negative tweets (tweets with Avg value -1 or less)?
# -> Word Cloud C

# Only one word cloud was created without modifying parameters min.freq or max.words. 
# Which word cloud is this?
# -> Word Cloud A

# Which word clouds were created with parameter random.order set to FALSE?
# -> Word Cloud B, D

# Which word cloud was built with a non-default value for parameter rot.per?
# -> Word Cloud A

# For which word cloud was the parameter ordered.colors set to TRUE?
# -> Word Cloud C


#### Problem 4 - Selecting a Color Palette

# The function brewer.pal() returns color palettes from the ColorBrewer project when provided 
# with appropriate parameters, and the function display.brewer.all() displays the palettes we 
# can choose from.

# Which color palette would be most appropriate for use in a word cloud for which we want to use color to indicate word frequency?
display.brewer.all()
# -> YlOrRd
# Explanation: From ?brewer.pal we read that Accent and Set2 are both "qualitative palettes," which means color changes don't imply a change in magnitude (we can also see this in the output of display.brewer.all). As a result, the colors selected would not visually identify the least and most frequent words.
# Explanation: On the other hand, YlOrRd is a "sequential palette," with earlier colors begin lighter and later colors being darker. Therefore, it is a good palette choice for indicating low-frequency vs. high-frequency words. 

# Which RColorBrewer palette name would be most appropriate to use when preparing an image for a document that must be in grayscale?
# -> Greys

# Which of the following commands addresses this issue by removing the first 4 elements of the 9-color palette of blue colors?
# -> brewer.pal(9, "Blues")[c(-1, -2, -3, -4)], brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]

# Final Word Cloud
wordcloud(words=colnames(allTweets),freq=colSums(allTweets),scale=c(4,.5),min.freq=3,max.words=Inf,random.order=TRUE, random.color=FALSE, 
	    rot.per=.1,colors=brewer.pal(8,"Accent"),ordered.colors=FALSE,use.r.layout=FALSE,fixed.asp=TRUE)









