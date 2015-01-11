#rm(list = ls())   # clear workspace

## Set working directory
setwd("C:/Users/Alexander/Desktop 2/Lectures/Books+Presentations/Doing Data Science Straight Talk from the Frontline/data")
getwd()


#### Chapter 2: Statistical Inference, Exploratory Data Analysis, and the Data Science Process

## Task

# Once you have the data loaded, it's time for some EDA:
#
# 1. Create a new variable, age_group, that categorizes users as "<18",
# "18-24", "25-34", "35-44", "45-54", "55-64", and "65+".
# 
# 2. For a single day:
#   . Plot the distributions of number impressions and clickthrough-
#   rate (CTR=# clicks/# impressions) for these six age categories.
#         . Define a new variable to segment or categorize users based on
#         their click behavior.
#         . Explore the data and make visual and quantitative comparisons
#         across user segments/demographics (<18-year-old males versus
#                                            < 18-year-old females or logged-in versus not, for example).
#         . Create metrics/measurements/statistics that summarize the data.
#         Examples of potential metrics include CTR, quantiles, mean,
#         median, variance, and max, and these can be calculated across
#         the various user segments. Be selective. Think about what will
#         be important to track over time-what will compress the data,
#         but still capture user behavior.
#
# 3. Now extend your analysis across days. Visualize some metrics and
#         distributions over time.
#
# 4. Describe and interpret any patterns you find.

data1 <- read.csv("nyt1.csv")

# categorize
head(data1)
data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))

# view
summary(data1)

# brackets
#install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~agecat, data=data1, FUN=siterange)
# so only signed in users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat,
          data=data1)
# plot
#install.packages("ggplot2")
library(ggplot2)
d <- ggplot(data1, aes(x=Impressions, fill=agecat))
d+geom_histogram(binwidth=1)

f <- ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat))
f+geom_boxplot()

# create click thru rate
# we don't care about clicks if there are no impressions
# if there are clicks with no imps my assumptions about
# this data are wrong
data1$hasimps <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks ~ hasimps, data=data1, FUN=siterange)
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions,
                                         colour=agecat)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions,
                                    colour=agecat)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=agecat, y=Clicks,
                                    fill=agecat)) + geom_boxplot()
ggplot(subset(data1, Clicks>0), aes(x=Clicks, colour=agecat)) + geom_density()

# create categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"

# Convert the column to a factor
data1$scode <- factor(data1$scode)
head(data1)

#look at levels
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat,
                    data = data1, FUN=clen)







