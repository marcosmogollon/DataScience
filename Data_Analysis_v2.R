#rm(list = ls())   # clear workspace

## Set working directory
setwd("c:/Documents and Settings/wagnale/My Documents/R/Courses/Data_Analysis")
getwd()

#### Quiz 1

# Q3
set.seed(31);
heightsCM = rnorm(30, mean=188, sd=5);
weightsK = rnorm(30, mean=84, sd=3);
hasDaughter = sample(c(TRUE, FALSE), size=30, replace=T);
dataFrame = data.frame(heightsCM, weightsK, hasDaughter);

dataFrameSubset = dataFrame[dataFrame$heightsCM > 188, ]

mean(dataFrameSubset$weightsK) 

# Q4
set.seed(41)
cauchyValues = rcauchy(100, location = 0, scale = 1)
set.seed(415)
sample(cauchyValues, size=10, replace=T, prob = NULL)


#### Week 2

## Structure of Data Analysis - Mail Case Study
# If it isn't installed, install the kernlab package
library(kernlab)
data(spam)
dim(spam)

# Subsampling our data set
set.seed(3435)
# Split into training and test set by assigning 0 or 1
trainIndicator = rbinom(4601, size=1, prob=0.5)
table(trainIndicator)

# Subsetting
trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]
dim(trainSpam)

# Exploratory data analysis
# names tells us variables of the data frame
names(trainSpam)
head(trainSpam)
# what is spam and what not in training set
table(trainSpam$type)

# make some basic plots (percentage of capital letters in spam and non-spam messages)
plot(trainSpam$capitalAve ~ trainSpam$type)

# in previous figure there are values above 1
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

# Relationship between predictors
plot(log10(trainSpam[,1:4]+1))

# Clustering
hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

# some transformation to make it easier to see
hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)

# Statistical prediction / modeling
# Write a loop that goes over all 55 variables that correspond
# to fraction of times they appear and fit predicted model, then
# calculate error rate and pick model with lowest

trainSpam$numType = as.numeric(trainSpam$type)-1
costFunction = function(x,y){sum(x!=(y > 0.5))}
cvError = rep(NA,55)
library(boot)
for (i in 1:55){
  lmFormula = as.formula(paste("numType~",names(trainSpam)[i],sep=""))
  glmFit = (lmFormula,family="binomial",data=trainSpam)
  cvError[i] = cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}
which.min(cvError)

names(trainSpam)[which.min(cvError)]

# Get measure of uncertainty
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5] = "spam"
table(predictedSpam, testSpam$type)

(61 + 458)/(1346 + 458 + 61 + 449)

# Interpreting Results
# Challenge Results

## Organizing a data analysis

## Getting Data

# Get/set working directory
getwd()
setwd("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis")
getwd()

# Download a file from the web
fileURL <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileURL, destfile="c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis")
list.files("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis")

dateDownload <- date()
dateDownload

# Get loval file
cameraData <- read.table("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\Baltimore_Fixed_Speed_Cameras.csv", sep=",",header=T)
head(cameraData)

# Getting data off webpages
library(XML)
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode

## Data Resources
# http://www.data.gov
# http://www.gapminder.org/
# www.asdfree.com
# www.infochimps.com/marketplace
# www.kaggle.com

## Summarizing Data
fileURL <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt"
download.file(fileUrl,destfile="c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis", method="curl")
dateDownloaded <- date()
dateDownloaded
# alternatively read.table
eData <- read.table("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\Earthquake.txt", sep=",",header=T)

# Looking at data
dim(eData)
names(eData)
head(eData)
nrow(eData)
ncol(eData)

quantile(eData$Lat)
summary(eData)

# Looking at class
class(eData)
# Trick: applying class function to first row in dataset
sapply(eData[1,],class)

# Looking at variables - qualitative variables!
unique(eData$Src)
length(unique(eData$Src))
table(eData$Src)
# relationship etween source and version variable
table(eData$Src, eData$Version)

# Look at any() and all()
eData$Lat[1:10]
eData$Lat[1:10] > 40
any(eData$Lat[1:10] > 40)

eData$Lat[1:10] > 40
all(eData$Lat[1:10] > 40)

# Looking at subsets
eData[eData$Lat > 0 & eData$Lon > 0, c("Lat", "Lon")]
eData[eData$Lat > 0 | eData$Lon > 0, c("Lat", "Lon")]

# Check if missing values
is.na(eData$Src[1:10])
sum(is.na(eData$Src))
sum(is.na(eData))
table(is.na(eData))

# Important to note about NA's
table(c(0,1,2,3,NA,3,3,2,2,3))
table(c(0,1,2,3,NA,3,3,2,2,3), useNA="ifany")  # if any NA it will be shown

# Summarize by rows / columns - variables must be numeric
colSums(eData)
rowSums(eData)
# setting na.rm (Na remove) will ignore all NA's
colMeans(eData, na.rm=T)
rowMeans(eData, na.rm=T)

## Data Munging

# Fixing character vectors
names(cameraData)
tolower(names(cameraData))  # make all characters in variable names small

splitNames = strsplit(names(cameraData), "\\.")  # split on period
splitNames[[5]]
splitNames[[6]]
splitNames[[6]][1]
firstElement <- function(x){x[1]}
# cut off only first element for all names
sapply(splitNames,firstElement)

head(eData, 2)

testName <- "this_is_a_test"
sub("_","",testName)
gsub("_","",testName)

# Reshaping data example
misShaped <- as.data.frame(matrix(c(NA,5,1,4,2,3),byrow=T,nrow=3))
names(misShaped) <- c("treatmentA", "treatmentB")
misShaped$people <- c("John","Jane","Mary")
misShaped

# Reconstruct data so that in each row we have a prticular observation
library(reshape2)
melt(misShaped, id.vars="people", variable.name="treatment", value.name="value")


#### Week 3


## Exploratory Graphs

pData <- read.csv("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\ss06pid.csv")

# boxplot
boxplot(pData$AGEP,col="blue")

boxplot(pData$AGEP ~ as.factor(pData$DDRS),col="blue")

boxplot(pData$AGEP ~ as.factor(pData$DDRS),col=c("blue","orange"),names=c("yes","no"),varwidth=TRUE)

barplot(table(pData$CIT),col="blue")

# histogram
hist(pData$AGEP,col="blue")

hist(pData$AGEP,col="blue", breaks=100,main="Age")

# density graph
dens <- density(pData$AGEP)
plot(dens,lwd=3,col="blue")

dens <- density(pData$AGEP)
densMales <- density(pData$AGEP[which(pData$SEX==1)])
plot(dens,lwd=3,col="blue")
lines(densMales,lwd=3,col="orange")

# scatterplot
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue")
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5)
plot(pData$JWMNP,pData$WAGP,pch=19,col=pData$SEX,cex=0.5)

# use size to illustrate relation
percentMaxAge <- pData$AGEP / max(pData$AGEP)
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=percentMaxAge*0.5)

# overlaying lines/points
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5)
lines(rep(100,dim(pData)[1]),pData$WAGP,col="grey",lwd=5)
points(seq(0,200,length=100),seq(0,20e5,length=100),col="red",pch=19)

# numeric variables as factors
library(Hmisc)
ageGroups <- cut2(pData$AGEP,g=5)
plot(pData$JWMNP,pData$WAGP,pch=19,col=ageGroups,cex=0.5)

# if one has a lot of points
x <- rnorm(1e5)
y <- rnorm(1e5)
plot(x,y,pch=19)

# use sampled values
sampledValues <- sample(1:1e5,size=1000,replace=FALSE)
plot(x[sampledValues],y[sampledValues],pch=19)

# use smoothScatter - smooth density plot
x <- rnorm(1e5)
y <- rnorm(1e5)
smoothScatter(x,y)

# use hexbin
library(hexbin)
x <- rnorm(1e5)
y <- rnorm(1e5)
hbo <- hexbin(x,y)
plot(hbo)

# QQ-plot
x <- rnorm(20)
y <- rnorm(20)
qqplot(x,y)
abline(c(0,1))

# Matplot ans spaghetti
X <- matrix(rnorm(20*5),nrow=20)
matplot(X,type="b")

# heatmap - but notice columns become rows in image
image(1:10,161:236,as.matrix(pData[1:10,161:236]))

newMatrix <- as.matrix(pData[1:10,161:236])
newMatrix <- t(newMatrix)[,nrow(newMatrix):1]    # command to avoid swap of rows & columns
image(161:236, 1:10, newMatrix)

# Maps - very basic
library(maps)
map("world")
lat <- runif(40,-180,180); lon <- runif(40,-90,90)
points(lat,lon,col="blue",pch=19)

# Missing values and plots
x <- c(NA,NA,NA,4,5,6,7,8,9,10)
y <- 1:10
plot(x,y,pch=19,xlim=c(0,11),ylim=c(0,11))

# look at distribution of NA's
x <- rnorm(100)
y <- rnorm(100)
y[x < 0] <- NA
boxplot(x ~ is.na(y))


## Expository Graphs

# Axes
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5,
     xlab="Travel time (mln)",ylab="Last 12 months wages (dollars)")

plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5,
     xlab="Travel time (mln)",ylab="Last 12 months wages (dollars)",cex.lab=2,cex.axis=1.5)

# Legends
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5,
     xlab="TT (mln)",ylab="Wages (dollars)")

legend(100,200000,legend="All surveyed",col="blue",pch=19,cex=0.5)


plot(pData$JWMNP,pData$WAGP,pch=19,cex=0.5,
     xlab="TT (mln)",ylab="Wages (dollars)",col=pData$SEX)

legend(100,200000,legend=c("men","women"),col=c("black","red"),pch=c(19,19),cex=c(0.5,0.5))

# Titles
plot(pData$JWMNP,pData$WAGP,pch=19,cex=0.5,
     xlab="TT (mln)",ylab="Wages (dollars)",main="Some Title",col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),pch=c(19,19),cex=c(0.5,0.5))

# Multiple Panels
par(mfrow=c(1,2))
hist(pData$JWMNP,xlab="CT (min)",col="blue",breaks=100,main="Title")
mtext(text="a",side=3,line=1)
plot(pData$JWMNP,pData$WAGP,pch=19,cex=0.5,
     xlab="TT (mln)",ylab="Wages (dollars)",col=pData$SEX)

legend(100,200000,legend=c("men","women"),col=c("black","red"),pch=c(19,19),cex=c(0.5,0.5))
mtext(text="b",side=3,line=1)

# back to normal
par(mfrow=c(1,1))

# How to create a PDF
pdf(file="Panel.pdf",height=4,width=8)
plot(pData$JWMNP,pData$WAGP,pch=19,cex=0.5,
     xlab="TT (mln)",ylab="Wages (dollars)",col=pData$SEX)

legend(100,200000,legend=c("men","women"),col=c("black","red"),pch=c(19,19),cex=c(0.5,0.5))
mtext(text="b",side=3,line=1)
dev.off()

# take exact picture on screen and save to pdf
dev.copy2pdf(file="Panel2.pdf")


## Hierarchical Clustering

# Hierarchical Clustering Example
set.seed(1234); par(mar=c(5,5,5,5))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

dataFrame <- data.frame(x=x,y=y)
dist(dataFrame)

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

# Pretty dendogram function
myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))

# heatmap()
dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)


## K-Means Clustering

set.seed(1234); par(mar=c(5,5,5,5))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

# kmeans() Function
dataFrame <- data.frame(x,y)
kmeans0bj <- kmeans(dataFrame,centers=3)
names(kmeans0bj)

kmeans0obj$cluster

par(mar=rep(4,4))
plot(x,y,col=kmeans0bj$cluster,pch=19,cex=2)
points(kmeans0bj$centers,col=1:3,pch=3,cex=3,lwd=3)

# Heatmaps
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeans0bj2 <- kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2),mar=rep(4,4))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")
image(t(dataMatrix)[,order(kmeans0bj$cluster)],yaxt="n")


## Dimension Reduction
set.seed(12345); par(mfrow=c(1,1),mar=rep(4,4))
dataMatrix <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])

# Cluster the data to see in a different way if there is a pattern -> No
heatmap(dataMatrix)

# What if we add a pattern
set.seed(678910)
for (i in 1:40){
  # flip coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)    # first 5 variables are 0, last 5 are 3
  }
}

# For some of the rows of the matrix there is a pattern that shows 0 for the first 5 and 3 for the last 5
# Since there are only 10 in a row this covers all

# Now check matrix with pattern
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])

# Use cluster to see pattern
heatmap(dataMatrix)

# Patterns in rows and columns
hh <- hclust(dist(dataMatrix)); dataMatrix0rdered <- dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrix0rdered)[,nrow(dataMatrix0rdered):1])
plot(rowMeans(dataMatrix0rdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrix0rdered),xlab="Column",ylab="Column Mean",pch=19)

# Components of the SVD - u and v
svd1 <- svd(scale(dataMatrix0rdered))
par(mfrow=c(1,3))
image(t(dataMatrix0rdered)[,nrow(dataMatrix0rdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

# Left singular vector figure shows the pattern of first rows
# compared with different (no) pattern for last rows

# Right singular vector figure shows the pattern across the rows plotted by column
# One can see that last five columns have same value and are different from first five
# who seem to have different values which is due to the fact that we added 3 to last 5
# column but 0 = random noise to first five

# Components of SVD - d and variance explained
svd1 <- svd(scale(dataMatrix0rdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2 / sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

# plot d values by column shows they are decreasing with first right singular vector starting
# at 12 

# In second figure we see that first right and left singular vector explain about 40%
# of variance in the matrix

# Relationship to principal components (PCH)
svd1 <- svd(scale(dataMatrix0rdered))
pca1 <- prcomp(dataMatrix0rdered,scale=TRUE)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",
     ylab="Right Singular Vector 1")
abline(c(0,1))

# It show SVD and PCA are the same in this case

# Components of SVD - variance explained
constantMatrix <- dataMatrix0rdered*0
for (i in 1:dim(dataMatrix0rdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2 / sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

# In constant matrix everey row is identical, with each having 5 values of 0 and 5 of 1.
# If we then calculate SVD and plot d values, we can see there is only one de value standing
# out above the others which explains 100% of variance in the matrix.
# Reason beign that I can replicate the matrix only by multiplying first left and right
# singular vector together -> so there is only one pattern

# What if we add a second pattern?
set.seed(678910)
for (i in 1:40){
  # flip coin
  coinFlip1 <- rbinom(1,size=1,prob=0.5)
  coinFlip2 <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)    # first 5 variables are 0, last 5 are 3
  }
  if(coinFlip2){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5),each=5)
  }
}
hh <- hclust(dist(dataMatrix)); dataMatrix0rdered <- dataMatrix[hh$order,]

# Now two patterns - Check clustering patterns
svd2 <- svd(scale(dataMatrix0rdered))
par(mfrow=c(1,3))
image(t(dataMatrix0rdered)[,nrow(dataMatrix0rdered):1])
plot(rep(c(0,1),each=5),pch=19,xlab="Column",ylab="Pattern 1")
plot(rep(c(0,1),each=5),pch=19,xlab="Column",ylab="Pattern 2")

# Pattern 1 is low for first five and high for second five
# pattern 2 is alternating

# v and patterns of variance in rows
svd2 <- svd(scale(dataMatrix0rdered))
par(mfrow=c(1,3))
image(t(dataMatrix0rdered)[,nrow(dataMatrix0rdered):1])
plot(svd2$v[,1],xlab="Column",ylab="First right singular vector")
plot(svd2$v[,2],xlab="Column",ylab="Second right singular vector")

# d and variance explained
svd1 <- svd(scale(dataMatrix0rdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2 / sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

# fast.svd function {corpcor}
bigMatrix <- matrix(rnorm(1e4*40),nrow=1e4)
system.time(svd(scale(bigMatrix)))
library(corpcor)
system.time(fast.svd(scale(bigMatrix),tol=0))

# Missing values
dataMatrix2 <- dataMatrix0rdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
svd1 <- svd(scale(dataMatrix))
# should be an error but isn't?

# Imputing {impute}
library(impute)
dataMatrix2 <- dataMatrix0rdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
svd1 <- svd(scale(dataMatrix0rdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)

# Face Example
load("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\face.rda")
par(mfrow=c(1,1))
image(t(faceData)[,nrow(faceData):1])

# Face example - variance explained
svd1 <- svd(scale(faceData))
plot(svd1$d^2 / sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")

# first 5 values explain large amount of variance
# So we can reconstruct the image only using first or first 5 or 10 singular vectors

# Face example - create approximations
svd1 <- svd(scale(faceData))
# %*% is matrix multiplication

# Here svd1$d[1] is a constant
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]

# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*%  t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*%  t(svd1$v[,1:10])

par(mfrow=c(1,4))
image(t(faceData)[,nrow(faceData):1])
image(t(approx10)[,nrow(approx10):1])
image(t(approx5)[,nrow(approx5):1])
image(t(approx1)[,nrow(approx1):1])


#### Week 4

## Clustering Example

load("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\samsungData.rda")

names(samsungData)[1:12]

table(samsungData$activity)

# plotting average acceleration for first subject
par(mfrow=c(1,2))
numericActivity <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==1]
plot(samsungData[samsungData$subject==1,1],pch=19,col=numericActivity,ylab=names(samsungData[1]))
plot(samsungData[samsungData$subject==1,2],pch=19,col=numericActivity,ylab=names(samsungData[2]))
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)

# Clustering based just on average acceleration
distanceMatrix <- dist(samsungData[samsungData$subject==1,1:3])
hclustering <- hclust(distanceMatrix)
par(mfrow=c(1,1))
myplclust(hclustering,lab.col=numericActivity)

# plotting max acceleration for the first subject
par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==1,10],pch=19,col=numericActivity,ylab=names(samsungData[10]))
plot(samsungData[samsungData$subject==1,11],pch=19,col=numericActivity,ylab=names(samsungData[11]))

# Clustering based just on max acceleration
distanceMatrix <- dist(samsungData[samsungData$subject==1,10:12])
hclustering <- hclust(distanceMatrix)
par(mfrow=c(1,1))
myplclust(hclustering,lab.col=numericActivity)

# Singular value decomposition
svd1 <- svd(scale(samsungData[samsungData$subject==1,-c(562,563)]))
par(mfrow=c(1,2))
plot(svd1$u[,1],col=numericActivity,pch=19)
plot(svd1$u[,2],col=numericActivity,pch=19)

# activities for first subject are in rows
# leaving out last two variables
# u is left singular vector and 1 is first left singular vector
# 2 is second singular vector
# vectors show most prevalent patterns where most variation is
# second singular vector is orthogonal (independent) to first and
# distinguishes quite well between different activities

# Find maximum contributor (look at right singular vector)
par(mfrow=c(1,1))
plot(svd1$v[,2],pch=19)

# New clustering with maximum contributor
maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(samsungData[samsungData$subject==1,c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
par(mfrow=c(1,1))
myplclust(hclustering,lab.col=numericActivity)

# check which variable was able to increase clustering efficiency by looking
# at which variable was the maximum contributor
names(samsungData)[maxContrib]

# K-means clustering (nstart=1,first try)
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])
# Clustering is pretty poor although we told k-means that there are 6 clusters

# Note that k-means starting point is random so try to run the clustering again
# and you will get very different results
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])

# Try k-means with different starts e.g. 100 and then do averaging
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=100)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])

# Look at cluster 1 variable centers (laying)
plot(kClust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="")

# Look at cluster 2 variable centers (walking)
plot(kClust$center[6,1:10],pch=19,ylab="Cluster Center",xlab="")


## Basic Least Squares

library(UsingR)
data(galton)
galton[1:10,]
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

# Only know the child - average height
par(mfrow=c(1,1))
hist(galton$child,col="blue",breaks=100)
meanChild <- mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)

# What if we plot child versus average parent
plot(galton$parent,galton$child,pch=19,col="blue")

# jittered plot
set.seed(1234)
plot(jitter(galton$parent,factor=2),jitter(galton$child,factor=2),pch=19,col="blue")

# Average parent = 65 inches tall
plot(galton$parent,galton$child,pch=19,col="blue")
near65 <- galton[abs(galton$parent - 65)<1,]
points(near65$parent,near65$child,pch=19,col="red")
lines(seq(64,66,length=100),rep(mean(near65$child),100),col="red",lwd=4)

# Average parent = 71 inches tall
plot(galton$parent,galton$child,pch=19,col="blue")
near71 <- galton[abs(galton$parent - 71)<1,]
points(near71$parent,near71$child,pch=19,col="red")
lines(seq(70,72,length=100),rep(mean(near71$child),100),col="red",lwd=4)

# Fitting a line with linear model (least squares algorithm)
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)
# left side of tilde in lm model is the outcome while right side are model parameters

# Plot what is leftover
par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)
plot(galton$parent,lm1$residuals,pch=19,col="blue")
abline(c(0,0),col="red",lwd=3)


## Inference Basics

# Fit a line to the Galton Data
par(mfrow=c(1,1))
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)

lm1

# Create a "population" of 1 million families
newGalton <- data.frame(parent=rep(NA,1e6),child=rep(NA,1e6))
newGalton$parent <- rnorm(1e6,mean=mean(galton$parent),sd=sd(galton$parent))
newGalton$child <- lm1$coeff[1] + lm1$coeff[2] * newGalton$parent + 
  rnorm(1e6,sd=sd(lm1$residuals))
smoothScatter(newGalton$parent,newGalton$child)
abline(lm1,col="red",lwd=3)

# Let's take a sample
set.seed(134325)
sampleGalton1 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm1 <- lm(sampleGalton1$child ~ sampleGalton1$parent)
plot(sampleGalton1$parent,sampleGalton1$child,pch=19,col="blue")
lines(sampleGalton1$parent,sampleLm1$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

# Let's take another sample - result will be different
sampleGalton1 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm1 <- lm(sampleGalton1$child ~ sampleGalton1$parent)
plot(sampleGalton1$parent,sampleGalton1$child,pch=19,col="blue")
lines(sampleGalton1$parent,sampleLm1$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

# Many samples
sampleLm <- vector(100,mode="list")
for(i in 1:100){
  sampleGalton <- newGalton[sample(1:1e6,size=50,replace=F),]
  sampleLm[[i]] <- lm(sampleGalton$child ~ sampleGalton$parent)
}

smoothScatter(newGalton$parent,newGalton$child)
for(i in 1:100){abline(sampleLm[[i]],lwd=3,lty=2)}
abline(lm1,col="red",lwd=3)

# Histogram of estimates
par(mfrow=c(1,2))
hist(sapply(sampleLm,function(x){coef(x)[1]}),col="blue",xlab="Intercept",main="")
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="")

# Estimating the values in R
sampleGalton4 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm4 <- lm(sampleGalton4$child ~ sampleGalton4$parent)
summary(sampleLm4)

par(mfrow=c(1,1))
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="",freq=F)
lines(seq(0,5,length=100),dnorm(seq(0,5,length=100),mean=coef(sampleLm4)[2],
                                sd=summary(sampleLm4)$coeff[2,2]),lwd=3,col="red")

# t versus normal distribution - t converges to normal with increasing degrees of freedom
x <- seq(-5,5,length=100)
plot(x,dnorm(x),type="l",lwd=3)
lines(x,dt(x,df=3),lwd=3,col="red")
lines(x,dt(x,df=10),lwd=3,col="blue")

# confidence intervals
summary(sampleLm4)$coeff
confint(sampleLm4,level=0.95)

plot(1:10,type="n",xlim=c(0,1.5),ylim=c(0,100),
     xlab="Coefficient Values",ylab="Replication")
for(i in 1:100){
  ci <- confint(sampleLm[[i]]); color="red";
  if((ci[2,1] < lm1$coeff[2]) & (lm1$coeff[2] < ci[2,2])){color = "grey"}
  segments(ci[2,1],i,ci[2,2],i,col=color,lwd=3)
}
lines(rep(lm1$coeff[2],100),seq(0,100,length=100),lwd=3)

# How to report the inference
sampleLm4$coeff
confint(sampleLm4,level=0.95)
# A one inch increase in parental height is associated with a 0.77 inch 
# increase in child's height (95% CI: 0.42-1.12 inches).

## P-Values

library(UsingR)
data(galton)
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child ~ galton$parent)
abline(lm1,col="red",lwd=3)

# Null distribution + observed statistic (t-value of slope in lm model)
x <- seq(-20,20,length=100)
plot(x,dt(x,df=(928-2)),col="blue",lwd=3,type="l")
arrows(summary(lm1)$coeff[2,3],0.25,summary(lm1)$coeff[2,3],0,col="red",lwd=4)

summary(lm1)

# A quick simulated example
set.seed(9898324)
yValues <- rnorm(10)
xValues <- rnorm(10)
lm2 <- lm(yValues ~ xValues)
summary(lm2)

x <- seq(-5,5,length=100)
plot(x,dt(x,df=(10-2)),col="blue",lwd=3,type="l")
arrows(summary(lm2)$coeff[2,3],0.25,summary(lm2)$coeff[2,3],0,col="red",lwd=4)

# A quick simulated example 2
xCoords <- seq(-5,5,length=100)
plot(xCoords,dt(xCoords,df=(10-2)),col="blue",lwd=3,type="l")
xSequence <- c(seq(summary(lm2)$coeff[2,3],5,length=10),summary(lm2)$coeff[2,3])
ySequence <- c(dt(seq(summary(lm2)$coeff[2,3],5,length=10),df=8),0)
polygon(xSequence,ySequence,col="red")
polygon(-xSequence,ySequence,col="red")

# Simulate a ton of data sets with no signal
set.seed(8323)
pValues <- rep(NA,100)
for(i in 1:100){
  xValues <- rnorm(20)
  yValues <- rnorm(20)
  pValues[i] <- summary(lm(yValues ~ xValues))$coeff[2,4]
}

hist(pValues,col="blue",main="",freq=F)
abline(h=1,col="red",lwd=3)

# Simulate a ton of data sets with signal
set.seed(8323)
pValues <- rep(NA,100)
for(i in 1:100){
  xValues <- rnorm(20)
  yValues <- 0.2 * xValues + rnorm(20)
  pValues[i] <- summary(lm(yValues ~ xValues))$coeff[2,4]
}

hist(pValues,col="blue",main="",freq=F)
abline(h=1,col="red",lwd=3)

# Simulate a ton of data sets with signal and bigger sample size
set.seed(8323)
pValues <- rep(NA,100)
for(i in 1:100){
  xValues <- rnorm(100)
  yValues <- 0.2 * xValues + rnorm(100)
  pValues[i] <- summary(lm(yValues ~ xValues))$coeff[2,4]
}

hist(pValues,col="blue",main="",freq=F,xlim=c(0,1))
abline(h=1,col="red",lwd=3)

# How you interpret the results
summary(lm(galton$child - galton$parent))$coeff

# A one inch increase in parental height is associated with a 0.77 inch increase in child's 
# height (95% CI: 0.42-1.12 inches). This difference was statistically significant (P < 0.001).

## Regression with Factor Variables

# Movie data
movies <- read.table("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\movies.txt",
                     sep="\t",header=T,quote="")
head(movies)

# Score vs Rating
plot(movies$score ~ jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating))

# Average score by rating
meanRatings <- tapply(movies$score,movies$rating,mean)
points(1:4,meanRatings,col="red",pch="-",cex=5)

# Linear Model
lm1 <- lm(movies$score ~ as.factor(movies$rating))
summary(lm1)

# Plot fitted values
plot(movies$score ~ jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating))
points(1:4,lm1$coeff[1] + c(0,lm1$coeff[2:4]),col="red",pch="-",cex=5)

# What is the average difference in rating between G and R movies?
lm1 <- lm(movies$score ~ as.factor(movies$rating))
summary(lm1)
confint(lm1)
# look at R values; confidence interval covers zero which would indicate there is no
# significant difference in the means which could also be seen in lm model t values

# What is the average difference in rating between PG-13 and R movies?
lm2 <- lm(movies$score ~ relevel(movies$rating,ref="R"))
summary(lm1)
confint(lm2)
# intercept term is now average rating in R movies
# difference is estimate PG-13 0.205
# Again interval covers zero

# Is there any difference in score between any of the movie ratings?
# Here we use ANOVA = Analysis of Variance
lm1 <- lm(movies$score ~ as.factor(movies$rating))
anova(lm1)

# Sum of Squares (G movies)
gMovies <- movies[movies$rating=="G",]
xVals <- seq(0.2,0.8,length=4)
plot(xVals,gMovies$score,ylab="Score",xaxt="n",xlim=c(0,1),pch=19)
abline(h=mean(gMovies$score),col="blue",lwd=3)
abline(h=mean(movies$score),col="red",lwd=3)
segments(xVals+0.01,rep(mean(gMovies$score),length(xVals)),xVals+0.01,
         rep(mean(movies$score),length(xVals)),col="red",lwd=2)
segments(xVals-0.01,gMovies$score,xVals-0.01,rep(mean(gMovies$score),length(xVals)),
         col="blue",lwd=2)

# Tukey's (honestly significant difference test)
lm1 <- aov(movies$score ~ as.factor(movies$rating))
TukeyHSD(lm1)


## Multiple Variable Regression

# WHO childhood hunger data
hunger <- read.csv("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\hunger.csv")
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)

# Plot percent hungry versus time
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")

# Add the linear model
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
lines(hunger$Year,lm1$fitted,lwd=3,col="darkgrey")

# Color by male/female
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male") * 1+1))


# Color by male/female
lmM <- lm(hunger$Numeric[hunger$Sex=="Male"] ~ hunger$Year[hunger$Sex=="Male"])
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"] ~ hunger$Year[hunger$Sex=="Female"])
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male") * 1+1))
lines(hunger$Year[hunger$Sex=="Male"],lmM$fitted,col="black",lwd=3)
lines(hunger$Year[hunger$Sex=="Female"],lmF$fitted,col="red",lwd=3)

# Two lines, same slope in R
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male") * 1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2]),col="black",lwd=3)

# Two lines, different slopes in R
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex + hunger$Sex*hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male") * 1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3],lmBoth$coeff[2] + lmBoth$coeff[4]),
       col="black",lwd=3)

summary(lmBoth)


## Regression in the real world

# Hunger over time by region
par(mfrow=c(1,2))
plot(hunger$Year,hunger$Numeric,col=as.numeric(hunger$WHO.region),pch=19)
plot(1:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
legend(1,10,col=unique(as.numeric(hunger$WHO.region)),legend=unique(hunger$WHO.region),pch=19)

# Region is correlated with year
anova(lm(hunger$Year ~ hunger$WHO.region))

# Region correlated with hunger
anova(lm(hunger$Numeric ~ hunger$WHO.region))

# Including region - a complicated interaction
par(mfrow=c(1,1))
plot(hunger$Year,hunger$Numeric,col=as.numeric(hunger$WHO.region),pch=19)
lmRegion <- lm(hunger$Numeric ~ hunger$Year + hunger$WHO.region + hunger$Year * 
                 hunger$WHO.region)
abline(c(lmRegion$coeff[1] + lmRegion$coeff[6],lmRegion$coeff[6] + lmRegion$coeff[12]),
       col=5,lwd=3)

# Income Data
incomeData <- read.csv("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\income.csv",
                       header=F)
income <- incomeData[,3]
age <- incomeData[,1]

# Logs to address right-skew
par(mfrow=c(1,2))
smoothScatter(age,income)
hist(income,col="blue",breaks=100)
hist(log(income+1),col="blue",breaks=100)
smoothScatter(age,log(income+1))

# Outliers Example - extreme points
set.seed(1235)
xVals <- rcauchy(50)
par(mfrow=c(1,1))
hist(xVals,col="blue")

# Example - Outliers might be real
# Add Tim Cook, CEO of Apple 2011 income
age <- c(age,52)
income <- c(income,378e6)
smoothScatter(age,income)

# Variance changes
bupaData <- read.table("c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\bupa.txt",
                     sep=",",header=F)

ggt <- bupaData[,5]
aat <- bupaData[,3]
plot(log(ggt),aat,col="blue",pch=19)

# Plot the residuals - there is strange shape with large values
lm1 <- lm(aat ~ log(ggt))
plot(log(ggt),lm1$residuals,col="blue",pch=19)









####################################################################################
## Loans Assignment
####################################################################################

## Load data
loansData <- read.table(file="c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\loansData.csv",
                        header=T, sep=",")

# Save as RDA
save(loansData,file="c:\\Documents and Settings\\wagnale\\My Documents\\R\\Courses\\Data_Analysis\\loansData.rda")

## Look at the data
names(loansData)
head(loansData)
summary(loansData)
sapply(loansData[1,],class)

mygsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}
mygsub(c("< 1 year","1 year","10+"," years"),c("1","1","10",""),loansData$Employment.Length)


## Transform data into sensible format
loansData$Interest.Rate <- as.numeric(gsub("%","",loansData$Interest.Rate))/100
unique(loansData$Loan.Length)
loansData$Loan.Length <- as.numeric(gsub(" months","",loansData$Loan.Length))
unique(loansData$Employment.Length)
loansData$Employment.Length <- mygsub(c("< 1 year","1 year","+"," years"),c("1","1","",""),loansData$Employment.Length)
loansData$Employment.Length) <- mygsub("10+","10",loansData$Employment.Length)
loansData$Debt.To.Income.Ratio <- as.numeric(gsub("%","",loansData$Debt.To.Income.Ratio))/100

## http://hawkeyemaps.wordpress.com/2013/02/18/data-analysis-in-r-loan-interest-rates/

## Check Missing Values
sum(is.na(loansData))
colSums(is.na(loansData))

## Make some plots
plot(loansData$Debt.To.Income.Ratio,loansData$Interest.Rate)
plot(as.numeric(loansData$FICO.Range),loansData$Interest.Rate,col=as.factor(loansData$Loan.Length),pch=19)
legend("topright",col=unique(loansData$Loan.Length),legend=(unique(loansData$Loan.Length),pch=19)

legend(1,10,col=unique(as.numeric(hunger$WHO.region)),legend=(unique(hunger$WHO.region),pch=19)

plot(as.factor(loansData$Employment.Length),loansData$Interest.Rate)

loansData[loansData$Employment.Length == 5 & loansData$Interest.Rate > 0.24,]

x <- rnorm(100)
y <- rnorm(100)
plot(x,y)

plot(as.numeric(gsub("%","",loansData$Interest.Rate))/100)

?plot







