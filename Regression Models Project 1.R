#' ---
#' title: "Regression Models Project 1"
#' author: "Dennis Oriaifo"
#' date: "June 25th, 2017"
#' ---


## Executive Summary
# Motor Trend a magazine about the automobile industry is interested in exploring the relationship between a set of variables and miles per gallon (MPG). They are particularly interested in the following two questions:
# 1) “Is an automatic or manual transmission better for MPG”
# 2) “Quantify the MPG difference between automatic and manual transmissions"
# 
# With the help of simple and multiple linear regressions, we conclude that there is in fact a statistically significant difference between the MPG for manual and automatic transmissions. 
# We can conclude the following:
# Cars with manual transmission get ~ 1.8 miles per gallon more than cars with automatic transmissions
# There is a negative relationship between horse power and MPG i.e for every 10 units of additional horsepower, there is a 0.03 reduction in MPG
# We also see a negative relationship between weight and MPG i.e for every 1000lb increase in weight, there is a 2.49 reduction in MPG
# Finally there is also a negative relationship between the number of cylinders and MPG
# R^2 = .86 i.e 86% of the variation in our data can be explained by this reduced model


rm(list=ls())
cat('\014')
library(knitr)
library(ggplot2)
data(mtcars)

mtcars$am = factor(mtcars$am,labels=c("Automatic","Manual"))
mtcars$carb = factor(mtcars$carb)
mtcars$cyl = factor(mtcars$cyl)
mtcars$gear = factor(mtcars$gear)
mtcars$vs = factor(mtcars$vs)

modelFull = lm(mpg ~ ., data = mtcars)
modelReduced = step(modelFull, direction = "both")
anova(modelReduced, modelFull)

summary(modelFull)
summary(modelReduced)

modelAM = lm(mpg ~ am, data = mtcars)
anova(modelAM, modelReduced)

modelReducedOutliers = hatvalues(modelReduced)
modelReducedOutliersSorted = sort(modelReducedOutliers)
head(sort(modelReducedOutliersSorted))
tail(sort(modelReducedOutliersSorted))

InfluenceMeasures = dfbetas(modelReduced)
InfluenceMeasuresSorted = sort(InfluenceMeasures[,6])
head(InfluenceMeasuresSorted,3)
tail(InfluenceMeasuresSorted,3)

# T test
t.test(mpg ~ am, data = mtcars)

# Visualizations MPG by Trans type
ggplot(mtcars, aes(y=mpg,x=am)) + geom_boxplot(aes(fill=factor(am))) 

#  Residuals
par(mfrow=c(2, 2))
plot(modelReduced)


## CONCLUSION

# Cars with manual transmission get ~ 1.8 miles per gallon more than cars with automatic transmissions
# There is a negative relationship between horse power and MPG i.e for every 10 units of additional horsepower, there is a 0.03 reduction in MPG
# We also see a negative relationship between weight and MPG i.e for every 1000lb increase in weight, there is a 2.49 reduction in MPG
# Finally there is also a negative relationship between the number of cylinders and MPG
# R^2 = .86 i.e 86% of the variation in our data can be explained by this reduced model

