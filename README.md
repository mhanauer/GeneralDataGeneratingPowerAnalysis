---
title: "Creating simulated data with correlations between them"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Want to create simulated longitudinal data that will be used in a multilevel power analysis for the simr package in R.  First I am creating the data for the treatment group, which I am assuming will be weakly correlated positively correlated over time so setting correlation to .1

Then I am using the mvrnorm package to create the data that is correlated with the settings that I have in the corrDataControl matrix.  I set the means to 7.  

I then use the cor function to reproduce the orginal correlation matrix to check that it is accruately generating the data with the desired underlying correlation. 
```{r}
library(MASS)
library(psych)
corrDataControl <- matrix(c(1,.1,.1,.1,  .1,1,.1,.1,  .1,.1,1,.1,  .1, .1, .1, 1),4,4)
corrDataControl
eigen(corrDataControl) # make sure all eigenvalues are not negative if so see cor.smooth
set.seed(123)
dataControl = mvrnorm(100, mu = c(rep(7,4)), Sigma = corrDataControl, empirical = TRUE)
cor(dataControl)
```
Now we do the same thing for the treatment group.  
```{r}
corrDataTreatment <- matrix(c(1,.3,.3,.3,  .3,1,.3,.3,  .3,.3,1,.3,  .3, .3, .3, 1),4,4)
corrDataTreatment
eigen(corrDataTreatment) # make sure all eigenvalues are not negative if so see cor.smooth
set.seed(123)
dataTreatment = mvrnorm(100, mu = c(rep(15,4)), Sigma = corrDataTreatment, empirical = TRUE)
cor(dataTreatment)
```
Now combine the data.  
```{r}
dataControl = data.frame(dataControl)
colnames(dataControl) = c("Time1", "Time2", "Time3", "Time4")
dataControl$treatment = rep(0, dim(dataControl)[1])
dataControl

dataTreatment = data.frame(dataTreatment)
colnames(dataTreatment) =  c("Time1", "Time2", "Time3", "Time4")
dataTreatment$treatment = rep(1, dim(dataTreatment)[1])
dataTreatment

dataPower = rbind(dataTreatment, dataControl)
head(dataPower)
```
Now get the data into long format.  
```{r}
library(reshape)
dataPower = reshape(dataPower, varying = list(c("Time1", "Time2", "Time3", "Time4")), times = c(1,2,3,4), direction = "long")
colnames(dataPower) = c("Treatment", "Time", "Outcome", "ID")
dataPower
```
Now we want to analyze the data in lme4 with simpower.  I am setting the fixef for the parameter estimate of interest the interaction between time and treatment to 1 and saying to increase the sample along ID, which mean increase the number of people per time point.  I am only running 4 times, because it take awhile. 
```{r}
library(simr)
model1 = lmer(Outcome ~ Time*Treatment + (Time | ID), data = dataPower)
summary(model1)

fixef(model1)["Time:Treatment"] = 1
powerCurve(model1, test = fixed("Time:Treatment"),  along = "ID", nsim = 4)

```
Now try with binary outcomes
```{r}
install.packages("evd")
install.packages("SimCorMultRes")
library(SimCorMultRes)
```

