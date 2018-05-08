---
title: "Creating simulated random effects data for two level"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Using this website: http://m-clark.github.io/docs/sem/latent-growth-curves.html

So creating a data set with 500 total people across 4 time points (ranging from 0 to 3) totaling 2,000 people.  Just creating the numbers for each and generate the actual data later. 

To add a treatment variable, we need to randomly select a set of either 4 0's or 4 1's.  So create a variable that samples from two varibles each with four of one's or zero's and do this 500 times.
```{r}
set.seed(1234)
n = 500
timepoints = 4
time = rep(0:3, times=n)
subject = rep(1:n, each=4)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = 4)
```
Then set the intercept and slope that you want.  Then create a matrix to generate the correlation between the intercept and slope to be .2.  This will surve as our sigma for the multivariate normal distribution generation.  

Then we need to generate the data
```{r}
library(MASS)
intercept = .5
slopeT = .25
slopeI = .25
randomEffectsCorr = matrix(c(1,.2,.2, .2, 1,.2, .2, .2, 1), ncol = 3)
randomEffectsCorr

randomEffects = mvrnonnorm(2000, mu = c(0,0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT", "SlopeI")
```
Now we need to create the indiviudal variables.  This means adding the intercept and the slope, but not sure why we just aren't adding the intercept 
randomEffects$Int[subject] = must be the intercept for each person
randomEffects$Slope[subject] = must be the slope for each person.

So we want to add the intercept and slope to person estimate from the multivaraite estimation, plus an error, because that is the model for predicting y.  Sigma is the error term, which is draw from a normal distribution.

So I think we need to assume the data is standardized for this example.
Adding the [subject] says for each subject give them the same number.
```{r}
sigma = .5
y1 = (intercept + randomEffects$Int[subject])+(slope + randomEffects$SlopeT[subject])*time + randomEffects$SlopeT[subject]*intervention + rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, y1)
d
```
Generate the data the + 1 doesn't change the results.
```{r}
library(lme4)
model1 = lmer(y1 ~ time*intervention + (time|subject), data = d)
summary(model1)
```
Let's add a variable first.  So we want to add a binary variable.  I think we need to create the variable.  So each subject needs to be in the same group though.  

Trying to understand what the level three model looks like
```{r}
library(simr)
set.seed(12345)
datCon = data.frame(id = rep(1:40, each = 30), cluster = rep(rep(1:5, each = 6), 40),time = rep(1:6, 200))
colnames(datCon) = c("id", "cluster", "time")
dim(datCon)
treatment = ifelse(datCon$cluster < 4, 1, 0)
datCon$treatment = treatment
### Get the dim for for each
datTreat = subset(datCon, cluster < 4)
dim(datTreat)

# For six time points go up by 2 points each time point
datControl = subset(datCon, cluster > 3)
dim(datControl)
## Now create the outcomes
outcomeTreat = rbind(rnorm(720/6, 50-10, 5), rnorm(720/6, 50-8, 5), rnorm(720/6, 50-6, 5), rnorm(720/6, 50-4, 5), rnorm(720/6, 50-2, 5), rnorm(720/6, 50, 5))

outcomeControl = rbind(rnorm(720/6, 30, 5), rnorm(720/6, 30, 5), rnorm(720/6, 30, 5), rnorm(720/6, 30, 5), rnorm(720/6, 30, 5), rnorm(720/6, 30, 5))

### Now put them into new variable called outcome
datCon$outcome = ifelse(datCon$cluster < 4, outcomeTreat, outcomeControl)
datCon

library(lme4)
model1 = lmer(outcome ~ time*treatment + (time | cluster/id), data = datCon)
summary(model1)
```

