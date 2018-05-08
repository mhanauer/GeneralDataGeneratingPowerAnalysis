---
title: "Creating simulated random effects data for two level"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Using this website to provide the example and then extending by adding a treatment variable: http://m-clark.github.io/docs/sem/latent-growth-curves.html

So creating a data set with 500 total people across 4 time points (ranging from 0 to 3) totaling 2,000 people.  Just creating the numbers for each and generate the actual data later. 

To add an intervention I create a treat variable which I then sample 500 the treat variable 500 times and replicate that number four times so we randomly assigned each person to treatment and control. 
```{r}
n = 500
timepoints = 4
time = rep(0:3, times=n)
subject = rep(1:n, each=4)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = 4)
```
I am assuming I have an outcome that is normally distrbuted. 

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25 using a rnorm.  For the intervention slope   

Then I am creating the random effects for intercept and time, because each person gets a unique intercept and a unique slope for time  
```{r}
library(MASS)
n = 500
intercept = .5
slopeT = .25
slopeI = rnorm(n, .25, .05)
randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
```
Now we need to create the indiviudal variables.  This means adding the intercept and the slope, but not sure why we just aren't adding the intercept 
randomEffects$Int[subject] = must be the intercept for each person
randomEffects$Slope[subject] = must be the slope for each person.

So we want to add the intercept and slope to person estimate from the multivaraite estimation, plus an error, because that is the model for predicting y.  Sigma is the error term, which is draw from a normal distribution.

So I think we need to assume the data is standardized for this example.
Adding the [subject] says for each subject give them the same number.
```{r}
sigma = .25
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI[subject]*intervention + rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention, y1)
d
```
Generate the data the + 1 doesn't change the results.
```{r}
library(lme4)
model1 = lmer(y1 ~ time*intervention + (time|subject), data = d)
summary(model1)
```
So we will have a random effects that will not be correlated with each other, but correlated within.  So I think just generate a seperate set of random effects.

Then need to figure out the nesting of the data.  Should be the same nesting with the multilevel model (time)(people)(cluster).  So time and people and should be random not cluster.

So how many clusters for 500 people so 50 people per cluster.  Need to include n in this somehow.  Cluster will be an each thing.  So the each for cluster needs to be divisable by the each for the subject.

Here the random assignment is at the cluster level.
n = number of people
nC = number of people per cluster 50*4 because there are 50 people per cluster over four times points so 200 total data points per cluster
cluster = number of clusters
```{r}
n = 500
nC = 50*4
cluster = 10
timepoints = 4
time = rep(0:3, times=n)
subject = rep(1:n, each=4)
cluster = rep(1:10, each = nC) 
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), 10)
intervention = rep(intervention, each = nC)
dat = data.frame(time, subject, cluster, intervention)
```
Now we have the data and we need to create the random effects, which I think are two sets one for the intercept and slope for time and the other for people
```{r}
n = n
interceptT = .5
slopeT = .25
randomEffectsCorrT = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorrT

randomEffectsT = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffectsT = data.frame(randomEffects)
colnames(randomEffectsT) = c("IntT", "SlopeT")

n = nC
interceptP = .5
slopeP = .25
randomEffectsCorrP = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorrP

randomEffectsP = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffectsP = data.frame(randomEffects)
colnames(randomEffectsP) = c("IntP", "SlopeP")

slopeI = rnorm(n, .25, .05)

```
Now we need to generate the outcome data while accounting for the nesting.  So i think we just nest the clustering variable in the clusters. 

So if the intervention is randomly assigned at the cluster level then it will be different from the cluster.  So I want the random effects to be by cluster for the cluster random effects, because each cluster gets its own intercept and slope

Still not sure conceptually about the cluster*subject multiplication
```{r}
sigma = .25
y1 = (interceptT + randomEffectsT$IntT[subject])+(slopeT + randomEffectsT$SlopeT[subject])*time + (interceptP + randomEffectsP$IntP[cluster]) + (slopeP + randomEffectsP$SlopeP[cluster])*subject + slopeI[cluster]*intervention + rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, cluster, intervention, y1)
d
```
So now we put together the model and see if we can recover the parameters.
```{r}
library(lme4)
model1 = lmer(y1 ~ time*intervention + (time | cluster/subject), data = d)
summary(model1)
```
