---
title: "Creating simulated random effects data for two and three level models"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here is the model that I am trying to recreate

Level 1: There is the intercept that varies for each person over time.  Then there is the slope for time that varies for each person over time.  Finally there is the error term that is unique for each data point.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + e_{ij}}~~~ (1.1)$$
Level 2 Intercept: Here the intercept is broken down into the consant plus the effect of the intervention, which is at level in the intercept, because it does not vary over time only by person and the error term which varies by person.
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Intervention_{j} + u_{0j}} ~~~ (1.2)$$
Then there is level the two slope which has the constant effect, plus the effect of time for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Intervention_{j} + u_{1j}} ~~~ (1.3)$$
Then we have the mixed model, which has all the components combined

$$Mixed~model: ~~~{y_{ij} =  (\gamma_{10}+ \gamma_{11}Time_{j} + +u_{1j}Time_{ij}) + (\gamma_{00}+ \gamma_{01}Intervention_{j} + u_{0j}) +  \gamma_{11}Time_{ij}Intervention_{j} + e_{ij}} ~~~(1.4)$$
I am basing this example on the example below and extending it by adding a treatment variable: http://m-clark.github.io/docs/sem/latent-growth-curves.html

I am creating a data set with 500 total people across 4-time points (ranging from 0 to 3) totaling 2,000 data points.  

I then create the number of subjects, which are 500 people replicated four times each.

To add an intervention I create a treat variable which I then sample from 500 times and then replicate these values 4 times.
```{r}
n = 500
timepoints = 4
time = rep(0:3, times=n)
subject = rep(1:n, each=4)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = 4)
```
I am assuming I have an outcome that is normally distributed and in standard normal form.     

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25 using a rnorm.

Then I am creating the random effects for intercept and time, because each person gets a unique intercept and a unique slope for time  

I am also creating a slope for the interaction effect between time and intervention, which is also .25
```{r}
library(MASS)
n = 500
intercept = .5
slopeT = .25
slopeI = .25
slopeTI = .25
randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
```
Now we need to create the indiviudal variables.  This means adding the random intercept and the slope  
randomEffects$Int[subject] = must be the intercept for each person
randomEffects$Slope[subject] = must be the slope for each person.

So we want to add the intercept and slope to person estimate from the multivaraite estimation, plus an error, because that is the model for predicting y.  Sigma is the error term, which is draw from a normal distribution.

So I think we need to assume the data is standardized for this example.
Adding the [subject] says for each subject give them the same number.
```{r}
sigma = .1
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI*intervention + slopeTI*time*intervention+ rnorm(n*timepoints, mean = 0, sd = sigma)
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
n = 1000
nC = 100*4
cluster = 10
timepoints = 4
time = rep(0:3, times=n)
subject = rep(1:n, each=4)
cluster = rep(1:cluster, each = nC) 
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), cluster)
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
sigma = .1
y1 = (interceptT + randomEffectsT$IntT[subject])+(slopeT + randomEffectsT$SlopeT[subject])*time + (interceptP + randomEffectsP$IntP[cluster]) + (slopeP + randomEffectsP$SlopeP[cluster]) + slopeI[cluster]*intervention + rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, cluster, intervention, y1)

```
So now we put together the model and see if we can recover the parameters.
```{r}
library(lme4)
model1 = lmer(y1 ~ time*intervention + (time | cluster/subject), data = d)
summary(model1)
```
