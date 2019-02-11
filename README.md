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


Level 2 Intercept: Here the intercept is broken down into the constant plus the effect of the intervention, which is at level 2 in the intercept because it does not vary over time only by person and the error term which varies by person. 

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Intervention_{j} + u_{0j}} ~~~ (1.2)$$


Then there is level the two slope which has the constant effect, plus the slope for the intervention for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Intervention_{j} + u_{1j}} ~~~ (1.3)$$

Then we have the mixed model, which has all the components combined
$$Mixed~model: ~~~{y_{ij} =   (\gamma_{00}+ \gamma_{01}Intervention_{j} + u_{0j}) + (\gamma_{10}}+\gamma_{11}*Intervention_{j} +u_{1j})*Time_{ij} + e_{ij} $$


I am basing this example on the example below and extending it by adding an intervention variable: http://m-clark.github.io/docs/sem/latent-growth-curves.html

I am creating a data set with 500 total people across 4-time points (ranging from 0 to 3) totaling 2,000 data points.  
I then create the number of subjects, which are 500 people replicated four times each.

To add an intervention I create a treat variable which I then sample from 500 times and then replicate these values 4 times.
```{r}

```
I am assuming I have an outcome that is normally distributed and in standard normal form.     

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25.

Then I am creating the random effects for the intercept and time, because each person gets a unique intercept and a unique slope for time.  

I am also creating a slope for the interaction effect between time and intervention, which is also .25
```{r}
library(MASS)
power_matt_two = function(){
n = 200
timepoints = 6
time = timepoints-1
time = rep(0:time, times=n)
subject = rep(1:n, each=timepoints)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = timepoints)
  intercept = .5
slopeT = .25
slopeI = .25
slopeTI = .25
randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
dim(randomEffects)
sigma = .05
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI*intervention + slopeTI*time*intervention+ rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention, y1)
model1 = lmer(y1 ~ time*intervention + (time|subject), data = d)
model1_summary = summary(model1)
model1_summary$coefficients[,1]
}

```
Try getting the effects and make sure they are being replicated
```{r}
rep = 50
power_rep = replicate(reps, power_matt_two())
power_rep_df = data.frame(power_rep)
power_rep_df_mean = rowMeans(power_rep_df)
power_rep_df_mean

```


Now I am trying to create the outcome variable that has the parameters above.  I am creating random effects for the intercept and slope across time because each person will get their own random effect over this variable, because it is nested within people.  Then I am creating the fixed effects, which are constant across the people according the variable.  For example, the slope for the intervention only varies across the whether someone get the intervention or not. 

Ok so when you have [subject] that just means that each subject gets the same value.  Then it makes sense, because you want the random effects for the person to be the same and then vary by time for the slope, because we want this estimate to vary over time instead of just the same data point for each person.
```{r}

```
Generate the data using the model that has the intervention effect that I want with time nested within participants.
```{r}
library(lme4)

```


Diagnostics for level one.  Look at the plot of residuals against actual values hopefully no patterns, then look at qqplot.  Will help with normailty assumption of residuals and heterskedasticity.
```{r}
library(HLMdiag)
library(ggplot2)
model1 = lmer(y1 ~ time*intervention + (1|subject), data = d)
resid1_model1 = HLMresid(model1, level = 1, type = "LS", standardize = TRUE)
head(resid1_model1)

#Psuedo r^2 see Peugh(2010)
r2 = cor(resid1_model1$y1, resid1_model1$fitted)^2; r2

qplot(x = resid1_model1$y1, y = resid1_model1$LS.resid, data = resid1_model1, geom = c("point", "smooth"))

ggplot_qqnorm(resid1_model1$LS.resid, line= "rlm")
```
Level two evaluation
```{r}
model2 = lmer(y1 ~ time*intervention + (time|subject), data = d)
resid2_model2 = HLMresid(object = model2, level = "subject")
head(resid2_model2)

ggplot_qqnorm(resid2_model2$`(Intercept)`, line = "rlm")
ggplot_qqnorm(resid2_model2$time, line = "rlm")

cooks_model2 = cooks.distance(model2, group = "subject")
dotplot_diag(x = cooks_model2, cutoff = "internal", name = "cooks.distance")
```
Ok now let us run it in the simr.  Models two and three run fine.  Problem seems to be when we add multiple covariates I can only run the function with the first covariate entered and cannot seem to change it.

If I only have the interaction term, it won't run.  
```{r}
library(simr)
model6 = lmer(y1 ~ time + intervention + time*intervention + (time | subject), data = d)
summary(model6)
fixef(model6)["time:intervention"] = .25
set.seed(1234)
powerCurve(model6, along = "subject", nsim = 10, test = fixed("time:intervention"))
```

So how many clusters for 500 people so 50 people per cluster.  Need to include n in this somehow.  Cluster will be an each thing.  So the each for cluster needs to be divisable by the each for the subject.

Here the random assignment is at the cluster level.
n = number of people
nC = number of people per cluster 50*4 because there are 50 people per cluster over four times points so 200 total data points per cluster
cluster = number of clusters

Now I am trying to replicate the two level study above, but with additional level of clustering.  So time points are nested in people, which are nested in clusters.  I have 500 peoeple who are measured over four time points and are placed into 10 clusters.  Then I am randomizing people at the cluster level by assigning half of the clusters to the intervention and half not. 
```{r}
n = 500
clusterPoints = 20 
timepoints = 4
nCluster = (n*timepoints)/clusterPoints
time = rep(0:3, times=n)
subject = rep(1:n, each=4)
cluster = rep(1:clusterPoints, each = nCluster) 
treat = c(1,0)
set.seed(1211)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), clusterPoints)
intervention = rep(intervention, each = nCluster)
dat = data.frame(time, subject, cluster, intervention)
```
Now I am creating the random effects, which I believe that I have two set of one for time and the other subjects.  The same process as above for creating these.  There should be 1,000 random effects for time, because each person gets their own random intercept and slope.  Since there are 10 random effects then there needs to be 10 different random effects one for each cluster. 
```{r}
interceptT = 0
slopeT = .25
slopeTI = .25
randomEffectsCorrT = matrix(c(.3,.2,.2, .3), ncol = 2)
randomEffectsCorrT

randomEffectsT = mvrnonnorm(n = n, mu = c(0,0), Sigma = randomEffectsCorrT, empirical = TRUE)
randomEffectsT = data.frame(randomEffectsT)
colnames(randomEffectsT) = c("IntT", "SlopeT")
dim(randomEffectsT)

clusterPoints  = 10
interceptP = 0
slopeP = .1
randomEffectsCorrP = matrix(c(.3,.2,.2, .3), ncol = 2)
randomEffectsCorrP

randomEffectsP = mvrnonnorm(n = clusterPoints, mu = c(0,0), Sigma = randomEffectsCorrP, empirical = TRUE)
randomEffectsP = data.frame(randomEffectsP)
colnames(randomEffectsP) = c("IntP", "SlopeP")
randomEffectsP
```
This is the same as before, but I am adding the clustering random effects, which are provided for each cluster.  
I am stuck because I think I understand why we have randomEffectsT$SlopeT[subject]) multiplied by time, because differences in time mean something.  A 2 means you should have about double the time in treatment relative to a 1. Therefore, we would expect the results to be twice as different (either large or small) if they are in time point two.

If we follow the same logic, then there needs to be a random slope component for the subject, because people are nested in clusters and each cluster is allowed to have its own intercept and slope over the people in the cluster.  By multiplying the randomEffectsP$SlopeP[cluster]) by subject doesn’t have any meaning, because differences between subjects are meaningless, unlike time.  So the random effects are arbitrarily enlarged as the subject number gets larger.  

$$  Mixed Model:~~ {(\gamma_{000}+ \gamma_{001}Intervention_{k}+u_{0jk} +u_{00k})+ (\gamma_{100} +  \gamma_{101}Intervention_{k}+ u_{1jk} + u_{10k})*Time_{ijk}+e_{ijk}}~~(1.6)$$

```{r}
sigma = .05
y1 = (interceptT + randomEffectsP$IntP[cluster] +randomEffectsT$Int[subject])+(slopeT + randomEffectsP$SlopeP[cluster] + randomEffectsT$SlopeT[subject])*time + slopeI*intervention + slopeTI*intervention*time + rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, cluster, intervention, y1)
d = round(d, 2)
head(d)
head(randomEffectsP$IntP[cluster])

```
So now we put together the model and see if we can recover the parameters.
```{r}
library(lme4)
model1 = lmer(y1 ~ time*intervention + (1 | cluster/subject), data = d)
summary(model1)
```
Here is the model for the level three with clustering added
$$ Level~1:~~~{y_{ijk} = \beta_{0jk} + \beta_{1jk}Time_{ijk} + e_{ijk}}~~~ (1.1)$$


$$ Level~2~Intercept:~~~{\beta_{0jk} = \gamma_{00k} + u_{0jk}} ~~~ (1.2)$$
$$ Level~2~Slope:~~~{\beta_{1jk} = \gamma_{10k} + u_{1jk}} ~~~ (1.3)$$

$$ Level~3~Intercept:~~~{\gamma_{00k} = \gamma_{000} +  \gamma_{001}Intervention_{k}+ u_{00k}} ~~~ (1.4)$$

$$ Level~3~Slope:~~~{\gamma_{10k} = \gamma_{100} +  \gamma_{101}Intervention_{k}+ u_{10k}} ~~~ (1.5)$$

$$  Mixed Model:~~ {(\gamma_{000}+ \gamma_{001}Intervention_{k}+u_{0jk} +u_{00k})+ (\gamma_{100} +  \gamma_{101}Intervention_{k}+ u_{1jk} + u_{10k})*Time_{ijk}+e_{ijk}}~~(1.6)$$
The trick is to take the final intercepts and slope recreate them and multiple the slopes by the level one variables and add the error terms for level two.

Loop for three level: This is correct
```{r}
library(semTools)
power_matt = function(){
n = 1000
clusterPoints = 20 
timepoints = 4
nCluster = (n*timepoints)/clusterPoints
time = rep(0:3, times=n)
subject = rep(1:n, each=timepoints)
cluster = rep(1:clusterPoints, each = nCluster) 
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), clusterPoints)
intervention = rep(intervention, each = nCluster)
dat = data.frame(time, subject, cluster, intervention)

interceptT = 0
slopeT = .25
slopeTI = 20
randomEffectsCorrT = matrix(c(.2,.2,.2, .2), ncol = 2)
randomEffectsCorrT

randomEffectsT = mvrnonnorm(n = n, mu = c(0,0), Sigma = randomEffectsCorrT, empirical = TRUE)
randomEffectsT = data.frame(randomEffectsT)
colnames(randomEffectsT) = c("IntT", "SlopeT")
dim(randomEffectsT)

clusterPoints  = 10
interceptP = 0
slopeP = .25
randomEffectsCorrP = matrix(c(.2,.2,.2,.2), ncol = 2)
randomEffectsCorrP

randomEffectsP = mvrnonnorm(n = clusterPoints, mu = c(0,0), Sigma = randomEffectsCorrP, empirical = TRUE)
randomEffectsP = data.frame(randomEffectsP)
colnames(randomEffectsP) = c("IntP", "SlopeP")
randomEffectsP

sigma = .05
y1 = (interceptT + randomEffectsP$IntP[cluster] +randomEffectsT$Int[subject])+(slopeT + randomEffectsP$SlopeP[cluster] + randomEffectsT$SlopeT[subject])*time + slopeI*intervention + slopeTI*intervention*time + rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, cluster, intervention, y1)
d = round(d, 2)

model1 = lmer(y1 ~ time*intervention + (1 | cluster / subject), data = d)
model1_summary = summary(model1)

}
```



Now replicate it
```{r}
reps = 50
power_rep = replicate(reps, power_matt())
power_rep_df = data.frame(power_rep)
power_rep_df_mean = rowMeans(power_rep_df)
power_rep_df_mean
```


