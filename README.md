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

Library packages 
```{r}
library(ggplot2)
library(lme4)
library(lmerTest)
library(sjstats)
library(semTools)
library(MASS)
library(MuMIn)
library(rstanarm)
```
Two level two treatment to make sure replication of effect


Then identifying what levels of the variable produce an effect size using Kane (2016) of dividing the baseline sd by the effect for the treatment by time fixed effects

```{r}

n = 150
timepoints = 5
time = timepoints-1
time = rep(0:time, times=n)
subject = rep(1:n, each=timepoints)
treat = c(1,0)
intervention = sample(treat, replace = TRUE, prob = c(.5, .5), n)
intervention = rep(intervention, each = timepoints)
intercept = 20
slopeT = 7
slopeI = 7
slopeTI = 4
randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr
# ICC
randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "SlopeT")
# Individual error
sigma = 40
y1 = (intercept + randomEffects$Int[subject])+(slopeT + randomEffects$SlopeT[subject])*time + slopeI*intervention + slopeTI*time*intervention+ rnorm(n*timepoints, mean = 0, sd = sigma)
## Scale dependent variable or take the log for percentage
d = data.frame(subject, time, intervention, y1 = y1)
model1 = lmer(y1 ~ time*intervention + (1|subject), data = d)
model1_summary = summary(model1)
model1_summary$coefficients[,1]

```
Try getting the effects and make sure they are being replicated

```{r}
rep = 2
power_rep = replicate(rep, power_matt_two())
power_rep_df = data.frame(power_rep)
power_rep_df_mean = rowMeans(power_rep_df)
power_rep_df_mean

```
Now do a loop for different n's and different parameters
```{r}
# Create function
### All empty lists need to be outside the loop 
cohen_d_time_inter = list()
### Create empty data sets
time = list()
p_value = list()
intervention = list()
slopeTI = list()
randomEffects = list()
y1 = list()
subject = list()
d = list()
model_out = list()
model1_summary = list()
eff = list()
sd_base_pooled = list()
cohen_d_ti = list()
sd_base_pooled_0 = list()
sd_base_pooled_1 = list()
n_sample = rep(c(seq(from = 300, to = 500, by =50)), each =5)
### This is the number of times to rep for the full simluation
n_rep = 1
n_sample = rep(n_sample, n_rep)
rep_eff = as.list(rep(c(2,3,4,5,6), length(n_sample) / 5))
for(i in 1:length(n_sample)){
#### Data generate
timepoints = 5
timepoints = timepoints-1
time[[i]] = rep(0:timepoints, times=n_sample[[i]])
subject[[i]] = rep(1:n_sample[[i]], each=timepoints+1)
treat = c(1,0)
intervention[[i]] = sample(treat, replace = TRUE, prob = c(.5, .5), n_sample[[i]])
intervention[[i]] = rep(intervention[[i]], each = timepoints+1)
intercept = 20
slopeT = 7
slopeI = 7
slopeTI[[i]] = rep_eff[[i]] 
randomEffectsCorr = matrix(c(.2), ncol = 1)
randomEffects[[i]] = mvrnonnorm(n_sample[[i]], mu = c(0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects[[i]] = data.frame(randomEffects[[i]])
colnames(randomEffects[[i]]) = c("Int")
sigma = 50
### Rep the random effects so there is one unique random intercept need +1 because you subtracted one from timepoints
randomEffects[[i]] = rep(randomEffects[[i]]$Int, each = timepoints+1)
y1[[i]] = (intercept + randomEffects[[i]])+(slopeT*time[[i]] + slopeI*intervention[[i]])+ rnorm(length(randomEffects[[i]]), mean = 0, sd = sigma)
d[[i]] = data.frame(subject[[i]], time[[i]], intervention[[i]], y1[[i]])
model_out[[i]] = lmer(y1[[i]] ~ time[[i]]*intervention[[i]] + (1 |subject[[i]]), data = d[[i]])
model1_summary[[i]] = summary(model_out[[i]])
### Need to grab p-values
p_value[[i]] =  ifelse(model1_summary[[i]]$coefficients[4,5] < .05, 1, 0)
### Divide by pooled baseline sd
eff[[i]] = model1_summary[[i]]$coefficients[4,1] 
sd_base_pooled[[i]] = subset(d[[i]], time..i.. == 0)
sd_base_pooled_0[[i]] = subset(sd_base_pooled[[i]], intervention..i.. == 0)
sd_base_pooled_0[[i]] = sd(sd_base_pooled_0[[i]]$y1..i..)^2
sd_base_pooled_1[[i]] = subset(sd_base_pooled[[i]], intervention..i.. == 1)
sd_base_pooled_1[[i]] = sd(sd_base_pooled_1[[i]]$y1..i..)^2
sd_base_pooled[[i]] = sqrt(sum(sd_base_pooled_0[[i]]+sd_base_pooled_1[[i]])/2)
sd_base_pooled[[i]] = sd(d[[i]]$y1..i..)
cohen_d_ti[[i]] = eff[[i]] / sd_base_pooled[[i]]
}
```
Get data set 
```{r}
p_value_results = unlist(p_value)
n_sample_results = unlist(n_sample)
rep_eff_results = unlist(rep_eff)
cohen_d_ti_results = unlist(cohen_d_ti)
data_results = round(data.frame(n = n_sample_results, effect_size =  rep_eff_results, p_value_results, cohen_d_ti_results),2)
library(dplyr)
data_power =  data_results %>%
  group_by(n, effect_size) %>%
  summarise(power = sum(p_value_results)/n_rep, cohen_d = mean(cohen_d_ti_results))
data_power
write.csv(data_power, "data_power.csv", row.names = FALSE)
```
Now plot the results
```{r}
library(ggplot2)
data_power %>%
  ggplot( aes(x=n_sample_results, y=power, color = rep_eff_results)) +
    geom_line()+
    geom_hline(yintercept=.8, linetype="dashed", color = "red")+
  ggtitle("Figure 1: SEASA power analysis")

```


