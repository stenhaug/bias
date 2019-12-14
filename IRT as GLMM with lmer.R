#### Questions ####

## 1- Wrapping mind around difference between fixed and random effects in this framework (difference between x and x2 below) [I think fixed uses different intercept and random says it comes from a normal distribution]
## 2- When we have a fixed effect and a random effect, why can't take out the fixed effect? (go from x to x3)
## 3- Interpreting (-1 + mode | item)
## 4- How to do a dif analysis and how to think about in this context 
## 5- Don't totally get individual differences modeling idea either

#### Framework #####

## 1) Use linear combination of covariates and effects to create linear component value N_pi 
## 2) Use a linking component to map N_pi to probabiliy PI_pi
## 3) PI_pi is the expected value of Y_pi which we model as bernoulli

## External covariate is what we typically think of (gender)... Internal covariate is related to repsonses like number of previous successful responses
## Random effects vary by something that is observable, fixed effects do not

## Think of the 1-PL model as having fixed effects for each item and a random effect for each person (ability)

# Breaking down a general call: lmer(ir ~ -1 + item + (1|id), family = binomial("logit"), data = DataSet)

# -1 says don't use the first item for the intercept
# item says use fixed effects for items
# (1|id) says use random effects for people

#### Model Fitting #### 

library(lme4)
head(VerbAgg)

x = glmer(r2 ~ -1 + btype + mode + situ + (1|id) + (-1 + mode | item), data = VerbAgg, family = binomial)
x2 = glmer(r2 ~ -1 + btype + mode + situ + factor(id) + (-1 + mode | item), data = VerbAgg, family = binomial)
x3 = glmer(r2 ~ -1 + btype + situ + (1|id) + (-1 + mode | item), data = VerbAgg, family = binomial)

# Reading the x call above: -1 = don't use first item, btype = fixed effect for btype, mode = fixed effect for mode, 1|id = random effect for item,  mode | item = random effect of mode over items)

#### Look at our model #### 
print(x)
summary(x)

# Pull out random effects values
coef(x)$id
coef(x)$item

# Make sure I'm understanding predictions
predict(x)[1]
v = -0.458 - 0.3765 + 1.7196 + 0 + 0
v
predict(x)[1:100]

hist(inv.logit(predict(x)))
library(boot)
inv.logit(0)

#### Diff analysis #### 

dif = with(VerbAgg, factor(0 + (Gender == "F" & mode == "do" & btype != "shout")))

difx = glmer(r2 ~ -1 + item + dif + Gender + (1 | id), data = VerbAgg, family = binomial)

#### Learning/Individual Differences

long <- data.frame(id = VerbAgg$id, item = VerbAgg$item, r2 = VerbAgg$r2) 
wide <- reshape(long, timevar = "item", idvar = "id", dir = "wide")[, -1] == "Y" 
VerbAgg$prosum = prosum <- as.vector(t(apply(wide, 1, cumsum)))

learningx = glmer(r2 ~ -1 + item + prosum + (1 | id), data = VerbAgg, family = binomial)
learningx2 = glmer(r2 ~ -1 + item + factor(prosum) + (1 | id), data = VerbAgg, family = binomial)
learningxIndDiff = glmer(r2 ~ -1 + item + prosum + (1 + prosum | id), data = VerbAgg, family = binomial)

summary(learningx)
summary(learningxIndDiff)

#### Let's run that back ####

# Here are fixed effects items (notice how we have to do dummy random effects but nothing there since we already have fixed effects)
simp = glmer(r2 ~ -1 + item + (1 | item), data = VerbAgg, family = binomial)

# Here is the classic 1PL model with item fixed effects and person random effects
simp2 = glmer(r2 ~ -1 + item + (1 | id), data = VerbAgg, family = binomial)

# Interesting it's not totally clear to my why we couldn't flip the script and have people as fixed effects and items as random effects (but that would take forever I guess!)
simp3 = glmer(r2 ~ -1 + id + (1 | item), data = VerbAgg, family = binomial)

# Things get interesting when I want to add in prosum which is a integer value (not a factor)
prosumFixed = glmer(r2 ~ -1 + item + (1 | id) + prosum, data = VerbAgg, family = binomial)

## Let's pause and make sure we understand how this model works to get linear component... Got it! 
pred2000 = -1.42 + 0.42854 * 4
predict(prosumFixed)[2000]

# What if we were to see prosum as a random effect?
prosumRandom = glmer(r2 ~ -1 + item + (1 | id) + (1|prosum), data = VerbAgg, family = binomial)

predict(prosumRandom)[2000]
-2.1777 + 2.78 - 0.48
