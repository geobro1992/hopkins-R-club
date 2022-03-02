#########################################
# cleaning real data-sets and basic stats

library(tidyverse)
library(ggplot2)

# read in database
dat = read.csv("hellbender_capture_data.csv")


# some useful base functions to check databases
colnames(dat)
str(dat)
summary(dat)


# summarizing individual columns
unique(dat$River)
table(dat$River)


# cleaning up river column
dat$River = recode_factor(dat$River, `Middle fork holston` = "mid.fork", `Middle fork Holston` = "mid.fork", `Middle Fork Holston` = "mid.fork",
                          `North fork holston` = "north.fork", `North Fork Holston` = "north.fork", 
                          `south fork holston` = "south.fork", `South fork holston` = "south.fork", `South fork Holston` = "south.fork",
                          `South Fork holston` = "south.fork", `South Fork Holston` = "south.fork", `South Fork Holston River` = "south.fork",
                          `Whitetop Laurel creek` = "whitetop", `Whitetop Laurel Creek` = "whitetop",
                          .default = NA_character_)


# check cleaning worked
table(dat$River)

# remove rivers with too few data points
dat = dat[!is.na(dat$River),]


# same naming problems with sites within rivers!! but let's skip for now
table(dat$River, dat$Site)


# histograms are best way to check numeric data
hist(dat$SVL.cm.)
hist(dat$Mass.g.)

dat[which(dat$SVL.cm. < 0),"SVL.cm."] = NA
dat[which(dat$Mass.g. < 0),"Mass.g."] = NA

# check it worked
hist(dat$SVL.cm.)
hist(dat$Mass.g.)


########################################
# basic regression of body size and mass

dat2 = dat %>% 
  select("River", svl = "SVL.cm.", mass = "Mass.g.")

# always plot your data first!
ggplot(dat2, aes(x = svl, y = mass)) +
  geom_point() +
  stat_smooth(method = "lm")

# log-log relationships are more common for this type of data
ggplot(dat2, aes(x = log(svl), y = log(mass))) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x)

# single predictor model
mod1 = lm(log(mass) ~ log(svl), data = dat2)
summary(mod1)

# diagnostic plots - see https://data.library.virginia.edu/diagnostic-plots/ for details
plot(mod1)


# get residuals from model
library(modelr)

dat2 = dat2 %>% 
  add_residuals(mod1) 


# locate abberant value and remove it
dat2[which(dat2$resid < -2),]
dat2 = dat2[-which(dat2$resid < -2),]

# rerun analysis without funky data
mod1 = lm(log(mass) ~ log(svl), data = dat2)
summary(mod1)

dat2 = dat2 %>% 
  add_predictions(mod1) %>% 
  add_residuals(mod1) 

## plot predictions, note we need transform predictions back to non-log space
dat2 %>%   
  ggplot(aes(svl,mass)) + geom_point(col='grey') + geom_line(aes(y=exp(pred)), size = 1.5)


#-------------------------
# add river as a predictor
mod2 = lm(log(mass) ~ log(svl) + River, data = dat2)

# diagnostic plots - see https://data.library.virginia.edu/diagnostic-plots/ for details
plot(mod2)

dat2 = dat2 %>% 
  add_predictions(mod2) %>% 
  add_residuals(mod2) 

## plot predictions, note we need transform predictions back to non-log space
dat2 %>%   
  ggplot(aes(svl,mass, colour = River)) + geom_point(aes(colour=River), alpha = 0.2) + geom_line(aes(y=exp(pred), colour = River), size = 1.5)

##################
# model comparison

# check outputs - River looks important!
summary(mod1)
summary(mod2)

# pairwise comparison to identify the significantly different rivers
library(emmeans)
emm = emmeans(mod2, pairwise ~ River)
emm


#--------------
# using anova()
anova(mod1, mod2) # the very small p-value means that adding river to the model did lead to a significantly improved fit over mod1
anova(mod2)


#------------------------------
# using a likelihood ratio test

# libray lmtest has a built in function for this
library(lmtest)
lrtest(mod1, mod2)

# but always good to know where the numbers come from
LL1 = as.numeric(logLik(mod1))
LL2 = as.numeric(logLik(mod2))

teststat = -2 * (LL1 - LL2)

p.val <- pchisq(teststat, df = 3, lower.tail = FALSE) # the tiny p-value says we should reject the null hypothesis that these models are equivalent and accept mod2


#-----------------------------------------
# comparing AICs gives the same conclusion
AIC(mod1)
AIC(mod2)


###################################################################
# linear regression with binary response (GLM; logistic regression)


#-------------------------------------------------
# relationship between body size and abnormalities


# cleaning up abnormalities column
table(dat$Abnormalities.Y.N..)

dat$abs = recode_factor(dat$Abnormalities.Y.N.., 
                                        y = "y", Y = "y", Yes = "y",
                                        n = "n", no = "n", N = "n", No = "n",
                                        .default = NA_character_)


dat.abs = dat %>%
  select("River", svl = "SVL.cm.", "abs")

# filter record with data
dat.abs = dat.abs[!is.na(dat.abs$abs),]


#convert yes' and nos to 0s and 1s
dat.abs = dat.abs %>%
  mutate(abs = ifelse(abs == "y", 1, 0))

# plot data first!
ggplot(dat.abs, aes(x = svl, y = abs)) +
  geom_count(alpha = 0.5)

# regression
mod.abs = glm(abs ~ svl, data = dat.abs, family = "binomial")
summary(mod.abs)

# logistic regression in ggplot
ggplot(dat.abs, aes(x = svl, y = abs)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Body Size (SVL)", y = "Probability of Abnormalities") +
  theme_classic()

# likelihood ratio test to see if our model outperforms the null model
with(mod.abs, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))


#------------------------------------------------------------
# a slightly more complicated model with River as a covariate
mod.abs.2 = glm(abs ~ svl + River, data = dat.abs, family = "binomial")
summary(mod.abs.2)


# wald test for overall effect of river (eqiuvalent to z test but for multiple parameters)
library(aod)
wald.test(b = coef(model2), Sigma = vcov(model2), Terms = 3:5)


# calculate odds ratios and confidence intervals
odds = exp(cbind(odds = coef(model2), confint(model2)))

#-------------------------------------
# predicted probabilities for plotting

# create new mock data for clean predictions
newdata = data.frame(svl = rep(seq(5,50, length.out = 100),4), River = rep(levels(dat.abs$River), each = 100))

# combine new data with predictions on new data
newdata = cbind(newdata, predict(mod.abs.2, newdata, type = "link", se = TRUE))

# calculate predicted probabilities and associated CIs (back transform predictions)
newdata = within(newdata, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})


# plot predicted probs and associated confidence intervals
ggplot(newdata, aes(x = svl, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = River), alpha = 0.2) +
  geom_line(aes(colour = River), size = 1)


#####################
# now it's your turn!

# 1. with your own data or mock data, perform a simple linear regression (continuous response, 1 predictor)

# 2. with the same data, perform a multiple linear regression (more than 1 predictor)

# 3. compare the models using an appropriate method

# 4. plot the predicted values of the best model

# 5. repeat steps 1-4 but with a binary response variable (logistic regression)
