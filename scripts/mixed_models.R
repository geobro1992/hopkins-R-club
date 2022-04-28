#####################
# linear mixed models
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(lubridate)
library(readxl)
library(emmeans)
library(modelr)
library(lme4)
library(stargazer)

# read in database
dat = read.csv("hellbender_capture_data.csv")

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

# remove -999s from size columns
dat[which(dat$SVL.cm. < 0),"SVL.cm."] = NA
dat[which(dat$Mass.g. < 0),"Mass.g."] = NA

# check it worked
hist(dat$SVL.cm.)
hist(dat$Mass.g.)

########################################
# basic regression of body size and mass

dat2 = dat %>% 
  dplyr::select(Site = "Site", River = "River", svl = "SVL.cm.", mass = "Mass.g.")

dat2 = dat2[complete.cases(dat2),]

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
  ggplot(aes(svl,mass)) + geom_point(col='grey') + 
  geom_line(aes(y=exp(pred)), size = 1.5)

# maybe the relationship is different across sites?
boxplot(svl ~ River, data = dat2)

#-------------------------
# add river as a predictor
mod2 = lm(log(mass) ~ log(svl) + River, data = dat2)
summary(mod2)

# pairwise comparison of river
emm = emmeans(mod2, pairwise ~ River)
emm

# get predictions for plotting
dat2 = dat2 %>% 
  add_predictions(mod2) %>% 
  add_residuals(mod2) 

## plot predictions, note we need transform predictions back to non-log space
dat2 %>%   
  ggplot(aes(svl,mass, colour = River)) + 
  geom_point(aes(colour=River), alpha = 0.2) + 
  geom_line(aes(y=exp(pred), colour = River), size = 1.5)


# creating facets is usually helpful if you have many groups
dat2 %>%   
  ggplot(aes(svl,mass, colour = River)) + 
  geom_point(aes(colour=River), alpha = 0.2) + 
  geom_line(aes(y=exp(pred), colour = River), size = 1.5) + 
  facet_wrap(~River)

#####################################
## river as a random effect

# if we didn't care about individual rivers, 
# and just want to account for variation/non-independence among sites
mixed.lm = lmer(log(mass) ~ log(svl) + (1|River), data = dat2)

# same diagnostics plots as regular linear models
plot(mixed.lm)
qqnorm(resid(mixed.lm))
qqline(resid(mixed.lm))

# summary table has a couple of differences
summary(mixed.lm)

0.01031 / (0.01031 + 0.02018) # river explains 33% of the residual noise

# lme4 does not provide p-values (by choice)
# so if you want them you need another package
stargazer(mixed.lm, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

 

# plot predictions
(mm_plot <- ggplot(dat2, aes(x = svl, y = mass, colour = River)) +
    facet_wrap(~River, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dat2, pred2 = predict(mixed.lm)), aes(y = exp(pred2)), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

###############################################
# discussion on random intercepts vs random slopes
# see: https://mfviz.com/hierarchical-models/
################################################

# random slopes model specification
mixed.lm2 = lmer(log(mass) ~ log(svl) + (1+svl|River), data = dat2)
summary(mixed.lm2)
# Note that we've added two parameters to our original model: 
# one for random slopes, and another for the relationship (i.e., correlation) 
# between random intercepts and random slopes.

(mm_plot3 <- ggplot(dat2, aes(x = log(svl), y = log(mass), colour = River)) +
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dat2, pred2 = predict(mixed.lm2)), aes(y = pred2), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

anova(mixed.lm, mixed.lm2)


####################################
#discussion on crossed vs nested REs
# see: https://ourcodingclub.github.io/tutorials/mixed-models/
####################################

# add site as a nested random term
dat2$Site = as.factor(dat2$Site)
boxplot(svl ~ Site, data = dat2)

# reomove sites with less than 10 observations
fs = names(which(table(dat2$Site) < 10))

dat3 = dat2 %>%
  filter(!Site %in% fs) %>%
  droplevels()

mixed.lm3 = lmer(log(mass) ~ log(svl) + (1|River) + (1|Site), data = dat3)
summary(mixed.lm3)

(mm_plot3 <- ggplot(dat3, aes(x = svl, y = mass, colour = Site)) +
    facet_wrap(~River, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dat3, pred3 = predict(mixed.lm3)), aes(y = exp(pred3)), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# prdicting SEs from RE models doesn't work
predict(mixed.lm3, se.fit = T)

# ggpredict is probably the quickest way 
# to estimate uncertainty of RE models
ggpredict(mixed.lm3, terms = c("svl", "Site"), type = "re") 

p = ggpredict(mixed.lm3, terms = c("svl"), type = "re") 
plot(p)

# if ggpredict doesn't work, CIs have to be approximated, see:
# https://towardsdatascience.com/how-linear-mixed-model-works-350950a82911

#####################
# now it's your turn!

# 1. with your own data or mock data, repeat a regression you have already done

# 2. add a random intercept

# 3. add a random slope

# 4. build a model with nest random effects