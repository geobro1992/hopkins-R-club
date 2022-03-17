############################
# regression with count data
############################
library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggthemes)
library(pscl)
library(lmtest)
library(MASS)
library(jtools)
library(emmeans)
# Might need to install from R-Forge instead of CRAN
# install.packages("countreg", repos="http://R-Forge.R-project.org")
library(countreg)


#######################################
# read in data and vizualize count data
#########################################

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


# summarizing individual columns
unique(dat$Sex)
table(dat$Sex)


# cleaning up nest column
dat$With_Nest = recode_factor(dat$With_Nest, `N` = "No", `No` = "No", 
                        `Yes` = "Yes",
                        .default = NA_character_)


# check cleaning worked
table(dat$With_Nest)


# histograms are best way to check numeric data
hist(dat$SVL.cm.)
hist(dat$Mass.g.)

dat[which(dat$SVL.cm. < 0),"SVL.cm."] = NA
dat[which(dat$Mass.g. < 0),"Mass.g."] = NA

# check it worked
hist(dat$SVL.cm.)
hist(dat$Mass.g.)

# histograms are best way to check count data too!
hist(dat$Leeches_Total)

dat[which(dat$Leeches_Total < 0),"Leeches_Total"] = NA

# check it worked
hist(dat$Leeches_Total)

# remove NA rows
dat = dat[!is.na(dat$River),]
dat = dat[!is.na(dat$With_Nest),]
dat = dat[!is.na(dat$Leeches_Total),]
dat = dat[!is.na(dat$SVL.cm.),]

##############
# count models

ggplot(dat, aes(Leeches_Total)) +
  geom_histogram() +
  facet_wrap(~With_Nest)


# create formula to use in all models
formula = Leeches_Total ~ With_Nest + SVL.cm.

####################
# poisson model

modelPoisson <- glm(formula = formula,
                    family  = poisson(link = "log"),
                    data    = dat)

summary(modelPoisson)

## Exponentiated coefficients
exp(coef(modelPoisson))

#Interpretation: The "baseline" average Leech count is 0.94. 
#The other exponentiated coefficients are interpreted multiplicatively. 
#One unit increase in SVL increases the average leeches by 1.02 times. 
#beng with a nest reduces the average leeches by 0.78 times. 

## calculate and store predicted values
dat$phat <- predict(modelPoisson, type="response")

## order by program and then by math
dat <- dat[with(dat, order(SVL.cm., With_Nest)), ]

## create the plot
ggplot(dat, aes(x = SVL.cm., y = phat, colour = With_Nest)) +
  geom_point(aes(y = log(Leeches_Total)), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "SVL", y = "Expected Number of Leeches")


# rootograms are a quick and dirty way to assess fit of count models
rootogram(modelPoisson, max = 80) # fit up to count 80

# Goodness-of-fit test
gof.pvalue = 1 - pchisq(modelPoisson$deviance, modelPoisson$df.residual)
gof.pvalue

###########################
## quasi-Poisson model

modelQuasiPoisson <- glm(formula = formula,
                         family  = quasipoisson(link = "log"),
                         data    = dat)

summary(modelQuasiPoisson) #The dispersion parameter is greater than 1, indicating overdispersion

#########################
## negative binomial model

modelNB <- glm.nb(formula = formula,
                  data    = dat)

summary(modelNB)

## Exponentiated coefficients
exp(coef(modelNB))

# rootograms are a quick and dirty way to assess fit of count models
rootogram(modelNB, max = 80) # fit up to count 80

AIC(modelPoisson)
AIC(modelNB)

lrtest(modelPoisson, modelNB)

################
## hurdle model

#hurdle models have two components: 
#A truncated count component, such as Poisson, geometric or negative binomial, is employed for positive counts, 
#and a hurdle (binary) component models zero vs. larger counts.

modelHurdle <- hurdle(formula = formula,
                      dist    = "negbin",
                      data    = dat)
summary(modelHurdle)

## Exponentiated coefficients
expCoef <- exp(coef((modelHurdle)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- names(coef(modelHurdle))[1:3]
colnames(expCoef) <- c("Count_model","Zero_hurdle_model")
expCoef


## just to prove that the hurdle component of the hurdle model is a logistic regression:
hurdlePart <- glm(formula = I(Leeches_Total>0) ~ With_Nest + SVL.cm.,
                  data    = dat,
                  family  = binomial(link = "logit"))


summary(modelHurdle)
summary(hurdlePart)

#
# the predictors for the 2 components of a hurdle model do not have to be the same 
#

## hurdle part without health
modelHurdleSimpler <- hurdle(formula = Leeches_Total ~ With_Nest | With_Nest*SVL.cm.,
                             dist    = "negbin",
                             data    = dat)

summary(modelHurdleSimpler)

## LRT to compare these models
lmtest::lrtest(modelHurdle, modelHurdleSimpler)

# quick assessment of model fit
rootogram(modelHurdleSimpler)

## Exponentiated coefficients
expCoef.H <- exp(coef((modelHurdleSimpler)))
expCoef.H <- data.frame("Count_model" = c(expCoef.H[1:2], NA, NA), 
                      "Zero_hurdle_model" = expCoef.H[3:6])
expCoef.H

#----------------------------------
# plot raw data for positive counts
plot(log(Leeches_Total) ~ With_Nest, data = dat, subset = Leeches_Total > 0,
     main = "Count")

# !!!be careful of the type of prediction!!!
predict(modelHurdleSimpler, newdata = data.frame(With_Nest = c("No", "Yes"), SVL.cm. = rep(mean(dat$SVL.cm.),2)),
        type = "count")
predict(modelHurdleSimpler, newdata = data.frame(With_Nest = c("No", "Yes"), SVL.cm. = rep(mean(dat$SVL.cm.),2)),
        type = "response")

# make new data to predict from
newdata <- data.frame(With_Nest=factor(rep(levels(dat$With_Nest),100)), 
                      SVL.cm. = seq(min(dat$SVL.cm.), max(dat$SVL.cm.), length.out = 200))

newdata$prob <- predict(modelHurdleSimpler, newdata=newdata, type = 'response')

# plot results for positive counts
ggplot(dat, aes(With_Nest, log(Leeches_Total))) +
  geom_violin(aes(fill = With_Nest)) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) + 
  geom_tufteboxplot(data = newdata, aes(With_Nest, prob),
                    median.type = "line", 
                    whisker.type = 'line',
                    hoffset = 0, width = 3)


# plot results for hurdle prt of the model
hurdlePart <- glm(formula = I(Leeches_Total>0) ~ With_Nest * SVL.cm.,
                  data    = dat,
                  family  = binomial(link = "logit"))

summary(hurdlePart)

# create new data for predictions
newdata <- data.frame(With_Nest=factor(rep(levels(dat$With_Nest),100)), 
                                     SVL.cm. = seq(min(dat$SVL.cm.), max(dat$SVL.cm.), length.out = 200))

newdata$prob <- predict(hurdlePart, newdata=newdata, type = 'response')
newdata$se <- predict(hurdlePart, newdata=newdata, type = 'response', se.fit = TRUE)$se.fit

newdata$LoCI <- newdata$prob - newdata$se
newdata$HiCI <- newdata$prob + newdata$se
  
  
ggplot(newdata, aes(x=SVL.cm., y=prob, color=With_Nest)) + 
  geom_line(lwd=2) + 
  geom_smooth(
    aes(ymin = LoCI, ymax = HiCI,
        fill = With_Nest, colour = With_Nest),
        stat = "identity") +
  ylim(0,1) +
labs(x="SVL", y="P(Leeches)", title="Probability of >0 Leeches")+
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  scale_color_manual(values=c("#56B4E9", "#E69F00"))
  
  
############################
# zero-inflated models

# ZI models also have two components. However, both components predict zero counts. 
#The count model predicts some zero counts, and on the top of that the zero-inflation binary model part adds zero counts, thus, the name zero "inflation". 
#This is more suitable for situations where there are two types of zeros, i.e., structural zeros and sampling zeros. 
#Those who have the structural zeros are the people who can only have zeros. 
#Those who have sampling zeros are the people who could have experienced non-zero outcome, but by chance experienced zeros.

## zeroinflated negatvie binomial
modelZeroInfl <- zeroinfl(formula = Leeches_Total ~ With_Nest | With_Nest*SVL.cm.,
                          dist    = "negbin",
                          data    = dat)

summary(modelZeroInfl)


# not strictly an appropriate comparison, but just to show that
# hurdle and zero inflated models will give you roughly the same fit.
# the important distinction is the nature of the zeroes
AIC(modelHurdleSimpler)
AIC(modelZeroInfl)
rootogram(modelHurdleSimpler)
rootogram(modelZeroInfl)

#####################
# now it's your turn!

# 1. with your own count data or mock data, perform a simple poisson regression

# 2. assess your date for overdispersion by comparing poisson model with quasi-poisson / negativebinomial models

# 3. fit hurdle and zero-inflated models to the same data

# 4. compare hurdle models with different predictors for the hurdle component
