#######################
# advanced mixed models
#######################

library(lme4)
library(ggplot2)
library(ggpubr)
library(gridExtra)

#######################################
# built in dataset on sleep deprivation
?sleepstudy
head(sleepstudy,20)
summary(lm(Reaction~Days, data = sleepstudy))

# basic linear regression
ggplot(sleepstudy,aes(x=Days,y=Reaction)) + geom_point() + geom_smooth(method = "lm")


# as always, we can create the same prediction plot from scratch 
# and make it pretty in base R
op <- par(mfrow = c(1, 1), mar = 0.1 + c(4, 4, 1, 1), oma = c(0, 0, 2, 0), cex.lab = 1.5, font.lab = 1.5, cex.axis = 1.3, bty = "n", las = 1, cex.main = 1.5)

plot(Reaction ~ Days, data = sleepstudy, col = "black", pch = 21, bg = "grey", cex = 1.5, 
     xlim = c(0, 9), ylim = c(150, 500), ylab = "", xlab = "", axes = F)

axis(1, at = seq(0, 9, 1))
axis(2)

conf_interval <- predict(lm(Reaction ~ Days, data = sleepstudy), 
                         newdata = data.frame(Days = seq(0, 9, by = 0.1)), 
                         interval = "confidence", level = 0.95)

lines(seq(0, 9, by = 0.1), conf_interval[,1], lty = 1, lwd = 2)
lines(seq(0, 9, by = 0.1), conf_interval[,2], lty = 2, lwd = 2)
lines(seq(0, 9, by = 0.1), conf_interval[,3], lty = 2, lwd = 2)


# BUT we have non-independent data, because each individual in the study 
# has 10 observations 
ggplot(sleepstudy,aes(x=Days,y=Reaction)) + 
  geom_smooth(method = "lm",level = 0.95) + 
  geom_point() + 
  facet_wrap(~Subject, nrow = 3, ncol = 6) # separate by individual


# from the above plot, a random slope model seems most appropriate
summary(lmer(Reaction ~ Days + (Days | Subject), sleepstudy))

# we can compare the residual variance not explained by the 2 models
sqrt(sum(residuals(lm(Reaction~Days,data=sleepstudy))^2)/(dim(sleepstudy)[1]-2))
sqrt(sum(resid(lmer(Reaction~Days+(Days|Subject),sleepstudy))^2)/(dim(sleepstudy)[1]-2))

# AIC/likelihood will tell you the same thing
# BUT remember, the decision to include random effects or not
# should already have been made
fit1 <- lm(Reaction ~ Days, data = sleepstudy)
fit2 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)
anova(fit2, fit1)


###############################
# deriving CIs for mixed models

# getting global confidence intervals for mixed models can be tricky
# because you have to find away to incorporate the additional variability
# introduced by your random effects

# BOOTSTRAPPING is the most reliable / common way

# number of iterations
N_boot <- 1000
#create storage vectors for intercepts and slopes of each iteration 
intercept_fixef <- vector() 
slope_fixef <- vector()

# population level data for prediction
newdata_pop_level <- data.frame(Days = seq(0, 9, by = 1))

# storage vector for fixed effect predictions
datapoints_fixef <- matrix(ncol = N_boot, nrow = dim(newdata_pop_level)[1])

# individual level data for prediction
newdata_individ_level<-data.frame(Days=sleepstudy$Days, Subject=sleepstudy$Subject)

# storage vector for random effects predictions
datapoints_individ_level <- matrix(ncol=N_boot, nrow=dim(newdata_individ_level)[1])

# bootstrapped slope and intercept for global model
for(i in 1:N_boot) {
    
  # randomly sample data set (with replacement) 
    sleepstudy_boot <- sleepstudy[sample(1:dim(sleepstudy)[1], dim(sleepstudy)[1]*0.25, 
                                       replace = TRUE),]

  # fit same mixed model with subset of data  
    lmerfit_boot <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy_boot)

  # extract intercept (for fixed effect) and append to storage vector  
    intercept_fixef <- append(intercept_fixef, as.numeric(fixef(lmerfit_boot)[1]))

  # extract slope (for fixed effect) and append to storage vector  
    slope_fixef <- append(slope_fixef, as.numeric(fixef(lmerfit_boot)[2]))
  
  # predict model for all subjects (even those not included in the subset)  
    datapoints_individ_level[,i] <- predict(lmerfit_boot, newdata_individ_level, 
                                          allow.new.levels = TRUE)

} # close i loop


# plot bootstrapped intercepts and slopes
hist(intercept_fixef, breaks = 200)
hist(slope_fixef, breaks = 200)

# combine bootstrapped intercepts and slopes
fixef_df <- data.frame(Intercept = intercept_fixef, Days = slope_fixef)

# predict for each iteration of intercept and slope 
for(i in 1:N_boot) {

    datapoints_fixef[,i] <- 
    model.matrix(~ 1 + Days, data = newdata_pop_level) %*% as.matrix(fixef_df)[i,]

} #close i loop

# calculate 95% CIs from bootstrapped values (full model)
bootstrap_conf_interval_pop_level <- data.frame(Days = seq(0, 9, by = 1),
                                                lwr=apply(datapoints_fixef,1,quantile,prob=.05),
                                                fit=apply(datapoints_fixef,1,quantile,prob=.5),
                                                upr=apply(datapoints_fixef,1,quantile,prob=.95))


#PLOT POPULATION LEVEL CONFIDENCE INTERVALS
p1 <- ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + 
  geom_abline(data=fixef_df,alpha=0.1,size=2,aes(intercept=Intercept,slope=Days)) + 
  geom_smooth(method = "lm", color = "blue", size = 0.5) +
  theme_pubr()

p2 <- ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue", size = 0.5) + 
  geom_line(data=bootstrap_conf_interval_pop_level,aes(y=fit),size=0.5,color="red") + 
  geom_line(data=bootstrap_conf_interval_pop_level,aes(y=lwr),lty=2, color = "red") + 
  geom_line(data=bootstrap_conf_interval_pop_level,aes(y=upr),lty=2, color = "red") +
  theme_pubr()

grid.arrange(p1, p2, nrow = 1)


# calculate 95% CIs from bootstrapped values (for each individual)
bootstrap_conf_interval_individ_level <- data.frame(Days = sleepstudy$Days, 
                                                    Subject = sleepstudy$Subject,
                                                    lwr = apply(datapoints_individ_level, 1, 
                                                                quantile, prob = 0.05),
                                                    fit = apply(datapoints_individ_level, 1, 
                                                                quantile, prob = 0.5),
                                                    upr = apply(datapoints_individ_level, 1, 
                                                                quantile, prob = 0.95))

#PLOT INDIVIDUAL LEVEL CONFIDENCE INTERVALS
ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point(size = 1) + 
  geom_smooth(method = "lm", level = 0.95, size = 0.5) + 
  facet_wrap(~Subject, nrow = 3, ncol = 6) + 
  geom_line(data = bootstrap_conf_interval_individ_level,aes(y=fit), size = 0.5, 
            color = "red") + 
  geom_line(data = bootstrap_conf_interval_individ_level,aes(y=lwr),lty=2,size=0.5, 
            color = "red") + 
  geom_line(data = bootstrap_conf_interval_individ_level,aes(y=upr),lty=2,size=0.5, 
            color = "red") +
  theme_pubr()

# the individual fits are "shrunk" toward their common population level mean 
# all the fits help each other to have more stable and resembling 
# population level slopes, intercepts and confidence intervals
