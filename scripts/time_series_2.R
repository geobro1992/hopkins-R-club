###########################################
# working with Becca's stream data - part 2
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(lubridate)
library(readxl)
library(countreg)
library(gamlss)

# read in logger data
dat_DO <- read_excel("431_DO.xlsx")

# extract just time from EST.time
dat_DO = dat_DO %>%
  mutate(Time = format(dat_DO$EST.time, format = "%H:%M")) 

# create date-time column
dat_DO = dat_DO %>%
  mutate(datetime = as.POSIXlt(paste(dat_DO$EST.date, dat_DO$Time)))

#filter just hourly data  
dat_DO_hourly = dat_DO %>%
  filter(minute(dat_DO$datetime) == "0") %>%
  mutate(day = factor(yday(datetime))) 


######################
# behavior data
dat_behav <- read_excel("behavior_data.xlsx")

# extract just time from EST.time
dat_behav = dat_behav %>%
  mutate(Time.s = format(dat_behav$EST.start, format = "%H:%M"),
         Time.e = format(dat_behav$EST.end, format = "%H:%M")) 

# create date-time columns for start and end times
dat_behav = dat_behav %>%
  mutate(datetime.s = as.POSIXlt(paste(dat_behav$Full.date, dat_behav$Time.s)),
         datetime.e = as.POSIXlt(paste(dat_behav$Full.date, dat_behav$Time.e)))

#filter just rocking behavior  
dat_rocking = dat_behav %>%
  filter(dat_behav$Behavior == "Rocking")%>%
  mutate(day = factor(yday(datetime.s))) 

# round behavior times to the hour (to match hourly DO data)
dat_rocking$datetime.s = round(dat_rocking$datetime.s, units = "hours")


###############################
# simple analyses of count data

# subset DO data to observation days
dat_DB = dat_DO_hourly %>%
  filter(datetime >= min(dat_rocking$datetime.s), datetime <= max(dat_rocking$datetime.s)) %>%
  droplevels()

# calculate counts, durations, and DO by hour
durations = aggregate(Dur.sec ~ as.factor(datetime.s), data = dat_rocking, FUN = sum)
n.fanning = aggregate(Dur.sec ~ as.factor(datetime.s), data = dat_rocking, FUN = length)
DO = aggregate(`Dissolved Oxygen` ~ as.factor(datetime), data = dat_DB, FUN = mean)

# duplicate behavior data 
#(I had to to mimic 2 individuals because of modelling issues)
durations = rbind(durations, durations)
n.fanning = rbind(n.fanning, n.fanning)
names(durations) = c("datetime", "duration")
names(n.fanning) = c("datetime", "count")
names(DO) = c("datetime", "DO")

# merge datasets
df = full_join(durations, n.fanning, by = "datetime")
df = full_join(df, DO, by = "datetime")

# convert NAs to zeros
df[is.na(df)] = 0

# inspect data
hist(df$count, breaks = 30)
hist(df$duration, breaks = 30)
hist(df$DO, breaks = 30)

##############################################
#logistic regression (0/1) of fanning behavior
df$binary = ifelse(df$count > 0, 1, 0)

m1 = glm(binary ~ DO, data = df, family = "binomial")
summary(m1)

## calculate and store predicted values
preds <- predict(m1, type="response", se.fit = T)

df$fit = preds$fit
df$lwr = preds$fit - 1.96 * preds$se.fit
df$upr = preds$fit + 1.96 * preds$se.fit

## order dataset for neat plotting
df <- df[with(df, order(DO, datetime)), ]

## create the plot
ggplot(df, aes(x = DO, y = fit)) +
  geom_point(aes(y = binary, alpha=.5)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin=lwr,ymax=upr),alpha=0.3) +
  labs(x = "DO", y = "Probability of Rocking") +
  theme_classic2()


###############################################
# hurdle model of behavioral counts
m2 = hurdle(count ~ DO, data = df, dist = "negbin")
summary(m2)

# create new data and predict probabilities for each count
newdat = data.frame(DO = seq(7.8, 11.5, length.out = 5))

pred.hurdle = as.data.frame(predict(
  hurdle(count~DO, dist = "negbin", link = "logit", data = df),
  newdata = newdat, type = "prob"))

pred.hurdle = pred.hurdle %>% gather(key = 'count', value = 'p.count', 0:86)

pred.hurdle$DO = rep(seq(7.8, 11.5, length.out = 5), length(unique(pred.hurdle$count)))
pred.hurdle$count = as.factor(as.numeric(pred.hurdle$count))

# probability of no rocking behavior given DO
p.zero = pred.hurdle[c(1:5),]

# (just recreating logistic regression plot)
ggplot(p.zero, aes(DO, 1-p.count)) +
  geom_line(size = 1.5) +
  ylim(0, 1) +
  theme_classic2()

# probabilities for all non-zero counts
pred.hurdle = pred.hurdle[-c(1:5),]

# plot predicted probabilities of counts for various levels of DO
ggplot(pred.hurdle, aes(x=count, y=p.count)) + 
  geom_bar(stat = "identity", width = 0.5) +
  xlab("Counts of Rocking Behavior (up to 85)") +
  ylab("Predicted Probability") +
  facet_wrap(~factor(DO), nrow = 1) +
  theme_classic2() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_flip() 

# add predicted means to plot
mean.preds = data.frame(DO = as.factor(newdat$DO), 
                        mean.full = predict(m2, newdata = newdat, type = "response"),
                        mean.count = predict(m2, newdata = newdat, type = "count"))

  ggplot(pred.hurdle, aes(x=count, y=p.count)) + 
    geom_bar(stat = "identity", width = 0.5) +
    geom_vline(data = mean.preds, aes(xintercept = mean.full), color = "red", size = 2) +
    geom_vline(data = mean.preds, aes(xintercept = mean.count), color = "blue", size = 2) +
    facet_wrap(~factor(DO), nrow = 1) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_flip() 

      
###################################################
# bootstrapped confidence intervals for hurdle model
library(boot)
  
# create functions to perform model fitting with replacement
hurdle_fn <- function(df, inds) {
  predict(
    hurdle(count~DO, dist = "negbin", link = "logit", data = df[inds, ]),
    newdata = newdat, type = "count")
}

res <- boot(df, hurdle_fn, R = 1000)

CI <- setNames(as.data.frame(t(sapply(1:nrow(newdat), function(row)
  boot.ci(res, conf = 0.95, type = "bca", index = row)$bca[, 4:5]))),
  c("lower", "upper"))

# merge predictions with data 
df_all <- cbind.data.frame(newdat, 
                           response = predict(m2, type = "count", newdata = newdat),
                           CI)

# plot predictions and data
ggplot(df_all, aes(DO, response)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymax = upper, ymin = lower, alpha = 0.3)) +
  geom_point(data = df, aes(DO, count)) +
  theme_classic()


##################################################
# beta model for proportion of time spent rocking

df$prop = df$duration/(60*60)

# zero inflated beta model of fanning durations
m3 <- gamlss(prop ~ DO, nu.formula = ~DO,  family = BEZI, data = df, trace = F)
summary(m3)


## calculate and store predicted values
df$phat <- predict(m1, type="response")

## order by program and then by math
df <- df[with(df, order(DO, day)), ]

## create the plot
ggplot(df, aes(x = DO, y = phat)) +
  geom_point(aes(y = count, alpha=.5)) +
  geom_line(size = 1) +
  labs(x = "SVL", y = "Expected Number of Leeches")






