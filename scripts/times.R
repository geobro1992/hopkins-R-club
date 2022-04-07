####################
# working with times
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(lubridate)

# read in database
dat = read.csv("hellbender_capture_data.csv")

# cleaning up river column
dat$River = recode_factor(dat$River, `Middle fork holston` = "mid.fork", `Middle fork Holston` = "mid.fork", `Middle Fork Holston` = "mid.fork",
                          `North fork holston` = "north.fork", `North Fork Holston` = "north.fork", 
                          `south fork holston` = "south.fork", `South fork holston` = "south.fork", `South fork Holston` = "south.fork",
                          `South Fork holston` = "south.fork", `South Fork Holston` = "south.fork", `South Fork Holston River` = "south.fork",
                          `Whitetop Laurel creek` = "whitetop", `Whitetop Laurel Creek` = "whitetop",
                          .default = NA_character_)


# cleaning up nest column
dat$With_Nest = recode_factor(dat$With_Nest, `N` = "No", `No` = "No", 
                              `Yes` = "Yes",
                              .default = NA_character_)


dat[which(dat$SVL.cm. < 0),"SVL.cm."] = NA
dat[which(dat$Mass.g. < 0),"Mass.g."] = NA
dat[which(dat$Hgb_1 < 0.1),"Hgb_1"] = NA

# remove NA rows
dat = dat[!is.na(dat$River),]
dat = dat[!is.na(dat$With_Nest),]
dat = dat[!is.na(dat$SVL.cm.),]

# as.Date() is the standard, base R function to convert an object or column to class Date
dat$txtDate
class(dat$txtDate)

# you must tell R the current format Date is in
?strptime

dat <- dat %>% 
  mutate(txtDate = as.Date(txtDate, format = "%d-%b-%y"))

class(dat$txtDate)
dat$txtDate

##########################
# times

# get current time
now()
now(tzone = "GMT")

# can convert times to a binary based on am/pm
am(now())
pm(now())

# date/times are of class POSIX
class(now())

# POSIXct stores date/time as number of seconds since Jan 1 1970
unclass(now())

# convert character string to POSIXct
random_datetime <- as.POSIXct("2019-12-12 08:05:03")
random_datetime

# POSIXlt stores information in a list
random_datetime2 <- as.POSIXlt("2019-12-12 08:05:03")
random_datetime2

unlist(random_datetime2)

random_datetime2$hour
random_datetime2$year # year is years since 1900


# arithmetic can still be done with date-times but gets a bit more complicated
# lubridate has nice functions to make it easier
random_datetime
random_datetime + 10
random_datetime + hours(10)
random_datetime + days(10)
random_datetime + years(10)


##########################
# hellbender capture times
?strptime
format <- "%Y-%m-%d %H:%M:%S"

# create new column for capture times
dat$captime = as.POSIXlt(paste(dat$txtDate, dat$Capture_Time..00.00.00..in.24hrs), 
           format=format)

hist(dat$captime$hour, breaks = 24)
hist(dat$captime$wday, breaks = 24)

# POSIXlt sometimes struggles with plotting, so we'll make 2 columns
dat$captime2 = as.POSIXct(paste(dat$txtDate, dat$Capture_Time..00.00.00..in.24hrs), 
                         format=format)


# create new column for bleed times
dat$bleedtime = as.POSIXlt(paste(dat$txtDate, dat$T0_Bleed_Time), 
                         format=format)

# difference between cap time and bleed time
dat$bleedtime - dat$captime

dat$timediff = as.numeric(dat$bleedtime - dat$captime)
hist(dat$timediff)

# for now lets just get rid of funky looking data
dat2 = dat[which(dat$timediff > 0 & dat$timediff < 1000),]
hist(dat2$timediff/60)

dat2$year = dat2$captime$year-100

# lets see in crew have got better at processing animals
aggregate(dat2$timediff/60 ~ dat2$year, FUN = mean)

m1 = glm(timediff ~ year, data = dat2, family = "poisson")
summary(m1)

# predict handling times for each year
newdat = data.frame(year = sort(unique(dat2$year)))

pred = predict(m1, newdata = newdat, se.fit = T, type = "response")

predframe = data.frame(year = sort(unique(dat2$year))+2000,
                  fit = pred$fit/60, 
                  upr = pred$fit/60 + pred$se.fit/60,
                  lwr = pred$fit/60 - pred$se.fit/60)

ggplot(predframe, aes(year, fit))+
  geom_line()+
  geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3) +
  xlab("Year") + ylab("Processing Time") +
  ylim(0,3) +
  theme_pubr()


# now lets forecast handling times into future years
library(forecast)

# create a time series object for mean handling time each year
mts <- ts(predframe$fit, start = decimal_date(ymd("2012-01-01")),
          frequency = 1)

# forecasting model using arima model (AR = autoregressive)
fit <- auto.arima(mts)

# Next 5 forecasted values
forecast(fit, 5)

# plotting the graph with next 5 years
# NOTE we are using a smooth line to forecast, so confidence is overly high
# AND forecasts are identical to model predictions
predict(m1, newdata = data.frame(year = 22:26), type = "response")/60

plot(forecast(fit, 5), xlab ="Year",
     ylab ="Processing Time", ylim = c(0,3))


# so lets add some noise to our predictions t see what happens

# create another time series object for handling time
# but add some random noise to each year
mts <- ts(predframe$fit + runif(10, -0.2, 0.2), start = decimal_date(ymd("2012-01-01")),
          frequency = 1)

# forecasting model using arima model
fit <- auto.arima(mts)

# Next 5 forecasted values
forecast(fit, 5)

# forecasts are no longer identical to model predictions
predict(m1, newdata = data.frame(year = 22:26), type = "response")/60

# plotting the graph with next 5 years
plot(forecast(fit, 5), xlab ="Year",
     ylab ="Processing Time", ylim = c(0,3))


###############################################
# AR models are more useful for non-linear data
data("AirPassengers")

#This will plot the time series
ts.plot(AirPassengers, xlab="Year", ylab="Number of Passengers", main="Monthly totals of international airline passengers, 1949-1960")

# add trend line
abline(reg=lm(AirPassengers~time(AirPassengers)))

#Fitting the AR Model to the time series
AR1 <- arima(AirPassengers, order = c(1,0,0))
print(AR1)

#Fitting the AR Model to the time series
AR2 <- arima(AirPassengers, order = c(2,0,0))
print(AR2)

#Fitting the AR Model to the time series
AR3 <- arima(AirPassengers, order = c(3,0,0))
print(AR3)

AIC(AR1)
AIC(AR2)
AIC(AR3)

# plot predicted vs observed
ts.plot(AirPassengers)
AR_fit <- AirPassengers - residuals(AR3)
points(AR_fit, type = "l", col = 2, lty = 2)


plot(forecast(AR3, 10), xlab ="Year",
     ylab ="Air Passengers")



#######################
# mock example for Katey

# create sequence of dates by hourly increments
start <- as.POSIXct("2012-01-01 00:00:00")
end <- now()
dt = seq(from=start, by=60*60, to=end)

# create random temp data
temp = runif(length(dt), 4, 25)

# mock data loggers dataset
loggers = data.frame(dt, temp)


# create random capture times (using intervals of minutes so the 2 datasets don't line up perfectly)
dcaps = seq(from=start, by=60, to=end)

# grab a small sample of capture date/times
captures = data.frame(caps = sample(dcaps, 10), 
                      median.24 = rep(NA, 10),
                      median.48 = rep(NA, 10),
                      median.72 = rep(NA, 10))

# create vectors of number of captures and median column names for looping
n.caps = length(captures$caps)
meds = names(captures)[-1]

# loop it up!!!!
for(i in 1:n.caps){ # loop through number of captures
  
  for(j in 1:length(meds)){ #loop through number of medians
  
    tmp.date = captures[i,"caps"]

    tmp.loggers = loggers %>%
      filter(dt > tmp.date, dt < tmp.date + days(j))

    captures[i,meds[j]] <- median(tmp.loggers$temp)

  }

}

#check to see if it worked
captures

###########################################################
# the key to loops is to always make sure it works for one:
tmp.date = captures[1,"caps"]
tmp.loggers = loggers %>%
  filter(dt > tmp.date, dt < tmp.date + days(1))

captures[1,meds[1]] <- median(tmp.loggers$temp)
