####################
# working with Becca's stream data
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggforce)
library(lubridate)
library(readxl)
library(forecast)

dat_DO <- read_excel("R/r_club/431_DO.xlsx")

# in this example we just have 1 site, 1 logger/individual
unique(dat_DO$Box)
unique(dat_DO$Site)
unique(dat_DO$PIT)

# extract just time from EST.time
dat_DO = dat_DO %>%
  mutate(Time = format(dat_DO$EST.time, format = "%H:%M")) 

# create date-time column
dat_DO = dat_DO %>%
  mutate(datetime = as.POSIXlt(paste(dat_DO$EST.date, dat_DO$Time)))

#filter just hourly data  
dat_DO_hourly = dat_DO %>%
  filter(minute(dat_DO$datetime) == "0")

#################################
# plot a time series of DO levels

DOts <- ts(dat_DO_hourly$`Dissolved Oxygen`, start = min(dat_DO_hourly$datetime),
          frequency = 24)

plot(DOts,
        xaxt = "n",
        xlab="Date", ylab="DO", 
        main=paste("DO levels at ", dat_DO$Site[1], dat_DO$Box[1]))

tsp = attributes(DOts)$tsp
dates = seq(from = min(dat_DO$EST.date), to = max(dat_DO$EST.date), by = "day")
axis(1, at = seq(tsp[1], tsp[2], along = dates), labels = format(dates, "%m-%d"))


# The decompose() and forecast::stl() splits the time series into 
# seasonality, trend and error components.
decomposedRes <- decompose(DOts, type="additive") 
plot (decomposedRes) # see plot below


#Fitting the AR Model to the time series
AR1 <- arima(DOts, order = c(1,0,0))

plot(forecast(AR1, 72), xlab ="Date",
     ylab ="DO")

AR2 <- arima(DOts, order = c(1,0,1))

AIC(AR1) 
AIC(AR2)

plot(forecast(AR2, 72), xlab ="Year",
     ylab ="DO")

#Autocorrelation is the correlation of a Time Series with lags of itself. This is a significant metric because,
#It shows if the previous states (lagged observations) of the time series has an influence on the current state. In the autocorrelation chart, if the autocorrelation crosses the dashed blue line, it means that specific lag is significantly correlated with current series. For example, in autocorrelation chart of AirPassengers - the top-left chart (below), there is significant autocorrelation for all the lags shown on x-axis.
#It is used commonly to determine if the time series is stationary or not. A stationary time series will have the autocorrelation fall to zero fairly quickly but for a non-stationary series it drops gradually.
#Partial Autocorrelation is the correlation of the time series with a lag of itself, with the linear dependence of all the lags between them removed.

# both acf() and pacf() can generates plots, but ggplot has a nice function 
acf(DOts, lag.max = 144)
pacf(DOts, lag.max = 144)

DOts %>% diff(lag = 24) %>% ggtsdisplay()

DOts %>% diff(lag = 24) %>% diff() %>% ggtsdisplay()


# add seasonal components
AR3 <- arima(DOts, order = c(1,0,1), seasonal = c(2,1,0))

#compare models
AIC(AR1) 
AIC(AR2)
AIC(AR3) 

plot(forecast(AR3, 72), xlab ="Year",
     ylab ="DO")

checkresiduals(AR3)



######################
# behavior data
dat_behav <- read_excel("R/r_club/behavior_data.xlsx")

# check it's from the same place as the logger
unique(dat_behav$Site)
unique(dat_behav$Box)


# extract just time from EST.time
dat_behav = dat_behav %>%
  mutate(Time.s = format(dat_behav$EST.start, format = "%H:%M"),
         Time.e = format(dat_behav$EST.end, format = "%H:%M")) 

# create date-time columns for start and end times
dat_behav = dat_behav %>%
  mutate(datetime.s = as.POSIXlt(paste(dat_behav$Full.date, dat_behav$Time.s)),
         datetime.e = as.POSIXlt(paste(dat_behav$Full.date, dat_behav$Time.e)))


#filter just fanning behavior  
dat_fanning = dat_behav %>%
  filter(dat_behav$Behavior == "Fanning")%>%
  mutate(day = factor(yday(datetime.s)), week = week(datetime.s)) 

#filter just rocking behavior  
dat_rocking = dat_behav %>%
  filter(dat_behav$Behavior == "Rocking")%>%
  mutate(day = factor(yday(datetime.s)), week = week(datetime.s)) 

#overlay some behaviors on time series
plot(dat_DO_hourly$datetime, dat_DO_hourly$`Dissolved Oxygen`, type = "l")
points(dat_fanning$datetime.s, rep(7,length(dat_fanning$datetime.s)), lwd = 2)
points(dat_rocking$datetime.s, rep(7.5,length(dat_rocking$datetime.s)), lwd = 2, col = "dark grey")

# similar in ggplot
ggplot(dat_DO_hourly, aes(as.POSIXct(datetime), y = `Dissolved Oxygen`)) +
  geom_line() +
  geom_point(data = dat_fanning, aes(as.POSIXct(datetime.s), y = rep(7, length(datetime.s)), size = Dur.sec)) +
  geom_point(data = dat_rocking, aes(as.POSIXct(datetime.s), y = rep(7.5, length(datetime.s)), size = Dur.sec))


# subset DO data to observation days
dat_DB = dat_DO_hourly %>%
  filter(datetime > min(dat_rocking$datetime.s), datetime < max(dat_rocking$datetime.s)) %>%
  mutate(day = factor(yday(datetime)), week = factor(week(datetime)))

# plot time series for each day
ggplot(dat_DB, aes(as.POSIXct(datetime), y = `Dissolved Oxygen`, color = day)) +
  geom_line() +
  geom_point(data = dat_fanning, aes(as.POSIXct(datetime.s), y = rep(7, length(datetime.s)), size = Dur.sec)) +
  geom_point(data = dat_rocking, aes(as.POSIXct(datetime.s), y = rep(7.5, length(datetime.s)), size = Dur.sec))+
 facet_wrap(~day, scales = "free_x")

# plot by week
ggplot(dat_DB, aes(as.POSIXct(datetime), y = `Dissolved Oxygen`)) +
  geom_line() +
  geom_point(data = dat_fanning, aes(as.POSIXct(datetime.s), y = rep(7, length(datetime.s)), size = Dur.sec)) +
  geom_point(data = dat_rocking, aes(as.POSIXct(datetime.s), y = rep(7.5, length(datetime.s)), size = Dur.sec))+
  facet_wrap(~week, scales = "free_x")

