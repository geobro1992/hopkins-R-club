####################
# working with dates
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
dat[which(dat$Hgb_1 < 0),"Hgb_1"] = NA

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

# format is the base function to extract elements from a date 
dat <- dat %>% 
  mutate(year = format(txtDate, "%Y"), 
         month = format(txtDate, "%m"),
         day = format(txtDate, "%d"),
         season = format(txtDate-180, "%Y"))

# the lubridate package has built in functions to do the same 
dat <- dat %>% 
  mutate(year2 = year(txtDate), 
         month2 = month(txtDate, label = TRUE, abbr = FALSE),
         day2 = mday(txtDate),
         season2 = year(txtDate-180))

#lubridate also has a function to create a date (or a date/time) from separate elements
dat <- dat %>% 
  mutate(date2 = make_date(year2, month2, day2))


# convert columns to factors for plotting
dat$year = as.factor(as.numeric(dat$year))  
dat$season = as.factor(as.numeric(dat$season))  
dat$month = as.numeric(dat$month)

# plot body size by year
ggboxplot(dat, "season", "SVL.cm.")

# plot nests by month
ggplot(dat, aes(x=month2, color=With_Nest, fill=With_Nest)) +
  geom_histogram(stat = "count", position="identity", alpha=0.5)+
  labs(x="Month", y = "Frequency")+
  scale_x_discrete(labels = month.abb) +
  theme_pubr()
  
# plot dates along axis
ggplot(dat, aes(txtDate)) + 
  geom_freqpoly(binwidth = 30) + # 30 days
  theme_pubr()

# variation on plot above
ggplot(dat, aes(x = River, y = txtDate, color = River)) +
  geom_sina() +
  coord_flip() +
  scale_color_brewer(palette = "Dark2") +
  theme_pubr()

# plot season along axis
ggplot(dat, aes(x = season, color = River, fill = River)) +
  geom_histogram(stat = "count", position="stack") +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  theme_pubr()


###################################
# stats with dates

# when incorporating dates in analyses,
# month/year could be categorical OR numerical predictors
# for time series analyses, date has to be a number

# haemoglobin with month as a numerical predictor
dat = dat[!is.na(dat$Hgb_1),]

m1 = glm(Hgb_1 ~ month, data = dat)
summary(m1)

pred = predict(m1, newdata = data.frame(month = 1:12), se.fit = T)

plot(1:12, pred$fit, type = "l",
     ylim = c(5,9),
     xlab = "month", ylab = "Predicted Hgb")
lines(1:12, pred$fit + 1.96*pred$se.fit, lty = "dashed")
lines(1:12, pred$fit - 1.96*pred$se.fit, lty = "dashed")

# but plotting data shows this is probably not the best thing to do
ggplot(dat, aes(month, Hgb_1)) +
  stat_smooth()

# sine transformation is most common for circular predictors
dat$xmonth = sin(2*pi*dat$month/12)

# redo analysis with transformed month
m2 = glm(Hgb_1 ~ xmonth, data = dat)
summary(m2)

pred = predict(m2, newdata = data.frame(xmonth = sin(2*pi*(1:12)/12)), se.fit = T)

plot(1:12, pred$fit, type = "l",
     ylim = c(5,9),
     xlab = "month", ylab = "Predicted Hgb")
lines(1:12, pred$fit + 1.96*pred$se.fit, lty = "dashed")
lines(1:12, pred$fit - 1.96*pred$se.fit, lty = "dashed")


# add year as a categorical predictor
dat = dat %>%
  filter(year != "2007") %>%
  filter(year != "2021") %>%
  droplevels()

# get rid of zeros so we can use a log link (because our response only takes positive values)
dat$Hgb_1 = dat$Hgb_1 + 0.1

m3 = glm(Hgb_1 ~ xmonth + year, data = dat, family = gaussian(link = log))
summary(m3)

newdat = expand.grid(sin(2*pi*(1:12)/12), c("2013", "2016", "2020"))
names(newdat) = c("xmonth", "year")
pred = predict(m3, newdata = newdat, se.fit = T)

predframe = cbind(month = rep(1:12, 3),
                  newdat, 
                  fit = exp(pred$fit), 
                  upr = exp(pred$fit) + exp(pred$se.fit),
                  lwr = exp(pred$fit) - exp(pred$se.fit))

ggplot(predframe, aes(month, fit, color = year, fill = year))+
    geom_line()+
    geom_ribbon(data=predframe,aes(ymin=lwr,ymax=upr),alpha=0.3) +
  xlab("month") + ylab("Predicted Haemoglobin") +
  ylim(0,10) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_pubr()


#####################
# now it's your turn!

# 1. with your own data, make sure you can convert date columns to date

# 2. extract year and month from date columns for plotting/analyses

# 3. perform a simple regression with some aspect of date as a predictor

# 4. transform month using sine or cosine, so that it's cyclical nature can be incorporated into models
