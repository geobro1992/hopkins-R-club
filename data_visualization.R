##################
# Merging datasets
##################

surveys = data.frame(survey = as.factor(1:100),
                 site = rep(LETTERS, length.out = 100),
                 temp = sample(50:70, 100, replace = TRUE),
                 precip = sample(0:1, 100, replace = TRUE)
                 )

species = data.frame(SURVEYID = rep(as.factor(1:100),10),
                     taxa = sample(c("spider", "cat", "dog", "snake", "lizard", "hellbender"), 1000, replace = TRUE)
                     ) 

# The base R function is merge() 
?merge
base_merge <- merge(surveys, species, by.x = "survey", by.y = "SURVEYID")


#-------------------------------------
# dplyr has joins for merging datasets

library(dplyr)

# There are many types of joins you can perform

# inner_join(x, y) keeps observations appearing in both tables.
# left_join(x, y) keeps all observations in x and only adds matches from y.
# right_join(x, y) keeps all observations in y and only adds matches from x. Note: it is the same as left_join(y, x)
# full_join(x, y) keeps all observations in x and y; if there's no match, it returns NAs.

# see https://r4ds.had.co.nz/relational-data.html#understanding-joins for more details

# left_join example
gg_merge <- left_join(surveys, species, by = c("survey" = "SURVEYID"))



#####################
# now it's your turn!

# 1. create a second mock data frame

# 2. combine your two data frames using merge

# 3. try out all of the different joins to see what they do 




##############################
# Data visualization in base R
##############################

df = data.frame(months = rep(month.abb, length.out = 100),
                alphabet = rep(LETTERS, length.out = 100),
                binary = sample(c(0,1), 100, replace = TRUE),
                sex = rep(as.factor(c("male", "female")), 50),
                count = sample(0:100, 100, replace = TRUE),
                count2 = sample(0:100, 100, replace = TRUE),
                year = as.factor(sample(2010:2020, 100, replace = TRUE)))

#-------------
# scatter plot

plot(df$count, df$count2)


#----------
# histogram

hist(df$count)


#---------
# bar plot

counts = table(df$binary) # store frequencies of the variable you want to plot

barplot(counts)


abundance = aggregate(df$count, by=list(Group=df$months), FUN=sum) # stores sums of count by month

barplot(abundance$x)


#---------
# box plot

boxplot(count ~ months, data = df) # here we use the formula syntax (y ~ x) to specify we want counts plotted by month


#-----------------------------------
# arranging different plots together

?par

par(mfrow = c(2,2)) # set graphical parameters to be 2 rows and 2 columns 

plot(df$count, df$count2)
hist(df$count)
barplot(counts)
boxplot(count ~ months, data = df)


#-------------
# saving plots

pdf("base_plots.pdf", width = 12, height = 8)

par(mfrow = c(2,2)) # set graphical parameters to be 2 rows and 2 columns 

plot(df$count, df$count2)
hist(df$count)
barplot(counts)
boxplot(count ~ months, data = df)

dev.off()


#################################
# Data visualization using ggplot
#################################
library(ggplot2)
library(dplyr)

#-------------------
# ggplot components

### geom: a geometric object which defines the type of graph you are making. e.g., geom_point(), geom_boxplot(), geom_histogram()

### aes: short for aesthetics. Usually placed within a geom_, this is where you specify your data source and variables, AND the properties of the graph

### stat: a stat layer applies some statistical transformation to the underlying data: for instance, stat_smooth(method = 'lm') displays a linear regression line and confidence interval ribbon on top of a scatter plot

### theme: a theme is made of a set of visual parameters that control the background, borders, grid lines, axes, text size, legend position, etc

#-------------
# scatter plot

gg_scatter = ggplot(df, aes(x = count, y = count2, colour = sex)) +  # linking colour to a factor inside aes() ensures that the points' colour will vary according to the factor levels
  geom_point()


#----------
# histogram

gg_hist = ggplot(df, aes(x = count))  +
  geom_histogram()


#---------
# bar plot

gg_barplot <- ggplot(df, aes(x = binary)) +
  geom_bar()

abundance <- df %>%   
  group_by(months) %>%
  mutate(abundance = (sum(count))) # create new column based on sum of counts by month

gg_barplot2 <- ggplot(abundance, aes(x = months, y = abundance)) +
  geom_bar(stat = "identity")


#--------
# boxplot

gg_boxplot = ggplot(df, aes(months, count)) + 
  geom_boxplot()


#-----------------------------------
# arranging different plots together

library(ggpubr) # need to load an additional package for arranging grids

gg_combine = ggarrange(
  
  gg_hist,      
  gg_barplot, 
  gg_boxplot,   
  gg_scatter,
  
  ncol = 2, nrow = 2)

#-------
# themes

# the ggpubr package also has a clean theme that we can apply to any plot
gg_hist + 
  theme_pubr()


gg_combine2 = ggarrange(
  
  gg_hist + theme_pubr(),      
  gg_barplot + theme_pubr(), 
  gg_boxplot + theme_pubr(),   
  gg_scatter + theme_pubr(),
  
  ncol = 2, nrow = 2)


#----------------
# saving the plot

ggsave(gg_combine2, file = "gg_plots.pdf", width = 12, height = 8)



###############################################################################################
# the 2 things that ggplot excels at is 1) quick and dirty regressions and 2) multi-panel plots

#----------------------------------
# scatter plot with regression line

gg_regression = ggplot(df, aes (x = count, y = count2, colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm", aes(fill = sex))


#------------------
# multi-panel plots

gg_facets <- ggplot(df, aes (x = count, y = count2, colour = months)) +
  geom_point() +                                               
  geom_smooth(method = "lm", aes(fill = months)) +               # Adding linear model fit, colour-code by country
  facet_wrap(~ months, scales = "free_y")                        # THIS LINE CREATES THE FACETTING




#####################
# now it's your turn!

# 1. with your own data, make each of the 4 types of plot in base R

# 2. combine the 4 plots in a single image and save it in the file type of your choice

# 3. do 1) and 2) but using ggplot

# 4. add a regression line to your gg scatter plot (if you want a challenge, try replicating in base R)

# 5. create a facet plot in ggplot  

