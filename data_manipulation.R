###################
# data manipulation
###################

df = data.frame(months = rep(month.abb, length.out = 100),
           alphabet = rep(LETTERS, length.out = 100),
           binary = sample(c(0,1), 100, replace = TRUE),
           ID = as.factor(1:100),
           count = sample(0:100, 100, replace = TRUE),
           year = as.factor(sample(2010:2020, 100, replace = TRUE)))

#############################
# logical operators in base R

# Subsetting with one condition
df[df$count < 50, ]    # count less than 50
df[!df$count >= 50, ]   # This is completely equivalent to the last statement


# Subsetting with two conditions
df[df$months == "Jun" | df$binary == 0, ]    # returns only June data OR binary = 0
df[df$months == "Jun" & df$binary == 0, ]    # returns only June data AND binary = 0

spring = c("Mar", "Apr", "May")
df[df$months %in% spring & df$binary == 0, ]    # returns months in 'spring' vector AND binary = 0


#########################
# tidyR data manipulation

library(tidyr)

#--------------------------------------------------------------
# gather() and spread() are equivalent to pivot tables in excel

# spread() goes from long to wide format
df_wide <- spread(df, key = year, value = count) # create individual columns for each year with count as the variable

# gather goes from wide to long format
df_long <- gather(df_wide, key = year, value = count, as.factor(2010:2020)) # we need to specify which columns to gather
df_long2 <- gather(df_wide, key = year, value = count, as.factor(2010:2020), na.rm = TRUE) # na.rm removes any missing values

# if you have a big data frame or weird names, it might be easier to specify the column numbers to gather
df_long3 <- gather(df_wide, year, count, c(5:15), na.rm = TRUE)



#------------------------------------------------
# rename() is the tidy function to rename columns
df_long2 <- rename(df_long2, season = year, abundance = count)

# the base R equivalent would have been
names(df_long3)[c(5,6)] <- c("season", "abundance")


#-----------------------------------------------------------------------
# filter() and select() allow you to subset data and columns respectively

# FILTER RECORDS
# Let's keep observations from spring of 2020
df_subset <- filter(df_long2, months %in% spring, season == "2020") # you can use multiple different conditions separated by commas
df_subset2 <- filter(df_long2, months %in% spring | season == "2020") # OR symbol from base R still works

# SELECT COLUMNS
# Let's ditch the alphabet column just as an example
df_no.letters <- dplyr::select(df_long2, months, binary, ID, season, abundance)   
# or alternatively
df_no.letters2 <- dplyr::select(df_long2, -alphabet) # the minus sign removes the column

# For comparison, the base R equivalent would be (not assigned to an object here):
# df_long2[ , -1]  # removes first column

# A nice hack! select() lets you rename and reorder columns on the fly
df_no.letters3 <- dplyr::select(df_long3, YEAR = season, INDIVIDUAL = ID, MON = months)


#------------------------------------------
# mutate() allows you to create new columns

df_add <- mutate(df, count2 = count^2)


#----------------------------------------------------------
# group_by() groups by a factor for subsequent manipulation

df_grouped <- group_by(df_long2, season)   # grouping our dataset by year


#---------------------------------------------------
# summarise() provides a specified summary statistic

#compare summary of grouped vs ungrouped datasets
summary1 <- summarise(df_long2, total.abundance = sum(abundance))
summary2 <- summarise(df_grouped, total.abundance = sum(abundance))

# allows for multiple summary statistics
summary3 <- summarise(df_grouped, total.abundance = sum(abundance),
                      sd.abundance = sd(abundance),
                      detection.prob = mean(binary))


#----------------------------------------------------------------------------------------
# case_when() is a generalisation of ifelse() that lets you assign more than two outcomes 
# All logical operators are available, and you assign the new value with a tilde ~

mutate(summary3, case_when(detection.prob <= 0.3 ~ "Low Detection",
                           between(detection.prob, 0.3, 0.7) ~ "Medium Detection",
                           detection.prob >= 0.7 ~ "High Detection"))


#-------------------------------------------------
# pipes - %>% - allow you to chain dplyr functions

df.subset <- df %>%
  filter(months %in% spring) %>%
  group_by(months, year) %>%
  summarise(mean.count = mean(count)) %>%
  glimpse()



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

# 1. subset your own data frame, using all of the logical operators (see syntax.md on the github site to remind yourself of what they mean)

# 2. try out all of the dplyr functions individually on your mock data

# 3. try combining dplyr functions (e.g. mutate() and case_by() to create a new factor)

# 4. chain some of the dplyr functions with pipes

# 5. create a second mock data frame and combine your two data frames using merge/join

# 6. try out all of the different joins to see what they do 
