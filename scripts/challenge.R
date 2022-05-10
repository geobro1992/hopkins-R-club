############################
# end of semester challenge!
#-----------------------------------------------------------------------
# 1. create a mock dataset for hellbender nest fates (success / failure)

# RULES:
# a. dataset must be at least 500 observations
# b. must include nest fate as a binary (0/1)
# c. must include 2 continuous variables (e.g. forest cover / body size / clutch size) 
# d. must include a discrete variable (e.g. nest box vs natural nest)    
# e. must include a variable that can be used as a random effect (e.g. Individual / Year) 
# f. must use functions (e.g. seq() / rep() / rnorm() ) to make sure your variables will be significant in analysis


#-------------------------------------------------------------------
# 2. Create a sumary table of your dataset that you might include in a pub 

# (e.g. mean body sizes of indivdiuals with successful vs unsuccessful nests)
# (e.g. breakdown of nest fates by site)


#---------------------------------------------------------------------
# 3. create the follwoing raw data plots (I want them to be pretty!):

# a. continuous variable against nest fate using BASE R
# b. continuous variable against nest fate using ggplot
# c. discrete variable against nest fate using BASE R
# d. discrete variable against nest fate using ggplot


#-----------------------------------------------
# 4a. perform a logistic regression of nest fate 
#     (that includes both continuous and discrete predictors, and a random effect)

# 4b. obtain p-values for the fixed effects


#-------------------------------------------------------
# 5a. drop the discrete predictor and rerun the analysis

# 5b. compare the fit of the two models using a test of your choice


#------------------------------------------------------------------------------
# 6a. overlay the predictions of the best supported model ontop of the figures made in step 3 

# 6b. if you want a challenge, try bootsrapping some confidence intervals for the predictions!


#--------------------
#If you finish early:
# 7a. subset your data frame to only include successful nests

# 7b. convert one of your continuous variable into a discrete variable (e.g. clutch size becomes small, medium, and large clutches)
# (HINT: use an ifelse() statement)

# 7c. Perform a t-test/ANOVA on your remaining continuous variable, using the newly created discrete variable as the grouping factor
# NOTE: if your data are not normally distributed, you will ned the non-parametric equivalent!
