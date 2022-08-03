################################
# intro to multivariate analysis
################################

#########################
# tutorials I stole from:
# https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/
# https://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html


# load libraries
library(car)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(corrplot)


# read in data
ami_data <- read.table("http://static.lib.virginia.edu/statlab/materials/data/ami_data.DAT")
names(ami_data) <- c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")


# 17 overdoses of the drug amitriptyline 
# There are two responses we want to model: TCAD plasma level (TOT) and the amount of amitriptyline present (AMI)

#The predictors are as follows:
# GEN, gender (male = 0, female = 1)
# AMT, amount of drug taken at time of overdose
# PR, PR wave measurement
# DIAP, diastolic blood pressure
# QRS, QRS wave measurement

# explore your data!
summary(ami_data)
pairs(ami_data)

##########
# analysis

# can use regular regression functions combined with cbind() for multiple responses
mlm1 <- lm(cbind(TOT, AMI) ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(mlm1)

# output tables are exactly the same as if we did them separately
# check for yoursleves
m1 <- lm(TOT ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(m1)
m2 <- lm(AMI ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(m2)


# the same diagnostic plots apply and you will have to make them by hand

# https://data.library.virginia.edu/diagnostic-plots/

# you wil have 2 sets of diagnostic plots
head(resid(mlm1)) # residuals
sigma(mlm1) # residual standard error
head(fitted(mlm1)) # fitted values
coef(mlm1) # coefficients


# the big difference is in the variance covariance matrix!
vcov(mlm1)
# the coefficients from both models covary 
# That covariance needs to be taken into account when determining if a predictor is jointly contributing to both models

# For example, the effects of PR and DIAP seem borderline. 
# They appear significant for TOT but less so for AMI. 
# But it’s not enough to eyeball the results from the two separate regressions! 
# We need to formally test for their inclusion. And that test involves the covariances between the coefficients in both models.

# Determining whether or not to include predictors in a multivariate multiple regression requires the use of multivariate test statistics. 
# These are often taught in the context of MANOVA, or multivariate analysis of variance.

# there is a dedicated Manova() function in the car package, but Anova() also knows how to handle multivariate model fits
Manova(mlm1)
Anova(mlm1)


# compare full model with simpler model
mlm2 <- update(mlm1, . ~ . - PR - DIAP - QRS)
anova(mlm1, mlm2)

#The large p-value provides good evidence that the model with two predictors fits as well as the model with five predictors. 
# Notice the test statistic is “Pillai”, which is one of the four common multivariate test statistics.


# linearHypothesis() will perform the same comparison and provide all 4 test statistics
lh.out <- linearHypothesis(mlm1, hypothesis.matrix = c("PR = 0", "DIAP = 0", "QRS = 0"))
lh.out
# (the Roy test is most sensitive to small sample sizes)


#############
# predictions

nd <- data.frame(GEN = 1, AMT = 1200)
p <- predict(object = mlm2, newdata = nd)
p


# We usually quantify uncertainty with confidence intervals 
# But in this case we have two predictions from a multivariate model with two sets of coefficients that covary! 
# This means we don’t calculate an interval but rather an ellipse to capture the uncertainty in two dimensions.
# R does not have built in functions ot do this


predictionEllipse <- function(mod, newdata, level = 0.95, ggplot = TRUE){
  # labels
  lev_lbl <- paste0(level * 100, "%")
  resps <- colnames(mod$coefficients)
  title <- paste(lev_lbl, "confidence ellipse for", resps[1], "and", resps[2])
  
  # prediction
  p <- predict(mod, newdata)
  
  # center of ellipse
  cent <- c(p[1,1],p[1,2])
  
  # shape of ellipse
  Z <- model.matrix(mod)
  Y <- mod$model[[1]]
  n <- nrow(Y)
  m <- ncol(Y)
  r <- ncol(Z) - 1
  S <- crossprod(resid(mod))/(n-r-1)
  
  # radius of circle generating the ellipse
  tt <- terms(mod)
  Terms <- delete.response(tt)
  mf <- model.frame(Terms, newdata, na.action = na.pass, 
                    xlev = mod$xlevels)
  z0 <- model.matrix(Terms, mf, contrasts.arg = mod$contrasts)
  rad <- sqrt((m*(n-r-1)/(n-r-m))*qf(level,m,n-r-m)*z0%*%solve(t(Z)%*%Z) %*% t(z0))
  
  # generate ellipse using ellipse function in car package
  ell_points <- car::ellipse(center = c(cent), shape = S, radius = c(rad), draw = FALSE)
  
  # ggplot2 plot
  if(ggplot){
    require(ggplot2, quietly = TRUE)
    ell_points_df <- as.data.frame(ell_points)
    ggplot(ell_points_df, aes(x, y)) +
      geom_path() +
      geom_point(aes(x = TOT, y = AMI), data = data.frame(p)) +
      labs(x = resps[1], y = resps[2], 
           title = title) +
      theme_classic()
  } else {
    # base R plot
    plot(ell_points, type = "l", xlab = resps[1], ylab = resps[2], main = title)
    points(x = cent[1], y = cent[2])
  }
}


# use function to plot prediction and uncertainty
predictionEllipse(mod = mlm2, newdata = nd)
predictionEllipse(mod = mlm2, newdata = nd, ggplot = FALSE)



######
# PCA
#####

# see relationships between predictors
scatterplotMatrix(ami_data[4:7])

# line graph for predictors
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval),(lastyval),namei,col="black",cex=0.6)
  }
}


# plot data
names <- names(ami_data)[c(4:7)]
mylist <- list(ami_data$AMT,ami_data$PR,ami_data$DIAP, ami_data$QRS)
makeProfilePlot(mylist,names)


# standardize predictors
standardisedpredictors <- as.data.frame(scale(ami_data[4:7]))

# plot standardized predictors
names <- names(ami_data)[c(4:7)]
mylist <- list(standardisedpredictors$AMT,standardisedpredictors$PR,standardisedpredictors$DIAP, standardisedpredictors$QRS)
makeProfilePlot(mylist,names)


################################################
# see which predictors are most highly correlated

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}

mosthighlycorrelated(ami_data[4:7], 6)


# perform PCA on standardized predictors
standardisedpredictors <- as.data.frame(scale(ami_data[4:7]))
od.pca <- prcomp(standardisedpredictors) # do a PCA

summary(od.pca)
screeplot(od.pca, type="lines")

# Kaiser’s criterion: we should only retain principal components for which the variance is above 1 
# (when applied to standardized data)
(od.pca$sdev)^2

# see loadings for each predictor
od.pca$rotation[,1] # first principal component
od.pca$rotation[,2] # second principal component


ggplot(as.data.frame(od.pca$x), aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = ami_data$TOT, size = 2)) +
  scale_colour_gradient2() +
  theme_classic()


# add loadings to the PC plot
fviz_pca_var(od.pca, col.var = "black")

# extract contributions of each predictor
var <- get_pca_var(od.pca)
var
corrplot(var$contrib[,1:2], is.corr=FALSE, col.lim = c(0,100), col = colorRampPalette(c("white", "green", "darkgreen"))(100))

# Contributions of variables to PC1
fviz_contrib(od.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(od.pca, choice = "var", axes = 2, top = 10)


fviz_pca_var(od.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


