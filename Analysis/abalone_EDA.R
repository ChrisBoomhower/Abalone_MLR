#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## abalone_EDA.R
##############################

require(MASS)
require(dplyr)
require(car)

##############################################
## Randomly sample NYSclean
##############################################
# set.seed(20) #Seed set for reproducibility
# NYSample <- NYSclean[sample(nrow(NYSclean), 10000),]

##############################################
## Perform EDA
##############################################

## Generate initial model for EDA
fit <- lm(sqrt.rings ~ Sex. + Length + Diameter + log.height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean)
par(mfrow = c(1,1))

## Check for covariance
cor(cbind(aby.clean$sqrt.rings, aby.clean$Sex., aby.clean$Length, aby.clean$Diameter, aby.clean$log.height, aby.clean$sqrt.whole, aby.clean$sqrt.shucked, aby.clean$sqrt.viscera, aby.clean$sqrt.shell))
scatterplotMatrix(~sqrt.rings + Sex. + Length + Diameter + log.height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean)

# QQ Plot
qqPlot(fit, main = "QQ Plot")

# Studentized residuals distribution
sresid <- studres(fit)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit) # Bonferonni p-value for most extreme obs
leveragePlots(fit) # leverage plots

# Check for variable influence on response
avPlots(fit)

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
par(mfrow = c(1,1))
plot(fit, which=4, cook.levels=cutoff)

# Influence Plot
# influencePlot(fit,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Evaluate homoscedasticity: non-constant error variance test
ncvTest(fit)

# Plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)

# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?

# Evaluate Nonlinearity: component + residual plot 
crPlots(fit)

# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Model summary
summary(fit)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

## Perform stepwise selection for assessment
step <- stepAIC(fit, direction="both")
step$anova # display results

