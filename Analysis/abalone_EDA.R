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
require(sjPlot)

##############################################
## Perform EDA
##############################################

## Generate initial model for EDA
fit <- lm(sqrt.years ~ male + female + Length + Diameter + Height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean)

par(mfrow = c(2,2))
attach(aby.clean)
plot(Length,sqrt.years, col = "dodgerblue")
plot(Diameter,sqrt.years, col = "darkorange")
plot(Height,sqrt.years, col = "darkgreen")
plot(subset(Height,Height < 100),subset(sqrt.years,Height < 100), col = "darkgreen")
plot(sqrt.whole,sqrt.years, col = "lightcoral")
plot(sqrt.shucked,sqrt.years, col = "tan4")
plot(sqrt.viscera,sqrt.years, col = "purple")
plot(sqrt.shell,sqrt.years, col = "turquoise4")

## Check for covariance
formattable(cor(cbind(sqrt.years, Sex., Length, Diameter, Height, sqrt.whole, sqrt.shucked, sqrt.viscera, sqrt.shell)))
detach(aby.clean)
pairs(~sqrt.years + Sex. + Length + Diameter + Height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean)

# QQ Plot
par(mfrow = c(1,1))
qqPlot(fit, main = "Q-Q Plot: Saturated Model")

# Studentized residuals distribution
sresid <- studres(fit)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals: Saturated Model", col = "khaki1")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit) # Bonferonni p-value for most extreme obs

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
par(mfrow = c(1,1))
plot(fit, which=4, cook.levels=cutoff)

# Evaluate Collinearity
vif(fit) # variance inflation factors

# Evaluate Nonlinearity: component + residual plot (Partial Residuals)
crPlots(fit, main="Partial-Residuals")

# Model summary
summary(fit)
sjt.lm(fit, string.est = "Estimate", string.dv = "Saturated Model",
       string.p = "p-value", digits.est = 4, digits.ci = 4, show.header = TRUE)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

## Perform stepwise selection for assessment
step <- stepAIC(fit, direction="forward")
step$anova # display results
