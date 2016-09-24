#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## abalone_ModelRefinement.R
##############################

##############################################################################
## Model Reduction Attempt #1 (observation 2052 and Length variable removed)
##############################################################################
aby.clean2 <- subset(aby.clean, rownames(aby.clean) != 2052) # Remove Outlier 2052 since mismeasurement presumed  # & rownames(aby.clean) != 1418)

fit2 <- lm(sqrt.rings ~ male + female + Diameter + Height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean)
cor(cbind(sqrt.rings, Sex., Diameter, Height, sqrt.whole, sqrt.shucked, sqrt.viscera, sqrt.shell))
detach(aby.clean)
pairs(~sqrt.rings + Sex. + Diameter + Height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean)

# QQ Plot
par(mfrow = c(1,1))
qqPlot(fit2, main = "QQ Plot fit2")

# Studentized residuals distribution
sresid <- studres(fit2)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit2) # Bonferonni p-value for most extreme obs

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit2$coefficients)-2))
par(mfrow = c(1,1))
plot(fit2, which=4, cook.levels=cutoff)

# Evaluate Collinearity
vif(fit2) # variance inflation factors

# Evaluate Nonlinearity: component + residual plot (Partial Residuals)
crPlots(fit2, main="Partial-Residuals")

# Model summary
summary(fit2)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit2)

##############################################################################
## Model Reduction Attempt #2 (sqrt.whole removal)
##############################################################################
fit3 <- lm(sqrt.rings ~ male + female + Diameter + Height  + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean)
cor(cbind(sqrt.rings, Sex., Diameter, Height, sqrt.shucked, sqrt.viscera, sqrt.shell))
detach(aby.clean)
pairs(~sqrt.rings + Sex. + Diameter + Height + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean)

# QQ Plot
par(mfrow = c(1,1))
qqPlot(fit3, main = "QQ Plot fit3")

# Studentized residuals distribution
sresid <- studres(fit3)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit3) # Bonferonni p-value for most extreme obs

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit3$coefficients)-2))
par(mfrow = c(1,1))
plot(fit3, which=4, cook.levels=cutoff)

# Evaluate Collinearity
vif(fit3) # variance inflation factors

# Evaluate Nonlinearity: component + residual plot (Partial Residuals)
crPlots(fit3, main="Partial-Residuals")

# Model summary
summary(fit3)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit3)

##############################################################################
## Model Reduction Attempt #3 (sqrt.viscera removal)
##############################################################################
fit4 <- lm(sqrt.rings ~ male + female + Diameter + Height  + sqrt.shucked + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean)
cor(cbind(sqrt.rings, Sex., Diameter, Height, sqrt.shucked, sqrt.shell))
detach(aby.clean)
pairs(~sqrt.rings + Sex. + Diameter + Height + sqrt.shucked + sqrt.shell, data = aby.clean)

# QQ Plot
par(mfrow = c(1,1))
qqPlot(fit4, main = "QQ Plot fit4")

# Studentized residuals distribution
sresid <- studres(fit4)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit4) # Bonferonni p-value for most extreme obs

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit4$coefficients)-2))
par(mfrow = c(1,1))
plot(fit4, which=4, cook.levels=cutoff)

# Evaluate Collinearity
vif(fit4) # variance inflation factors

# Evaluate Nonlinearity: component + residual plot (Partial Residuals)
crPlots(fit4, main="Partial-Residuals")

# Model summary
summary(fit4)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit4)

##############################################################################
## Model Reduction Attempt #4 (Diameter removal) *** I LIKE THIS ONE THE MOST!!!
##############################################################################
fit5 <- lm(sqrt.rings ~ male + female  + Height  + sqrt.shucked + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean)
cor(cbind(sqrt.rings, Sex., Height, sqrt.shucked, sqrt.shell))
detach(aby.clean)
pairs(~sqrt.rings + Sex. + Height + sqrt.shucked + sqrt.shell, data = aby.clean)

# QQ Plot
par(mfrow = c(1,1))
qqPlot(fit5, main = "QQ Plot fit5")

# Studentized residuals distribution
sresid <- studres(fit5)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit5) # Bonferonni p-value for most extreme obs

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit5$coefficients)-2))
par(mfrow = c(1,1))
plot(fit5, which=4, cook.levels=cutoff)

# Evaluate Collinearity
vif(fit5) # variance inflation factors

# Evaluate Nonlinearity: component + residual plot (Partial Residuals)
crPlots(fit5, main="Partial-Residuals")

# Model summary
summary(fit5)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit5)

##############################################################################
## Model Reduction Attempt #5 (sqrt.shell removal)
##############################################################################
fit6 <- lm(sqrt.rings ~ male + female  + Height  + sqrt.shucked, data = aby.clean2)

## Check for covariance
attach(aby.clean)
cor(cbind(sqrt.rings, Sex., Height, sqrt.shucked))
detach(aby.clean)
pairs(~sqrt.rings + Sex. + Height + sqrt.shucked, data = aby.clean)

# QQ Plot
par(mfrow = c(1,1))
qqPlot(fit6, main = "QQ Plot fit6")

# Studentized residuals distribution
sresid <- studres(fit6)
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Outlier assessment
outlierTest(fit6) # Bonferonni p-value for most extreme obs

# Cook's D plot: identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit6$coefficients)-2))
par(mfrow = c(1,1))
plot(fit6, which=4, cook.levels=cutoff)

# Evaluate Collinearity
vif(fit6) # variance inflation factors

# Evaluate Nonlinearity: component + residual plot (Partial Residuals)
crPlots(fit6, main="Partial-Residuals")

# Model summary
summary(fit6)

## Generate diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit6)