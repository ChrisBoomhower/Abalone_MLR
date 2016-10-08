#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## abalone_ModelRefinement.R
##############################

ModelEDA <- function(modelfit, df, pairsIn){
    ## Check for covariance
    pairs(pairsIn, data = df)
    
    # QQ Plot
    par(mfrow = c(1,1))
    qqPlot(modelfit, main = "QQ Plot for Model Reduction")
    
    # Studentized residuals distribution
    sresid <- studres(modelfit)
    hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
    xfit<-seq(min(sresid),max(sresid),length=40)
    yfit<-dnorm(xfit)
    lines(xfit, yfit)
    
    # Outlier assessment
    writeLines("\n-----Bonferroni p-values for most extreme outliers-----\n")
    print(outlierTest(modelfit)) # Bonferonni p-value for most extreme obs
    
    # Cook's D plot: identify D values > 4/(n-k-1) 
    cutoff <- 4/((nrow(mtcars)-length(modelfit$coefficients)-2))
    par(mfrow = c(1,1))
    plot(modelfit, which=4, cook.levels=cutoff)
    
    # Evaluate Collinearity
    writeLines("\n-----VIF Values-----\n")
    print(vif(modelfit)) # variance inflation factors
    
    # Evaluate Nonlinearity: component + residual plot (Partial Residuals)
    crPlots(modelfit, main="Partial-Residuals")
    
    # Model summary
    writeLines("\n-----Model Summary-----\n")
    print(summary(modelfit))
    
    ## Generate diagnostic plots
    layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
    plot(modelfit)
}

##############################################################################
## Model Reduction Attempt #1 (observation 2052 [obs.2051 after removing
## 0 Height values] and Length variable removed)
##############################################################################
aby.clean2 <- subset(aby.clean, rownames(aby.clean) != 2051) # Remove Outlier 2051 since mis-measurement presumed

fit2 <- lm(sqrt.years ~ male + female + Diameter + Height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean2)
writeLines("\n-----Covariance Matrix-----\n")
cor(cbind(sqrt.years, Sex., Diameter, Height, sqrt.whole, sqrt.shucked, sqrt.viscera, sqrt.shell))
detach(aby.clean2)
pairsIn <- eval(~sqrt.years + Sex. + Diameter + Height + sqrt.whole + sqrt.shucked + sqrt.viscera + sqrt.shell)

ModelEDA(fit2,aby.clean2,pairsIn)

##############################################################################
## Model Reduction Attempt #2 (sqrt.whole removal)
##############################################################################
fit3 <- lm(sqrt.years ~ male + female + Diameter + Height  + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean2)
cor(cbind(sqrt.years, Sex., Diameter, Height, sqrt.shucked, sqrt.viscera, sqrt.shell))
detach(aby.clean2)
pairsIn <- eval(~sqrt.years + Sex. + Diameter + Height + sqrt.shucked + sqrt.viscera + sqrt.shell)

ModelEDA(fit3,aby.clean2,pairsIn)

##############################################################################
## Model Reduction Attempt #3 (Diameter removal)
##############################################################################
fit4 <- lm(sqrt.years ~ male + female + Height + sqrt.shucked + sqrt.viscera + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean2)
cor(cbind(sqrt.years, Sex., Height, sqrt.shucked, sqrt.viscera, sqrt.shell))
detach(aby.clean2)
pairsIn <- eval(~sqrt.years + Sex. + Height + sqrt.shucked + sqrt.viscera + sqrt.shell)

ModelEDA(fit4,aby.clean2,pairsIn)

##############################################################################
## Model Reduction Attempt #4 (sqrt.viscera removal) *** Selected model ***
##############################################################################
fit5 <- lm(sqrt.years ~ male + female  + Height  + sqrt.shucked + sqrt.shell, data = aby.clean2)

## Check for covariance
attach(aby.clean2)
cor(cbind(sqrt.years, Sex., Height, sqrt.shucked, sqrt.shell))
detach(aby.clean2)
pairsIn <- eval(~sqrt.years + Sex. + Height + sqrt.shucked + sqrt.shell)

ModelEDA(fit5,aby.clean2,pairsIn)
sjt.lm(fit5, string.est = "Estimate", string.dv = "Reduced Model",
       string.p = "p-value", digits.est = 4, digits.ci = 4, show.header = TRUE)

## SOME QUICK AD HOC REVIEW OF OUTLIER INFLUENCE ON FINAL MODEL
aby.clean3 <- subset(aby.clean2, rownames(aby.clean2) != 1417)
fitT <- lm(sqrt.years ~ male + female  + Height  + sqrt.shucked + sqrt.shell, data = aby.clean)
vif(fitT) # variance inflation factors
summary(fitT)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitT)

##############################################################################
## Model Reduction Attempt #5 (sqrt.shell removal)
##############################################################################
fit6 <- lm(sqrt.years ~ male + female  + Height  + sqrt.shucked, data = aby.clean2)

## Check for covariance
attach(aby.clean2)
cor(cbind(sqrt.years, Sex., Height, sqrt.shucked))
detach(aby.clean2)
pairsIn <- eval(~sqrt.years + Sex. + Height + sqrt.shucked)

ModelEDA(fit6,aby.clean2,pairsIn)
