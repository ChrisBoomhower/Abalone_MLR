#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## abalone_EDA.R
##############################

require(dplyr)

##############################################
## Import raw data from disk
##############################################
aby <- read.table("abalone.data", sep = ",")

##############################################
## Clean-up / Preliminary EDA to see what
## transformations may be required
##############################################
aby.clean <- rename(aby, Sex. = V1)
aby.clean <- rename(aby.clean, Length = V2)
aby.clean <- rename(aby.clean, Diameter = V3)
aby.clean <- rename(aby.clean, Height = V4)
aby.clean <- rename(aby.clean, Whole = V5)
aby.clean <- rename(aby.clean, Shucked = V6)
aby.clean <- rename(aby.clean, Viscera = V7)
aby.clean <- rename(aby.clean, Shell = V8)
aby.clean <- rename(aby.clean, Rings = V9)

str(aby.clean)
head(aby.clean)

par(mfrow = c(1, 1))
barplot(table(aby.clean$Sex.))

par(mfrow = c(2, 2))
hist(aby.clean$Length)
hist(aby.clean$Diameter)
hist(aby.clean$Height)
hist(log(aby.clean$Height))

par(mfrow = c(2, 2))
hist(aby.clean$Whole)
hist(sqrt(aby.clean$Whole))
hist(aby.clean$Shucked)
hist(sqrt(aby.clean$Shucked))
hist(aby.clean$Viscera)
hist(sqrt(aby.clean$Viscera))
hist(aby.clean$Shell)
hist(sqrt(aby.clean$Shell))
hist(aby.clean$Rings)
hist(sqrt(aby.clean$Rings))

##############################################
## Transform the data
##############################################
aby.clean$log.height <- log(aby.clean$Height + 1)
aby.clean$sqrt.whole <- sqrt(aby.clean$Whole)
aby.clean$sqrt.shucked <- sqrt(aby.clean$Shucked)
aby.clean$sqrt.viscera <- sqrt(aby.clean$Viscera)
aby.clean$sqrt.shell <- sqrt(aby.clean$Shell)
aby.clean$sqrt.rings <- sqrt(aby.clean$Rings)
