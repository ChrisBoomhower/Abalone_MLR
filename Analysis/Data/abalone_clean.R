#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 1: Multiple Linear Regression Analysis
## 10/03/2016
##
## abalone_EDA.R
##############################

require(dplyr)
require(formattable)

##############################################
## Import raw data from disk
##############################################
aby <- read.table("abalone.data", sep = ",")

##############################################
## Clean-up / Preliminary EDA to see what
## transformations may be required
##############################################
aby.clean <- aby
names(aby.clean) <- c("Sex.", "Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "Rings")
aby.clean[,2:8] <- aby.clean[,2:8] * 200 # Muliply continuous variables by 200 to rescale to actual values (See UCI study data description)

# Create dummy variables to match SAS methodology
aby.clean$male <- ifelse(aby.clean$Sex. == "M", 1, 0)
aby.clean$female <- ifelse(aby.clean$Sex. == "F", 1, 0)

str(aby.clean)
head(aby.clean)

# Review distributions
par(mfrow = c(1, 1))
barplot(table(aby.clean$Sex.)) # Abalone sex barplots
table(aby.clean$Sex.) # Combine values with barplot using ggplot

par(mfrow = c(2, 2))
hist(aby.clean$Length) # Height Histograms
hist(aby.clean$Diameter)
hist(aby.clean$Height)
boxplot(aby.clean$Height, main = "Boxplot of Height")
#hist(log(aby.clean$Height+1))

# View potential outliers in Height data
outliers = boxplot(aby.clean$Height, plot=FALSE)$out
aby.clean[aby.clean$Height %in% outliers,]

# Remove 0 values for Height
sum(aby.clean$Height == 0)
aby.clean <- aby.clean %>% filter(Height > 0)
sum(aby.clean$Height == 0)

# Observe what the Height distribution would look like without the two large outliers
par(mfrow = c(1, 1))
hist(aby.clean[aby.clean$Height < 100,]$Height)

# Continue reviewing distributions
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

# Provide descriptive summary statistics
min <- round(sapply(aby.clean[,2:8], min, na.rm = TRUE),2)
max <- round(sapply(aby.clean[,2:8], max, na.rm = TRUE),2)
mean <- round(sapply(aby.clean[,2:8], mean, na.rm = TRUE),2)
median <- round(sapply(aby.clean[,2:8], median, na.rm = TRUE),2)
std.dev <- round(sapply(aby.clean[,2:8], sd, na.rm = TRUE),2)
variance <- round(sapply(aby.clean[,2:8], var, na.rm = TRUE),2)

var.summary <- rbind(min, max, mean, median, std.dev, variance) # Combine rows for summary table
formattable(var.summary)


##############################################
## Transform the data
##############################################
aby.clean$sqrt.whole <- sqrt(aby.clean$Whole)
aby.clean$sqrt.shucked <- sqrt(aby.clean$Shucked)
aby.clean$sqrt.viscera <- sqrt(aby.clean$Viscera)
aby.clean$sqrt.shell <- sqrt(aby.clean$Shell)
aby.clean$sqrt.rings <- sqrt(aby.clean$Rings)
