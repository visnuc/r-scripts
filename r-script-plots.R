# ===================
#   DIFFERENT PLOTS 
# ===================

# boxplot
boxplot(myData$income)

# histogram
hist(myData$income)
hist(myData$momwt)

# density plot 
plot(density(myData$income))

# scatter plot 
plot(myData$momed, myData$income, main = "Practice scatter plot", xlab = "Mother's education", ylab = "Income", pch = 18) + 
  abline(lm(myData$income ~ myData$momed), col = "red") + 
  lines(lowess(myData$momed, myData$income), col="blue")

