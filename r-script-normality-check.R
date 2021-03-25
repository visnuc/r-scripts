# setting working directory
setwd("/my/desired/directory/for/r") 

# import csv data set  
myData <-read.csv(file.choose(), header=T) 

# =====================
# Testing normality
# =====================

# with histogram 
hist(myDataDsdc$income)
library(ggplot2)
# library(ggpubr)
# install.packages("ggpubr")

# with Q-Q plot                               
qqnorm(myDataDsdc$income, pch = 1, frame = FALSE)
qqline(myDataDsdc$income, col = "steelblue", lwd = 2)

# with stem and leaf plot 
stem(myDataDsdc$income)

# with boxplot
boxplot(myDataDsdc$income)

# with p-p plot                               
# pp.plot(myDataDsdc$income)    # yet to work 

# with skewness
library(e1071)
skewness(myDataDsdc$income) 
# if close to 0, its close to normal
# If skewness >0, distribution is skewed to right, 
#   more observations on left

# with kurtosis
library(moments) 
kurtosis(myDataDsdc$income) 
# if kurtosis ~3, its near normal
# kurtosis >3, distro had higher peak and thin tails, not normal 
# kurtosis <3, distro has thicker tails and lower peak, not normal 

# Shapiro–Wilk test
library("dplyr")
# library("ggpubr")
shapiro.test(myDataDsdc$income)  

# Kolmogorov-Smirnov test
library("dgof")
# ks.test(sample1, sample2) # need two vars, yet to verify                      ? 