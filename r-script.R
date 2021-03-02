# setting custom working directory
setwd("/my/desired/directory/for/r") 

# import data set in csv format 
myData <-read.csv(file.choose(), header=T) 
# imports data directly 
myData <-read.csv("/source/of/data/my-data.csv",
                         header=T) 

# to attach data set 
# attach(practice_data) 

# get variable names
names(myData) 

# to view data set 
View(myData)

# basic descriptive statistics 
mean(myData$income) 
median(myData$income)
summary(myData$income)

# sort by income
myData <- myData[order(myData$income), ]   


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


# ===========
#   T-TESTs
# ===========

# ---------------------
#   One sample t-test 
# ---------------------
# e.g., there is a data where Hb of children was found as X. Now, I want to check
# whether the children of this hospital has the same level of Hb, or more, or less. 
# As there is only one sample to test, and see whether my hypothesis is right or 
# wrong, we will be doing one sample t-test. 

#   Assumptions
# ---------------
#   1. define null and alternate hypothesis 
#         e.g., H0 - no differences in means 
#               Ha - difference exists
#   2. State Alpha 
#         α = 0.05                                                              ?
#   3. calculate df
#         df = (n - 1)
#   4. state decision rule 
#         two tailed or one tailed                                              ?
#         define t's range to accept/reject H0                                  ?
#   5. calculate test statistic
#   6. state results 
#   7. state conclusion 

install.packages("devtools")
# or
# install.packages("ggpubr")
myDataDsdc <-read.csv(file.choose(),header=T)
summary(myDataDsdc$income)

ostt <- t.test(myDataDsdc$income, mu = 100, alternative = "two.sided") 
ostt
# ostt$conf.int

# ---------------------------------------
# Unpaired/Independent sample t-test 
# ---------------------------------------
#   3. calculate df
#         df = [(n1 - 1) + (n2 - 1)]
t.test(myDataDsdc$income ~ myDataDsdc$gender, data=myDataDsdc, var.equal = TRUE)
t.test(myDataDsdc$income ~ myDataDsdc$gender, data=myDataDsdc, var.equal = F)
# var.equal T or F, which one when                                              ?

# -----------------------------------
# Paired/Dependent sample t-test 
# -----------------------------------
# e.g., we want to test whether a drug is good enough to reduce BP significantly
# we will take BP before the intake of the drug and reading after the intake, and 
# compare the BP. But, as the individuals are the same, we will have to be doing 
# paired t-test
# ------------------------------------------------------------------------------

# Can't do paired sample t-test when variables are not paired in this data set  ?
# code: t.test(y1, y2, paired=TRUE)



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
shapiro.test(myDataDsdc$income)  #                                              ? 

# Kolmogorov-Smirnov test
library("dgof")
# ks.test(sample1, sample2) # need two vars, yet to verify                      ? 


# factorization 
library(plyr)
myDataDsdc$gender <- as.factor(myDataDsdc$gender)
myDataDsdc$gender <- revalue(myDataDsdc$gender, c("0"="female", "1"="male"))
View(myDataDsdc)
boxplot(myDataDsdc$income ~ myDataDsdc$gender)

# Ho: Median income of men = that of women 
# two sided test 
wilcox.test(myDataDsdc$income ~ myDataDsdc$gender, mu = 0, 
            alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, 
            exact = T, correct = T) 
# exact = T, may produce errors if there are ties in ranks of observations 


# =====================================
# Chi-square test: Goodness of fit
# =====================================
jobs <- c(11091, 11282, 15378, 12696) # observed 
names(jobs) <- c("Management", "Supply", "Service", "Quality")
jobs
jobs/sum(jobs)
probability <- c(0.25, 0.25, 0.25, 0.25) # expected / hypothetical 
# Ho: Probability of jobs in each category is 0.25
# Ha: Probability of jobs in each category is NOT same 
chisq.test(jobs, p=probability) # goodness of fit 

# =======================================
# Chi-square test: Test of independence
# =======================================
# converting integer to factor
library(plyr)
myDataDsdc$country <- as.factor(myDataDsdc$country)
myDataDsdc$country <- revalue(myDataDsdc$country, 
                              c("1"="Bangladesh", "2"="Brazil", "3"="India", 
                                "4"="Nepal", "5"="Pakistan", "7"="South Africa", 
                                "8"="Tanzania")) 
myDataDsdc$momedcat <- as.factor(myDataDsdc$momedcat)
myDataDsdc$momedcat <- revalue(myDataDsdc$momedcat, c("0"="Poor", "1"="Good"))
# making contingency table
contin_table <- table(myDataDsdc$country, myDataDsdc$momedcat)
contin_table
# test of independence 
chisq.test(contin_table)
chisq.test(contin_table)$observed
chisq.test(contin_table)$expected
# Monte Carlo simulation as expected freq <5
chisq.test(contin_table, simulate.p.value = T, B = 10000) 

# =======================================
# Chi-square test: Test of homogeneity
# =======================================
comm <- rbind(c(40, 40, 5, 7), c(41, 30, 15, 14))
dimnames(comm) <- list(Age = c("15-24", "25-34"), 
                       Tools = c("Phone", "IM", "email", "Others"))
comm
# Ho: Two groups share same proportion of communication method 
# Ha: Two groups DO NOT share same proportion of communication method 
chisq.test(comm, correct = F)
chisq.test(comm, correct = F)$expected

# ========================
# Fisher's Exact test
# ========================
contin_table
fisher.test(contin_table)



# ============================
#   START FROM HERE!!!!
# ============================

# =================
# McNemar's test
# =================
# Assumptions 
# ---------------
# variabls are nominal, paired, random, independent from another, not normal
# number of discordant pairs >= 30
# --------------
# Hypothesis
# --------------
# Ho: numbers in discordant cells are equal 
# Ha: numbers in discordant cells are UNEQUAL 

## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("approve", "disapprove"),
                         "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
## => significant change (in fact, drop) in approval ratings


# =================
# Binomial test 
# =================
## Under (the assumption of) simple Mendelian inheritance, a cross
##  between plants of two particular genotypes produces progeny 1/4 of
##  which are "dwarf" and 3/4 of which are "giant", respectively.
##  In an experiment to determine if this assumption is reasonable, a
##  cross results in progeny having 243 dwarf and 682 giant plants.
##  If "giant" is taken as success, the null hypothesis is that p =
##  3/4 and the alternative that p != 3/4.
binom.test(c(682, 243), p = 3/4)
binom.test(682, 682 + 243, p = 3/4)   # The same.
## => Data are in agreement with the null hypothesis.


# =================================
# did_difference_in_difference
# =================================
setwd("/home/visnu/MEGA/projects/r_statistics/dsdc_subhasish_das_chayon/did_difference_in_difference") 
did_data <-read.csv(file.choose(),header=T) 

# -----------
#   Try 001
# -----------
did_data$did <- did_data$group * did_data$post_treatment
didreg <- lm(house_price ~ group + post_treatment + did, data = did_data)
summary(didreg)

# -----------
#   Try 002
# -----------
# Suppose a policy implemented starting on 1994, 
#   only in countries E, G, and F. 
#   We want to know causal effect of the policy 
#   on a generic outcome y.  

library(foreign)
library(ggplot2)
library(lfe)
# Source: https://www.princeton.edu/~otorres/DID101R.pdf
didRaw <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
didRaw$y = didRaw$y/1e9
attach(didRaw)
names(didRaw)
View(didRaw)

# Graphical summary 
ggplot(didRaw, aes(x = year, y = y, group = country, color = country)) + 
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d() + 
  geom_vline(xintercept = 1993.9, linetype = "dashed")

didRaw$time <- ifelse(didRaw$year >= 1994, 1, 0)
didRaw$treated = ifelse(didRaw$country == "E" | 
                          didRaw$country == "F" |
                          didRaw$country == "G", 1, 0)
didRaw$interac <- didRaw$time * didRaw$treated

# DID
didReg <- lm(y ~ treated + time + interac, data = didRaw)
summary(didReg)

didReg1 <- lm(y ~ treated*time, data = didRaw)
summary(didReg1)

# -----------
#   Try 003
# -----------
library(foreign)
library(ggplot2)
library(lfe)
didDum <-read.dta(file.choose())
# didRaw$y = didRaw$y/1e9
attach(didDum)
names(didDum)
View(didDum)

# Graphical summary 
ggplot(didRaw, aes(x = year, y = y, group = country, color = country)) + 
  geom_line() + 
  geom_point() + 
  scale_color_viridis_d() + 
  geom_vline(xintercept = 1993.9, linetype = "dashed")

didRaw$time <- ifelse(didRaw$year >= 1994, 1, 0)
didRaw$treated = ifelse(didRaw$country == "E" | 
                          didRaw$country == "F" |
                          didRaw$country == "G", 1, 0)
didRaw$interac <- didRaw$time * didRaw$treated

# DID
didReg <- lm(y ~ treated + time + interac, data = didRaw)
summary(didReg)

didReg1 <- lm(y ~ treated*time, data = didRaw)
summary(didReg1)


# ===============================
# BINARY LOGISTIC REGRESSION 
# ===============================  
library(foreign)
t3ss_raw <-read.spss(file.choose(),header=T)
t3ss_raw <-read.csv(file.choose(),header=T) 
attach(t3ss_raw)
View(t3ss_raw)
names(t3ss_raw)

model <- glm(ho_re_str_01 ~ lab_ial_01 + lab_set_01 + 
               lab_virB_01 + lab_ipaBCD_01 + 
               lab_ipgB1_01 + lab_icsB_01 + 
               lab_ipgD_01 + lab_ipgF_01 + 
               lab_mxiH_01 + lab_mxiK_01 + 
               lab_mxiE_01 + lab_mxiC_01 + 
               lab_spa47_01 + lab_spa32_01, 
             data = t3ss_raw, 
             family = "binomial")
summary(model)

model <- glm(ho_re_str_01 ~ lab_set_01 + lab_ipgB1_01 + lab_icsB_01 + lab_mxiC_01, 
             data = t3ss_raw, family = "binomial")
summary(model)


# =================
# MERRGING DATA 
# =================
