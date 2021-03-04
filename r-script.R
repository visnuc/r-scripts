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


# ============================
#   START FROM HERE!!!!
# ============================


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
