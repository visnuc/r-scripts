# =================================
#   Difference in Differences 
# =================================

# ---------------------------------
# Assumptions
#   1: Treatment and control groups have Parallel Trends in outcome
#   2: Allocation of intervention was not determined by outcome
#   3: Composition of intervention and comparison groups is stable 
#     for repeated cross-sectional design

# ---------------------------------
# Problem e.g.: 
#   An imaginary policy implementation began in 1994, but, only 
# in countries E, G, and F. We want to know causal effect 
# of the policy on a generic outcome y.

# ---------------------------------
library(foreign)
library(ggplot2)
library(lfe)

# ---------------------------------
# Source: https://www.princeton.edu/~otorres/DID101R.pdf
ddDt <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
attach(ddDt)
names(ddDt)
dim(ddDt)
str(ddDt)
ddDt$y = ddDt$y/1e9
View(ddDt)

# ---------------------------------
# Visualization
ggplot(ddDt, aes(x = year, y = y, group = country, color = country)) + 
  geom_line() +
  geom_point() +
  scale_color_viridis_d() +
  geom_vline(xintercept = 1993.9, linetype = "dashed")

# ---------------------------------
# creating a new variable time,
#   where the point of intervention is 1994.
# Any time after 1994 will be coded as 1, 
# and the rests would be as 0.
ddDt$time <- ifelse(ddDt$year >= 1994, 1, 0)

# creating a new variable treated,
#   countries receiving the treatment, 
# i.e., E, F and G will be
# coded as 1, and the rests would be as 0.
ddDt$treated = ifelse(ddDt$country == "E" | 
                        ddDt$country == "F" | 
                        ddDt$country == "G", 1, 0)

# creating a new interaction variable 
#   between time and treatment, interac,
ddDt$interac <- ddDt$time * ddDt$treated

# ---------------------------------
# DID - unadjusted 
didReg <- lm(y ~ treated + time + interac, data = ddDt)
summary(didReg)
# Interpretation e.g.: 
#   From the DID analysis of our data, we can say that the intervention 
# has a significant - at 10% level of significance - negative effect on 
# the countries. The outcome y is 2.5 units less in those countries that 
# began to receive the intervention in 1994, compared to those countries 
# that did not received the intervention before 1994.

# ---------------------------------
# DID - adjusted
didDum <- read.dta(file.choose())
names(didDum)
attach(didDum)
names(didDum)
View(didDum)
summary(didDum)
didDum$treatment <- as.numeric(didDum$mnp)
# Recoding variable
didDum$treatment[didDum$treatment == 1] = 0
didDum$treatment[didDum$treatment == 2] = 1
# Creating new variable
didDum$interac <- didDum$time * didDum$treatment
# DID - adjusted test
didRegAdjusted <- lm(hemo ~ time + treatment + 
                       interac + age_m + 
                       cefsex + catasset, 
                     data = didDum)
summary(didRegAdjusted)
