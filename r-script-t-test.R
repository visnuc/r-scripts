# ===============================
#   T-TESTs
# ===============================

# -------------------------------
# install 
install.packages("devtools")

# -------------------------------
# libraries
library("devtools")

# -------------------------------
# working directory
setwd("/my/desired/directory/for/r") 

# -------------------------------
# data import 
myData <-read.csv(file.choose(), header=T) 


# -------------------------------
#   One sample t-test 
# -------------------------------
# e.g., there is a data where Hb of children was found as X. 
# Now, I want to check whether the children of this hospital 
# has the same level of Hb, or more, or less. 
# As there is only one sample to test, and see whether my 
# hypothesis is right or wrong, we will be doing one sample t-test. 

# -------------------------------
#   Assumptions
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
# ---------------------------------------------

# Can't do paired sample t-test when variables are not paired in this data set  ?
# code: t.test(y1, y2, paired=TRUE)

