# ====================
#   FRIEDMAN TEST
# ====================

# import, attach, view, var names
ft <- read.csv(file.choose(), h=T)
attach(ft)
View(ft)
names(ft)

# data structure
str(ft)

# adding new var "ID" 
ft2 <- ft
ft2$ID <- seq.int(nrow(ft2))
names(ft2)
View(ft2)

# from wide to long format 
install.packages("reshape2")
library(reshape2)
ftLong <- melt(ft2, id.vars = c("ID"))
names(ftLong)
View(ftLong)

# Assumption 1: One group measured 
#   on ≥3 different occasions
# Assumption 2: Group is a random sample 
#   from the population
# Assumption 3: Dependent variable should 
#   be measured at ordinal/continuous level
# Assumption 4: Samples do NOT need to be 
#   normally distributed

# Friedman test: using “stats” package
library(stats)
friedman.test(ftLong$value, 
              ftLong$variable, 
              ftLong$ID)
# Interpretation e.g.: 
#   In the analysis of our imaginary data set 
# of treatment response in three consecutive 
# weeks, it can be said that there is no 
# statistically significant difference 
# (p-value 0.3114 <0.05) between the 
# treatment response, while 
# Friedman chi-squared test statistic 
# is 2.3333, degrees of freedom is 2, and 
# sample size is 6. 
