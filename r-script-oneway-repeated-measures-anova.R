# ===================================
# One way repeated-measures ANOVA
# ===================================
# Assumption 1: Dependent var should be measured 
#   at the interval or ratio level 
#   (i.e., continuous variable)
# Assumption 2: Independent var (i.e., 
#   the within-subjects factor) should consist 
#   ???2 categorical, "related groups"
# Assumption 3: No significant outliers in any 
#   of the related groups (i.e., in any levels 
#   of the within-subjects factor)
# Assumption 4: Distribution of dependent var 
#   in ???2 related groups should be approximately 
#   normally distributed
# Assumption 5: Known as sphericity, variances of 
#   differences between all combinations of 
#   related groups must be equal

library(tidyverse)
# importing data 
owrm <- read.csv(file.choose(), h=T)
attach(owrm)
View(owrm)
# names(owrm)
# head(owrm)
str(owrm)

# initiate variables for RM ANOVA
Subject <- factor(Subject)
Time <- factor(Time)

# checking line graph for relationship 
graphLine <- tapply(Response, Time, mean)
plot(graphLine, 
     type = "o", 
     xlab = "Time (weeks)", 
     ylab = "Response of tolerance")

# one-way RM anova test 
owrmTest <- aov(Response ~ Time + Error(Subject/Time))
summary(owrmTest)
# Interpretation e.g., 
# There was a significant effect of the painkillers 
# upon the tolerance in patients where F 27.4, 
# df (3, 15), p-value 2.47x10^-6 (<0.001). 
# However, where the difference lies can not be 
# understood from the One-way repeated-measures ANOVA 
# alone. 

# Bonferroni corrected post hoc comparisons 
library(stats)
pairwise.t.test(Response, 
                Time, 
                p.adjust.method = "bonferroni")
# Interpretation e.g.: 
#   From the post hoc analysis we can see that 
# there are statistically significant difference 
# in pain tolerance exists between all weeks except 
# in between 1st vs 2nd week, and 2nd vs 3rd week. 