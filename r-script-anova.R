# ==================
#   ONE-WAY ANOVA  
# ==================
library(tidyverse)

# data import
ow <- read.table(file.choose(), h=T)

# names(ow)
# View(ow)

# Assumption 1: All samples are independent, and 
#   collected in >2 independent categorical groups

# Label groups and set as categorical factors
ow$Group <- as.factor(ow$Group)
ow$Group = factor(ow$Group, 
                  labels = c("Wall_Lizard", 
                             "Viviparous_Lizard", 
                             "SnakeEyed_Lizard"))

class(ow$Group)

# Assumption 2: Dependent variable is continuous
# Assumption 3: Each group normally distribute, 
#   no major outliers

gr1 <- subset(ow, Group == "Wall_Lizard")
gr2 <- subset(ow, Group == "Viviparous_Lizard")
gr3 <- subset(ow, Group == "SnakeEyed_Lizard")

# checking normality with qqplot
qqnorm(gr1$Length)
qqline(gr1$Length)

qqnorm(gr2$Length)
qqline(gr2$Length)

qqnorm(gr3$Length)
qqline(gr3$Length)

# Assumption 4: Homogeneneity of variances
bartlett.test(Length ~ Group, data = ow)
# Interpretation e.g., 
# Bartlett test has been done 
# to test whether the groups are homogeneous 
# or heterogeneous, where 
#   Ho: The three groups have no heterogeneity 
#   Ha: The three groups have heterogeneity
# If p>0.05, we fail to reject the Ho, 
# indicating no significant heterogeneity in groups

# One Way ANOVA test:  
#   Ho: the means of the k (>2) populations are equal
model1 = lm(Length ~ Group, data = ow)
anova(model1)
# Interpretation e.g., 
# If p<0.05, we reject the Ho meaning 
# the mean difference in lengths between groups 
# in these three groups are significantly different.
# However, which groups are different can not 
# be deduced from this test. 

# Post-hoc TukeyHSD test
#   to test which of the groups have different means
TukeyHSD(aov(model1))


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