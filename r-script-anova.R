# ==================
#   ONE-WAY ANOVA  
# ==================

library(tidyverse)

# data import
ow <- read.table(file.choose(), h=T)

names(ow)
View(ow)

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
# If p<0.05, we reject the Ho meaning 
# the mean difference in lengths between groups 
# in these three groups are significantly different.
# However, which groups are different can not 
# be deduced from this test. 

# Post-hoc TukeyHSD test
#   to test which of the groups have different means
TukeyHSD(aov(model1))