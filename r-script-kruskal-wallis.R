# =======================
#   KRUSKAL-WALLIS TEST 
# =======================

# import, view, attach, and var names of data
kwt <- read.table(file.choose(), h=T)
View(kwt)
attach(kwt)
names(kwt)

# data structure
str(kwt)

# Assumption 1: All samples are independent, 
#   collected in >2 independent categorical groups
#   Label groups and set as categorical factors
kwt$Group <- as.factor(kwt$Group)
kwt$Group = factor(kwt$Group, 
                   labels = c("Wall lizard", 
                              "Viviparous lizard", 
                              "Snake-eyed lizard"))
class(kwt$Group)

# Assumption 2: Dependent variable is continuous
# Assumption 3: Normal distribution is not necessary
gr1 <- subset(kwt, Group == "Wall lizard")
gr2 <- subset(kwt, Group == "Viviparous lizard")
gr3 <- subset(kwt, Group == "Snake-eyed lizard")

qqnorm(gr1$Length); qqline(gr1$Length)
qqnorm(gr2$Length); qqline(gr2$Length)
qqnorm(gr3$Length); qqline(gr3$Length)

# Assumption 4: Homogeneneity of variances
#   Here we use Levene's test rather than Bartlett's test 
#   (= less vulnerable to deviation from normality)
#     Ho: data is NOT homogeneous
#     Ha: data is homogeneous
install.packages("car")
library(car)
leveneTest(Length ~ Group, data = kwt)
# Interpretation e.g.: 
#   As during the Levene's test to check the 
# Homogeneneity of variances we get a p-value 
# of 0.3437 (<0.05), we reject the null hypothesis 
# and may conclude that the data is homogeneous. 

# Kruskall Wallis test
kruskal.test(Length ~ Group, data = kwt)
# Interpretation e.g.: 
#   At this point from the Kruskal-Wallis test 
# it can be said that there is significant difference 
# in length in at least one of the three species 
# of the lizards, where degrees of freedom is 2 
# and p-value is 0.003388 (<0.01). 
# However, from this test alone it can not 
# be said where exactly this difference lies. 

# Post-hoc test - Dunn test for multiple comparisons
install.packages("FSA")
library(FSA)
dunnTest(Length ~ Group, 
         data = kwt, 
         method="bonferroni")
# Interpretation e.g.: 
#   To find out which species is larger and which one 
# is smaller Dunn test has been done with the 
# bonferroni correction, and we found that there 
# is a significant length difference between 
# the Viviparous lizard and the Wall lizard 
# (p-value 0.002<0.01). 
