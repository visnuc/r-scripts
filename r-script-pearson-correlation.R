# ===========================
#   PEARSON CORRELATION 
# ===========================

pc <- read.table(file.choose(), h=T)
attach(pc)
View(pc)
names(pc)

# Assumption 1: Samples consist of related pairs
# Assumption 2: Both variables (x and y) are 
#   continuous and linearly related
plot(Forest_size, Highest_tree)

# Assumption 3: Variables follow 
#   a bivariate normal distribution
qqnorm(pc$Forest_size); qqline(pc$Forest_size)
qqnorm(pc$Highest_tree); qqline(pc$Highest_tree)

# Assumption 4: Homogeneity of variances - the 
#   variance of one variable should be stable at 
#   all levels of the other variable

# Assumption 5: No major outliers

# Pearson's correlation test, if two variables 
#   (x and y) are linearly correlated 
#   While, Correlation does NOT equal causation!

cor.test(Forest_size, 
         Highest_tree, 
         method="pearson", 
         data=pc)
# Interpretation e.g.: 
#   With a correlation co-efficient of 0.825 and 
# a p-value of 0.0002 (<0.01), while the degree 
# of freedom is 12, we reject the Ho that infers 
# the size of the forest and the height of the 
# highest tree is highly significantly correlated
# at 1% level of significance. 