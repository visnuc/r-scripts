# ==========================================
#   Spearman's (rank) correlation test
# ==========================================

spc <- read.table(file.choose(), h=T)
attach(spc)
names(spc)
View(spc)

# Assumption 1: Samples consist of related pairs

# Spearman's rank order correlation (rho) measures 
# the strength (and direction) of association 
# between two variables
# while, 
# Correlation does NOT equal causation!

cor.test(Forest_size, Highest_tree, 
         method="spearman", data = spc)
# Interpretation e.g.: 
# In this analysis, we can see that there is 
# a strong positive relationship
# (Spearman’s rho value 0.91) between 
# the forest size and the tree height, 
# where p-value is < 2.2×10^16 (<0.001). 


# Data visualisation
# --------------------
# install.packages("ggplot2")
library(ggplot2)

qplot(Forest_size, Highest_tree, data = spc, 
      geom="point",
      xlab= expression(" Forest size " ~(ha)),
      ylab= expression(" Highest tree " ~(m)),
      size=I(2))+
  theme_bw()
