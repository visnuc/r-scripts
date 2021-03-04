# =================
#   Wilcoxon test
# =================

# import data set in csv format 
myData <- read.csv(file.choose(), header=T) 

# factorization 
library(plyr)
myDataDsdc$gender <- as.factor(myDataDsdc$gender)
myDataDsdc$gender <- revalue(myDataDsdc$gender, c("0"="female", "1"="male"))
View(myDataDsdc)
boxplot(myDataDsdc$income ~ myDataDsdc$gender)

# Ho: Median income of men = that of women 
# two sided test 
wilcox.test(myDataDsdc$income ~ myDataDsdc$gender, mu = 0, 
            alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, 
            exact = T, correct = T) 
# exact = T, may produce errors if there are ties in ranks of observations 