# =====================================
# Chi-square test: Goodness of fit
# =====================================
jobs <- c(11091, 11282, 15378, 12696) # observed 
names(jobs) <- c("Management", "Supply", "Service", "Quality")
jobs
jobs/sum(jobs)
probability <- c(0.25, 0.25, 0.25, 0.25) # expected / hypothetical 
# Ho: Probability of jobs in each category is 0.25
# Ha: Probability of jobs in each category is NOT same 
chisq.test(jobs, p=probability) # goodness of fit 

# =======================================
# Chi-square test: Test of independence
# =======================================
# converting integer to factor
library(plyr)
myDataDsdc$country <- as.factor(myDataDsdc$country)
myDataDsdc$country <- revalue(myDataDsdc$country, 
                              c("1"="Bangladesh", "2"="Brazil", "3"="India", 
                                "4"="Nepal", "5"="Pakistan", "7"="South Africa", 
                                "8"="Tanzania")) 
myDataDsdc$momedcat <- as.factor(myDataDsdc$momedcat)
myDataDsdc$momedcat <- revalue(myDataDsdc$momedcat, c("0"="Poor", "1"="Good"))
# making contingency table
contin_table <- table(myDataDsdc$country, myDataDsdc$momedcat)
contin_table
# test of independence 
chisq.test(contin_table)
chisq.test(contin_table)$observed
chisq.test(contin_table)$expected
# Monte Carlo simulation as expected freq <5
chisq.test(contin_table, simulate.p.value = T, B = 10000) 

# =======================================
# Chi-square test: Test of homogeneity
# =======================================
comm <- rbind(c(40, 40, 5, 7), c(41, 30, 15, 14))
dimnames(comm) <- list(Age = c("15-24", "25-34"), 
                       Tools = c("Phone", "IM", "email", "Others"))
comm
# Ho: Two groups share same proportion of communication method 
# Ha: Two groups DO NOT share same proportion of communication method 
chisq.test(comm, correct = F)
chisq.test(comm, correct = F)$expected

# ========================
# Fisher's Exact test
# ========================
contin_table
fisher.test(contin_table)