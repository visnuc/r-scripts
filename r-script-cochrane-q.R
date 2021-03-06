# ==========================
#   COCHRANE Q TEST   
# ==========================

ccq <- read.table(file.choose(), h = T)
attach(ccq)
names(ccq)
View(ccq)

ccq$resp <- c(1:150)
View(ccq)

# Assumption 1: Participants are randomly selected from 
#   the population 
# Assumption 2: Participants are independent from 
#   one another 
# Assumption 3: Number of participants that did not 
#   respond the same for all conditions (n) should be 
#   more than or equal to to 4  
# Assumption 4: n multiplied by k (groups) is greater 
#   than or equal to 24 


# Cochran's Q test - using nonpar package 
install.packages("nonpar")
library(nonpar)

ccqMatrix <- cbind(ccq$munt, 
                   ccq$movies, 
                   ccq$tuschin, 
                   ccq$arena)

cochrans.q(ccqMatrix)

# # Cochran's Q test - using RVAideMemoire package 
# install.packages("RVAideMemoire")
# library(RVAideMemoire)
# 
# names(ccq)
# ccqLong <- reshape(ccq, 
#                    varying = c("munt", "movies", 
#                                "tuschin", "arena"), 
#                    v.names = "score", 
#                    timevar = "cinema", 
#                    times = c("munt", "movies", 
#                              "tuschin", "arena"), 
#                    new.row.names = 1:1000, 
#                    direction = "long")
# 
# View(ccqLong)
# 
# cochran.qtest(score ~ cinema | resp, 
#               data = ccqLong)


# Post hoc Dunn test - using PMCMRplus package 
ccqList <- list(ccq$munt, 
                ccq$movies, 
                ccq$tuschin, 
                ccq$arena)

install.packages("PMCMR")
library(PMCMR)
# install.packages("PMCMRplus")
# library(PMCMRplus)
posthoc.kruskal.dunn.test(ccqList, 
                          p.adjust.method = "bonferroni")

# # Post hoc Dunn test - using FSA package 
# library(FSA)
# # needs data set in long format 
# dunnTest(ccqLong$score, 
#          ccqLong$cinema, 
#          method = "bonferroni")
