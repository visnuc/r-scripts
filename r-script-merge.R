# ========================
#   DATA MANIPULATION      
# ========================

# importing data 
dataRaw <-read.csv(file.choose(), header=T) 

washMain <- dataRaw
vaxTiter <- dataRaw

View(washMain)
names(washMain)

View(vaxTiter)
names(vaxTiter)

library(tidyverse)
# vtAll <- vaxTiter %>%
#   select(Pid, agedays, everything())

vt <- vaxTiter %>%
  select(Pid, agedays, 
         log2mea, log2tet, log2per, 
         log2rotaa, log2rotag,
         log2poligg, log2pol1n, log2pol2n, log2pol3n) %>%
  rename(pid = Pid, 
         log2meas = log2mea, 
         log2teta = log2tet, 
         log2pertu = log2per, 
         log2rotaA = log2rotaa, 
         log2rotaG = log2rotag, 
         log2polG = log2poligg, 
         log2pol1 = log2pol1n, 
         log2pol2 = log2pol2n, 
         log2pol3 = log2pol3n)

View(vt)

nrow(vt)
nrow(washMain)
nrow(washMain2)

ncol(vt)
ncol(vaxTiter)
ncol(washMain)

washMain2 <- merge(washMain, vt, by = "pid", all.x = T)
write.table(washMain2, file = "data_wash_2_v20210302.csv", sep = ",", row.names = F)


