# ========================
#   DATA MANIPULATION      
# ========================

# setting checking working directory 
setwd("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data")
getwd()

# importing data 
dataRaw <-read.csv(file.choose(), header=T) 

washMain <- dataRaw
vaxTiter <- dataRaw

# # gets number of rows 
# nrow(vt)
# # gets number of columns 
# ncol(vt)
# # gets names of variables
# names(vt)
# # view data set, from tidyverse 
# view(vt)
# view data set, default 
# View(vt)

# install.packages("tidyverse")
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


washMain2 <- merge(washMain, vt, by = "pid", all.x = T)
write.table(washMain2, file = "data_wash_2_v20210302.csv", sep = "," 
            , row.names = F)

# ---------------------
#   testing codes 
# ---------------------
# # lists preloaded data 
# data()

vtRaw <- vaxTiter %>%
  select(Pid, agedays, target_month, 
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
         log2pol3 = log2pol3n) %>%
  na.omit() %>%
  group_by(target_month) %>%
  summarise(avg_titer_measles = mean(log2meas), 
            avg_titer_tetanus = mean(log2teta), 
            avg_titer_pertussis = mean(log2pertu), 
            avg_titer_rotavirusIgA = mean(log2rotaA), 
            avg_titer_rotavirusIgG = mean(log2rotaG), 
            avg_titer_polioIgG = mean(log2polG), 
            avg_titer_polioS1 = mean(log2pol1), 
            avg_titer_polioS2 = mean(log2pol2), 
            avg_titer_polioS3 = mean(log2pol3)
            ) 

View(vtRaw)
write.table(vtRaw, file = "tmp_titer_status.csv", sep = "," 
            , row.names = F)

vtByMonth <- vaxTiter %>%
  count(target_month)

View(vtByMonth)
write.table(vtByMonth, file = "tmp_month_counts.csv", sep = "," 
            , row.names = F)