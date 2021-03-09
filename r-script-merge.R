# ========================
#   DATA MANIPULATION      
# ========================

# setting checking working directory 
setwd("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data")
getwd()

# importing data 
dataRaw <- read.csv(file.choose(), header=T)

washMain <- read.csv("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_wash_rev_final_v20210228.csv", 
                     header=T) 
vtMain <- read.csv("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_vax_titer_wdates_v20210228.csv", 
                     header=T) 

attach(c(washMain, vtMain))
View(washMain)
View(vtMain)
nrow(washMain)
nrow(vtMain)

# # number of rows and columns
# nrow(vt)
# ncol(vt)
# # gets names of variables
# names(vt)
# # view data set, from tidyverse
# view(vt)
# # view data set, default
# View(vt)

names(washMain)
names(vtMain)

# install.packages("tidyverse")
library(tidyverse)
# vtAll <- vaxTiter %>%
#   select(Pid, agedays, everything())

vt2 <- vtMain %>%
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
         log2pol3 = log2pol3n)

View(vt2)
nrow(vt2)
write.table(vt2, 
            file = "vt2_v20210309.csv", 
            sep = ",", 
            row.names = F)

table(target_month)
str(vt2)

wm2 <- merge(washMain, vt2, 
             by = "pid", 
             all.x = T) %>%
  na.omit()

nrow(wm2)
View(wm2)
table(wm2$target_month)

write.table(wm2,
            file = "wm2_v20210309.csv",
            sep = ",",
            row.names = F)

class(wm2$target_month)
class(wm2$log2meas)
wm2$target_month <- as.factor(wm2$target_month)
str(wm2)
summary(wm2)

# # -----------------------
# #   trials and errors
# # -----------------------
# # # lists preloaded data 
# # data()
# 
# vt2 <- vtMain %>%
#   select(Pid, agedays, target_month, 
#          log2mea, log2tet, log2per, 
#          log2rotaa, log2rotag,
#          log2poligg, log2pol1n, log2pol2n, log2pol3n) %>%
#   rename(pid = Pid, 
#          log2meas = log2mea, 
#          log2teta = log2tet, 
#          log2pertu = log2per, 
#          log2rotaA = log2rotaa, 
#          log2rotaG = log2rotag, 
#          log2polG = log2poligg, 
#          log2pol1 = log2pol1n, 
#          log2pol2 = log2pol2n, 
#          log2pol3 = log2pol3n) %>%
#   na.omit() %>%
#   group_by(target_month) %>%
#   summarise(avg_titer_measles = mean(log2meas), 
#             avg_titer_tetanus = mean(log2teta), 
#             avg_titer_pertussis = mean(log2pertu), 
#             avg_titer_rotavirusIgA = mean(log2rotaA), 
#             avg_titer_rotavirusIgG = mean(log2rotaG), 
#             avg_titer_polioIgG = mean(log2polG), 
#             avg_titer_polioS1 = mean(log2pol1), 
#             avg_titer_polioS2 = mean(log2pol2), 
#             avg_titer_polioS3 = mean(log2pol3)
#             ) 
# 
# vtByMonth <- vaxTiter %>%
#   count(target_month)
#

detach(c(washMain, vtMain))