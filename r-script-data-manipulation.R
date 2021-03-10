# ========================
#   DATA MANIPULATION      
# ========================

# ---------------------------------
library("tidyverse")
library("reshape")
library("datasets")

# ---------------------------------
# setting working directory 
setwd("/home/visnu/Dropbox/projects_DSDC/study_stunting_vs_vaccination_dsdc/data/tmp_data")
setwd("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/tmp_data")
getwd()

# ---------------------------------
# data import
dataRaw <- read.csv(file.choose(), header=T)

washMain <- read.csv("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_wash_rev_final_v20210228.csv", 
                     header=T) 
vtMain <- read.csv("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_vax_titer_wdates_v20210228.csv", 
                     header=T) 
# vtMain <- read.csv("/home/visnu/Dropbox/projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_vax_titer_wdates_v20210228.csv", 
#                    header=T) 

attach(vtMain)
names(vtMain)

# install.packages("tidyverse")
library(tidyverse)

# ---------------------------------
# making smaller data set 
vt2 <- vtMain %>%
  select(Pid, target_month, 
         log2mea, log2tet, log2per, 
         log2rotaa, log2rotag,
         log2poligg, log2pol1n, log2pol2n, log2pol3n) %>%
  rename(pid = Pid, 
         month = target_month, 
         log2meas = log2mea, 
         log2teta = log2tet, 
         log2pertu = log2per, 
         log2rotaA = log2rotaa, 
         log2rotaG = log2rotag, 
         log2polG = log2poligg, 
         log2pol1 = log2pol1n, 
         log2pol2 = log2pol2n, 
         log2pol3 = log2pol3n)

vt3 <- vt2Wide %>%
  select(pid, log2measAvg, 
         log2tetaAvg, log2pertuAvg, 
         log2rotaAavg, log2roraGAvg, 
         log2polGAvg, log2pol1avg, 
         log2pol2avg, log2pol3avg) %>%
  rename(avgLog2mea = log2measAvg, 
         avgLog2tet = log2tetaAvg, 
         avgLog2pert = log2pertuAvg, 
         avgLog2rotA = log2rotaAavg, 
         avgLog2rotG = log2roraGAvg, 
         avgLog2polG = log2polGAvg, 
         avgLog2pol1 = log2pol1avg, 
         avgLog2pol2 = log2pol2avg, 
         avgLog2pol3 = log2pol3avg)

attach(vt3)
names(vt3)
as.numeric(vt3$avgLog2rotA)
dim(vt3)
str(vt3)
View(vt3)
sum(is.na(vt3))

nrow(washMain)
View(washMain)

# ---------------------------------
# exporting new data set as csv 
write.table(vt3, 
            file = "vt3_v20210310.csv", 
            sep = ",", 
            row.names = F)

# ---------------------------------
# long to wide - with reshape
install.packages("reshape")
library("reshape")
vt2Wide <- reshape(vt2, 
                   idvar = "pid", 
                   timevar = "month", 
                   direction = "wide")

attach(vt2Wide)
names(vt2Wide)
# export(vt2Wide, "vt2Wide.csv") # not working

write.table(vt2Wide, 
            file = "vt2Wide_v20210310.csv", 
            sep = ",", 
            row.names = F)

# ---------------------------------
# merging data sets 
wm3 <- merge(washMain, vt3, 
             by = "pid", 
             all.x = T, all.y = T)

write.table(wm3,
            file = "wm3_v20210310.csv",
            sep = ",",
            row.names = F)

attach(wm2)
names(wm2)
dim(vt3)


  
# ---------------------------------
# #   trials and errors
# # -----------------------
# # # lists preloaded data 
# # data()

# vtByMonth <- vaxTiter %>%
#   count(target_month)
#
# table(target_month)

# # long to wide - with reshape2
# install.packages("reshape2")
# library("reshape2")
# log2measWide <- dcast(vt2, 
#                       pid ~ month, 
#                       value.var = "log2meas",  
#                       fun.aggregate = sum)

# log2measWide$log2measAvg <- rowMeans(log2measWide[ ,c("7", "15", "24")], 
#                                      na.rm=TRUE)

# dim(log2measWide)
# View(log2rotaGWide)
# log2measWide[log2measWide == 0] <- NA

# log2measWide$log2measAvg <- rowMeans(log2measWide[ ,2:4], 
#                                      na.rm = TRUE)

# View(log2measWide)
# str(log2measWide)
# dim(log2measWide)
# 
# nrow(wm2)
# View(wm2)
# table(wm2$target_month)

# vtAll <- vaxTiter %>%
#   select(Pid, agedays, everything())

# detach(c(washMain, vtMain))
# dim(vt2) # row and column # nrow(vt2); ncol(vt2)

# class(wm2$target_month)
# class(wm2$log2meas)
# wm2$target_month <- as.factor(wm2$target_month)
# # wm2$target_month <- as.integer(wm2$target_month) 
# str(wm2)
# summary(wm2)
# 
# wm2 %>%
#   group_by(target_month) %>%
#   summarise(log2measAvg = mean(log2meas))
# 
# View(wm2)
# names(wm2)
# wm2 <- wm2[ , -1]