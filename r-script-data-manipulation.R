# =================================
#   DATA MANIPULATION      
# =================================
# install packages
install.packages("reshape")

# ---------------------------------
# libraries
library("tidyverse")
library("reshape")
library("datasets")

# ---------------------------------
# working directory 
setwd("~/Dropbox/projects_DSDC/study_stunting_vs_vaccination_dsdc/data")
setwd("C:/Users/visnu.pritom/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/tmp_data")
getwd()


# ---------------------------------
# data import
dataRaw <- read.csv(file.choose(), header=T)

# on Windows + Linux
washMain <- read.csv("~/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_wash_rev_final_v20210228.csv", 
                     header=T) 
vtMain <- read.csv("~/Dropbox/Projects_DSDC/study_stunting_vs_vaccination_dsdc/data/data_vax_titer_wdates_v20210228.csv", 
                     header=T) 


# ---------------------------------
# subsetting 
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


# ---------------------------------
# others
attach(vt3)
dim(vt3)
names(vt3)
as.numeric(vt3$avgLog2rotA)
ncol(vt3)
nrow(vt3)
str(vt3)
sum(is.na(vt3))
View(vt3)


# ---------------------------------
# exporting new data set as csv 
write.table(vt3, 
            file = "vt3_v20210310.csv", 
            sep = ",", 
            row.names = F)


# ---------------------------------
#   long to wide format 

# install.packages("reshape")
# library("reshape")
vt2Wide <- reshape(vt2, 
                   idvar = "pid", 
                   timevar = "month", 
                   direction = "wide")


# ---------------------------------
# merging data sets 
wm3 <- merge(washMain, vt3, 
             by = "pid", 
             all.x = T, all.y = T)


# ---------------------------------
# remove NAs
wm4 <- wm3 %>%
  na.omit()

# checking for NAs
any(is.na(wm4))

# ---------------------------------
# remove certain values from data frame
for(i in names(wm3)){
  wm3[[i]][wm3[[i]] == "#DIV/0!"] = NA
}
