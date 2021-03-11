# =================================
#   Binary logistic regression 
# =================================

# ---------------------------------
library(foreign)
library(tidyverse)
library(MASS)

# ---------------------------------
# data import export 
# t3ss <- read.spss(file.choose(), header=T)
t3ss <- read.csv(file.choose(), header=T)

attach(t3ss)
names(t3ss)
View(t3ss)
dim(t3ss)
str(t3ss)

write.table(t3ssReg, 
            file = "t3ss_small_20210311_coded.csv", 
            sep = ",", 
            row.names = F)

# ---------------------------------
# recoding
t3ssReg <- t3ss %>%
  select(lab_id, 
         cf_temp_fever_01, ho_bloody_01, ho_cough_01, 
         ho_mucoid_01, ho_re_str_01, 
         lab_ial_01, lab_set_01, lab_virB_01, lab_ipaBCD_01, 
         lab_ipgB1_01, lab_icsB_01, lab_ipgD_01, lab_ipgF_01, 
         lab_mxiH_01, lab_mxiK_01, lab_mxiE_01, lab_mxiC_01, 
         lab_spa47_01, lab_spa32_01) %>%
  mutate(cf_temp_fever_01 = recode(cf_temp_fever_01, Yes = 1, No = 0), 
         ho_bloody_01 = recode(ho_bloody_01, Yes = 1, No = 0),
         ho_cough_01 = recode(ho_cough_01, Yes = 1, No = 0),
         ho_mucoid_01 = recode(ho_mucoid_01, Yes = 1, No = 0),
         ho_re_str_01 = recode(ho_re_str_01, Yes = 1, No = 0),
         lab_ial_01 = recode(lab_ial_01, Positive = 1, Negative = 0),
         lab_set_01 = recode(lab_set_01, Positive = 1, Negative = 0),
         lab_virB_01 = recode(lab_virB_01, Positive = 1, Negative = 0),
         lab_ipaBCD_01 = recode(lab_ipaBCD_01, Positive = 1, Negative = 0),
         lab_ipgB1_01 = recode(lab_ipgB1_01, Positive = 1, Negative = 0),
         lab_icsB_01 = recode(lab_icsB_01, Positive = 1, Negative = 0),
         lab_ipgD_01 = recode(lab_ipgD_01, Positive = 1, Negative = 0),
         lab_ipgF_01 = recode(lab_ipgF_01, Positive = 1, Negative = 0),
         lab_mxiH_01 = recode(lab_mxiH_01, Positive = 1, Negative = 0),
         lab_mxiK_01 = recode(lab_mxiK_01, Positive = 1, Negative = 0),
         lab_mxiE_01 = recode(lab_mxiE_01, Positive = 1, Negative = 0),
         lab_mxiC_01 = recode(lab_mxiC_01, Positive = 1, Negative = 0),
         lab_spa47_01 = recode(lab_spa47_01, Positive = 1, Negative = 0),
         lab_spa32_01 = recode(lab_spa32_01, Positive = 1, Negative = 0))

# View(t3ssReg)
# str(t3ssReg)
# names(t3ssReg)

# ---------------------------------
# Binary logistic regression - unadjusted

# t3ssRegUnadj <- glm(cf_temp_fever_01 ~ lab_set_01, 
#                       family = binomial(link = "logit"), 
#                       data = t3ss)

# t3ssRegUnadj <- glm(ho_bloody_01 ~ lab_set_01 + lab_icsB_01, 
#                     family = binomial(link = "logit"), 
#                     data = t3ss)

# t3ssRegUnadj <- glm(ho_cough_01 ~ lab_set_01 + lab_mxiE_01, 
#                     family = binomial(link = "logit"), 
#                     data = t3ss)

# t3ssRegUnadj <- glm(ho_mucoid_01 ~ lab_set_01 + lab_icsB_01, 
#                     family = binomial(link = "logit"), 
#                     data = t3ss)

t3ssRegUnadj <- glm(ho_re_str_01 ~ lab_set_01 + lab_icsB_01 + lab_mxiC_01, 
                    family = binomial(link = "logit"), 
                    data = t3ss)

summary(t3ssRegUnadj)

# OR & 95% CI
round(exp(cbind(coef(t3ssRegUnadj), confint(t3ssRegUnadj))), 3)


# Binary logistic regression - adjusted

t3ssRegAdj <- glm(ho_mucoid_01 ~ lab_ial_01 + lab_set_01 + 
                    lab_virB_01 + lab_ipaBCD_01 + 
                    lab_ipgB1_01 + lab_icsB_01 + 
                    lab_ipgD_01+ lab_ipgF_01 + 
                    lab_mxiH_01 + lab_mxiK_01 + 
                    lab_mxiE_01 + lab_mxiC_01 + 
                    lab_spa47_01 + lab_spa32_01, 
                  family = binomial(link = "logit"), 
                  data = t3ss)

summary(t3ssRegAdj)

# OR & 95% CI
round(exp(cbind(coef(t3ssRegAdj), confint(t3ssRegAdj))), 3)
