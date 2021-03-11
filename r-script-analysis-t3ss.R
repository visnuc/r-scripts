# libraries
library(foreign)
library(tidyverse)
library(plyr)
library(MASS)

# import data
t3ss <- read.spss(file.choose(), header=T)
t3ss <- read.csv(file.choose(), header=T)

# set working directory
setwd("/home/visnu/Dropbox/tmp_sync/statistical_analysis")
getwd()

# ---------------------------------
# recode
t3ssReg <- t3ss %>%
  select(lab_id, 
         cf_temp_fever_01, 
         ho_bloody_01, ho_cough_01, ho_mucoid_01, ho_re_str_01, 
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

t3ssChisq <- t3ss %>%
  select(lab_ID, lab_Sero, lab_p140, 
         lab_ipaH, lab_ial_01, lab_set_01, lab_sen, 
         lab_virB_01, lab_ipaBCD_01, 
         lab_ipgC, lab_ipgB1_01, lab_ipgA, lab_icsB_01, 
         lab_ipgD_01, lab_ipgE, lab_ipgF_01, 
         lab_mxiH_01, lab_mxiI, lab_mxiK_01, lab_mxiE_01, lab_mxiC_01, 
         lab_spa15, lab_spa47_01, lab_spa32_01, lab_spa24, 
         ho_mucoid_01, ho_bloody_01, ho_vomiting_01, ho_abd_pain_01, 
         ho_re_str_01, ho_cough_01, ho_fever_01, ho_convul_01, 
         ho_measles_01, 
         cf_mod_sev_dis_01, cf_temp_fever_01, cf_eye_sunken_01, 
         cf_dry_mouth_01, cf_skin_pinch_slow_01, cf_restless_01, 
         cf_dh_01, cf_ped_ede_01, cf_rec_pro_01) %>%
  rename(lab_id = lab_ID, 
         lab_sero = lab_Sero) %>%
  mutate(ho_mucoid_01 = recode(ho_mucoid_01, Yes = 1, No = 0),
         ho_bloody_01 = recode(ho_bloody_01, Yes = 1, No = 0),
         ho_vomiting_01 = recode(ho_vomiting_01, Yes = 1, No = 0),
         ho_abd_pain_01 = recode(ho_abd_pain_01, Yes = 1, No = 0),
         ho_re_str_01 = recode(ho_re_str_01, Yes = 1, No = 0),
         ho_cough_01 = recode(ho_cough_01, Yes = 1, No = 0),
         ho_fever_01 = recode(ho_fever_01, Yes = 1, No = 0),
         ho_convul_01 = recode(ho_convul_01, Yes = 1, No = 0),
         ho_measles_01 = recode(ho_measles_01, Yes = 1, No = 0),
         cf_mod_sev_dis_01 = recode(cf_mod_sev_dis_01, Yes = 1, No = 0),
         cf_temp_fever_01 = recode(cf_temp_fever_01, Yes = 1, No = 0), 
         cf_eye_sunken_01 = recode(cf_eye_sunken_01, Yes = 1, No = 0), 
         cf_dry_mouth_01 = recode(cf_dry_mouth_01, Yes = 1, No = 0), 
         cf_skin_pinch_slow_01 = recode(cf_skin_pinch_slow_01, Yes = 1, No = 0), 
         cf_restless_01 = recode(cf_restless_01, Yes = 1, No = 0), 
         cf_dh_01 = recode(cf_dh_01, Yes = 1, No = 0), 
         cf_ped_ede_01 = recode(cf_ped_ede_01, Yes = 1, No = 0), 
         cf_rec_pro_01 = recode(cf_rec_pro_01, Yes = 1, No = 0), 
         lab_p140 = recode(lab_p140, Positive = 1, Negative = 0),
         lab_ipaH = recode(lab_ipaH, Positive = 1, Negative = 0),
         lab_ial_01 = recode(lab_ial_01, Positive = 1, Negative = 0),
         lab_set_01 = recode(lab_set_01, Positive = 1, Negative = 0),
         lab_sen = recode(lab_sen, Positive = 1, Negative = 0),
         lab_virB_01 = recode(lab_virB_01, Positive = 1, Negative = 0),
         lab_ipaBCD_01 = recode(lab_ipaBCD_01, Positive = 1, Negative = 0),
         lab_ipgC = recode(lab_ipgC, Positive = 1, Negative = 0),
         lab_ipgB1_01 = recode(lab_ipgB1_01, Positive = 1, Negative = 0),
         lab_ipgA = recode(lab_ipgA, Positive = 1, Negative = 0),
         lab_icsB_01 = recode(lab_icsB_01, Positive = 1, Negative = 0),
         lab_ipgD_01 = recode(lab_ipgD_01, Positive = 1, Negative = 0),
         lab_ipgE = recode(lab_ipgE, Positive = 1, Negative = 0),
         lab_ipgF_01 = recode(lab_ipgF_01, Positive = 1, Negative = 0),
         lab_mxiH_01 = recode(lab_mxiH_01, Positive = 1, Negative = 0),
         lab_mxiI = recode(lab_mxiI, Positive = 1, Negative = 0),
         lab_mxiK_01 = recode(lab_mxiK_01, Positive = 1, Negative = 0),
         lab_mxiE_01 = recode(lab_mxiE_01, Positive = 1, Negative = 0),
         lab_mxiC_01 = recode(lab_mxiC_01, Positive = 1, Negative = 0),
         lab_spa15 = recode(lab_spa15, Positive = 1, Negative = 0),
         lab_spa47_01 = recode(lab_spa47_01, Positive = 1, Negative = 0),
         lab_spa32_01 = recode(lab_spa32_01, Positive = 1, Negative = 0), 
         lab_spa24 = recode(lab_spa24, Posotive = 1, Negative = 0))

attach(t3ss)
names(t3ss)
View(t3ssChisq)
dim(t3ss)
str(t3ss)

# -------------------------------
# export data
write.table(t3ssChisq, 
            file = "t3ss_small_20210312_chisq_coded.csv", 
            sep = ",", 
            row.names = F)

# -------------------------------
# chi-square test of independence

# contingency table
contin_table <- table(t3ss$lab_ial_01, t3ss$ho_abd_pain_01)
contin_table <- table(t3ss$lab_ipaBCD_01, t3ss$ho_abd_pain_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_bloody_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_mucoid_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_re_str_01)
contin_table <- table(t3ss$lab_set_01, t3ss$cf_temp_fever_01)
contin_table <- table(t3ss$lab_set_01, t3ss$cf_dh)
contin_table <- table(t3ss$lab_spa24, t3ss$ho_re_str_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_cough_01)
contin_table <- table(t3ss$lab_ipgD_01, t3ss$ho_cough_01)
contin_table <- table(t3ss$lab_set_01, t3ss$cf_mod_sev_dis)
contin_table <- table(t3ss$lab_sen, t3ss$ho_bloody_01)
contin_table <- table(t3ss$lab_sen, t3ss$ho_mucoid_01)
contin_table <- table(t3ss$lab_sen, t3ss$ho_re_str_01)
contin_table
# test of independence 
chisq.test(contin_table)
chisq.test(contin_table)$observed
chisq.test(contin_table)$expected

# Monte Carlo simulation as expected freq <5
chisq.test(contin_table, simulate.p.value = T, B = 10000) 

# -------------------------------
# Fisher's Exact test
contin_table
fisher.test(contin_table)

# -------------------------------
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

# -------------------------------
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
