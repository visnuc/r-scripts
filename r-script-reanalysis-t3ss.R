# ---------------------------------
# libraries
library("arm")
library("car")
library("coefplot")
library("datasets")
library("dplyr")
library("foreign")
library("ggplot2")
library("MASS")
library("plotrix")
library("plyr")
library("reshape")
library("tidyverse")
library("utils")
library("visreg")


# ---------------------------------
# set working directory
setwd("~/MEGA/biotech/04_spring_2013/thesis_550_A/manuscript/manuscript/statistical_analysis/t3ss_data_set")
getwd()


# ---------------------------------
# list files in dir 
dir()


# ---------------------------------
# import data
t3ss <- read.spss(file.choose(), header=T)
t3ss <- read.dta(file.choose())
t3ss <- read.csv(file.choose(), header=T)


# -------------------------------
# others 
attach(t3ss)
names(t3ss)
View(t3ss)
dim(t3ss)
str(t3ss)


# -------------------------------
# others 
attach(t3ss)
names(t3ssReg)
View(t3ss)
dim(t3ssReg)
str(t3ss)


# -------------------------------
# export data
write.table(t3ss, 
            file = "haabijaabi.csv", 
            sep = ",", 
            row.names = F)


# ---------------------------------
#   subset 
t3ssReg <- subset(t3ss, select = c(lab_id, 
                                   cf_temp_fever_01, 
                                   ho_bloody_01, ho_cough_01, 
                                   ho_mucoid_01, ho_re_str_01, 
                                   reg_toxin, reg_virB, reg_ipaBCD_ipgC, 
                                   reg_ial, reg_ipgB1_spa15, 
                                   reg_ipgA_icsB, reg_ipgD_ipgE, 
                                   reg_ipgF, reg_mxiH_mxiI, reg_mxiK, 
                                   reg_mxiE, reg_mxiC, reg_spa47, 
                                   reg_spa32_spa24))


t3ssChisq <- subset(t3ss, 
                    select = c(lab_id, lab_sero, lab_p140, 
                               lab_ipaH, lab_ial_01, lab_set_01, lab_sen, 
                               lab_virB_01, lab_ipaBCD_01, lab_ipgC, 
                               lab_ipgB1_01, lab_ipgA, lab_icsB_01, 
                               lab_ipgD_01, lab_ipgE, lab_ipgF_01, 
                               lab_mxiH_01, lab_mxiI, lab_mxiK_01, 
                               lab_mxiE_01, lab_mxiC_01, lab_spa15, 
                               lab_spa47_01, lab_spa32_01, lab_spa24, 
                               ho_mucoid_01, ho_bloody_01, ho_vomiting_01, 
                               ho_abd_pain_01, ho_re_str_01, ho_cough_01, 
                               ho_fever_01, ho_convul_01, ho_measles_01,
                               cf_mod_sev_dis_01, cf_temp_fever_01, 
                               cf_eye_sunken_01, cf_dry_mouth_01, 
                               cf_skin_pinch_slow_01, cf_restless_01, 
                               cf_dh_01, cf_ped_ede_01, cf_rec_pro_01))

# t3ssDemo <- subset(t3ss, 
#                     select = c(lab_ID, 
#                                age_mon, sex, weight, y_s_pc, ys_p_15, 
#                                liv_h_6, elec, siz_hom, cul_lan, total, 
#                                sou_w_dr, bfc_hwp, bef_e_hw, af_ut_hw, 
#                                bcg, dpt_3rd, hib_3rd, pol_3rd, measl, 
#                                hepa_3rd))

# age_mon = demAgeMnth
# sex = demSex
# weight = demWeight
# y_s_pc = demYrSchlPC
# ys_p_15 = demYrSchlPC15
# liv_h_6 = demPplInH6m
# elec = demElec
# siz_cul = demOwnCulLandSize
# cul_lan = demOwnCulLand
# total = demTotIncomeMnth
# sou_w_dr = demSrcDrinkWtr
# bfc_hwp = demHndWashB4ChFeed
# bef_e_hw = demHndWashB4Eat
# af_ut_hw = demHndWashAftToilet
# bcg = vacBcg
# dpt_3rd = vacDpt
# hib_3rd = vacHib
# pol_3rd = vacPolio
# measl = vacMeasl
# hepa_3rd = vacHepB


# ---------------------------------
#   conditional variable 
t3ss$reg_ial <- t3ss$lab_ial_01
t3ss$reg_toxin <- ifelse(t3ss$lab_set_01 == 1 | t3ss$lab_sen == 1 , 1, 0)
t3ss$reg_virB <- t3ss$lab_virB
t3ss$reg_ipaBCD_ipgC <- ifelse(t3ss$lab_ipaBCD_01 == 1 & t3ss$lab_ipgC == 1 , 1, 0)
t3ss$reg_ipgB1_spa15 <- ifelse(t3ss$lab_ipgB1_01 == 1 & t3ss$lab_spa15 == 1 , 1, 0)
t3ss$reg_ipgA_icsB <- ifelse(t3ss$lab_ipgA == 1 & t3ss$lab_icsB_01 == 1 , 1, 0)
t3ss$reg_ipgD_ipgE <- ifelse(t3ss$lab_ipgD_01 == 1 & t3ss$lab_ipgE == 1 , 1, 0)
t3ss$reg_ipgF <- t3ss$lab_ipgF_01
t3ss$reg_mxiH_mxiI <- ifelse(t3ss$lab_mxiH_01 == 1 & t3ss$lab_mxiI == 1 , 1, 0)
t3ss$reg_mxiK <- t3ss$lab_mxiK_01
t3ss$reg_mxiE <- t3ss$lab_mxiE_01
t3ss$reg_mxiC <- t3ss$lab_mxiC_01
t3ss$reg_spa47 <- t3ss$lab_spa47_01
t3ss$reg_spa32_spa24 <- ifelse(t3ss$lab_spa32_01 == 1 & t3ss$lab_spa24 == 1 , 1, 0)



# ---------------------------------
#     removing variables 
t3ss$dis_cid <- NULL
t3ss$ho_mucoid <- NULL
t3ss$ho_bloody <- NULL
t3ss$ho_vomiting <- NULL
t3ss$ho_abd_pain <- NULL
t3ss$ho_re_str <- NULL
t3ss$ho_cough <- NULL
t3ss$ho_fever <- NULL
t3ss$ho_convul <- NULL
t3ss$ho_measles <- NULL

t3ss$cf_mod_sev_dis <- NULL
t3ss$cf_temp_fever <- NULL
t3ss$cf_eye_sunken <- NULL
t3ss$cf_dry_mouth <- NULL
t3ss$cf_skin_pinch_slow <- NULL
t3ss$cf_restless <- NULL
t3ss$cf_dh <- NULL
t3ss$cf_ped_ede <- NULL
t3ss$cf_rec_pro <- NULL


# ---------------------------------
#     recoding
t3ss$lab_p140 <- recode(t3ss$lab_p140, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipaH <- recode(t3ss$lab_ipaH, "Positive" = 1, "Negative" = 0)
t3ss$lab_ial_01 <- recode(t3ss$lab_ial_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_set1A <- recode(t3ss$lab_set1A, "Positive" = 1, "Negative" = 0)
t3ss$lab_set1B <- recode(t3ss$lab_set1B, "Positive" = 1, "Negative" = 0)
t3ss$lab_set_01 <- recode(t3ss$lab_set_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_sen <- recode(t3ss$lab_sen, "Positive" = 1, "Negative" = 0)
t3ss$lab_virB_01 <- recode(t3ss$lab_virB_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipaBCD_01 <- recode(t3ss$lab_ipaBCD_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipgC <- recode(t3ss$lab_ipgC, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipgB1_01 <- recode(t3ss$lab_ipgB1_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipgA <- recode(t3ss$lab_ipgA, "Positive" = 1, "Negative" = 0)
t3ss$lab_icsB_01 <- recode(t3ss$lab_icsB_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipgD_01 <- recode(t3ss$lab_ipgD_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipgE <- recode(t3ss$lab_ipgE, "Positive" = 1, "Negative" = 0)
t3ss$lab_ipgF_01 <- recode(t3ss$lab_ipgF_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_mxiH_01 <- recode(t3ss$lab_mxiH_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_mxiI <- recode(t3ss$lab_mxiI, "Positive" = 1, "Negative" = 0)
t3ss$lab_mxiK_01 <- recode(t3ss$lab_mxiK_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_mxiE_01 <- recode(t3ss$lab_mxiE_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_mxiC_01 <- recode(t3ss$lab_mxiC_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_spa15 <- recode(t3ss$lab_spa15, "Positive" = 1, "Negative" = 0)
t3ss$lab_spa47_01 <- recode(t3ss$lab_spa47_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_spa32_01 <- recode(t3ss$lab_spa32_01, "Positive" = 1, "Negative" = 0)
t3ss$lab_spa24 <- recode(t3ss$lab_spa24, "Positive" = 1, "Negative" = 0)
t3ss$lab_spa <- recode(t3ss$lab_spa, "Positive" = 1, "Negative" = 0)

t3ss$ho_mucoid_01 <- recode(t3ss$ho_mucoid_01, "Yes" = 1, "No" = 0)
t3ss$ho_bloody_01 <- recode(t3ss$ho_bloody_01, "Yes" = 1, "No" = 0)
t3ss$ho_vomiting_01 <- recode(t3ss$ho_vomiting_01, "Yes" = 1, "No" = 0)
t3ss$ho_abd_pain_01 <- recode(t3ss$ho_abd_pain_01, "Yes" = 1, "No" = 0)
t3ss$ho_re_str_01 <- recode(t3ss$ho_re_str_01, "Yes" = 1, "No" = 0)
t3ss$ho_cough_01 <- recode(t3ss$ho_cough_01, "Yes" = 1, "No" = 0)
t3ss$ho_fever_01 <- recode(t3ss$ho_fever_01, "Yes" = 1, "No" = 0)
t3ss$ho_convul_01 <- recode(t3ss$ho_convul_01, "Yes" = 1, "No" = 0)
t3ss$ho_measles_01 <- recode(t3ss$ho_measles_01, "Yes" = 1, "No" = 0)

t3ss$cf_mod_sev_dis_01 <- recode(t3ss$cf_mod_sev_dis_01, "Yes" = 1, "No" = 0)
t3ss$cf_temp_fever_01 = recode(t3ss$cf_temp_fever_01, "Yes" = 1, "No" = 0)
t3ss$cf_eye_sunken_01 <- recode(t3ss$cf_eye_sunken_01, "Yes" = 1, "No" = 0)
t3ss$cf_dry_mouth_01 <- recode(t3ss$cf_dry_mouth_01, "Yes" = 1, "No" = 0)
t3ss$cf_skin_pinch_slow_01 <- recode(t3ss$cf_skin_pinch_slow_01, "Yes" = 1, "No" = 0)
t3ss$cf_restless_01 <- recode(t3ss$cf_restless_01, "Yes" = 1, "No" = 0)
t3ss$cf_dh_01 <- recode(t3ss$cf_dh_01, "Yes" = 1, "No" = 0)
t3ss$cf_ped_ede_01 <- recode(t3ss$cf_ped_ede_01, "Yes" = 1, "No" = 0)
t3ss$cf_rec_pro_01 <- recode(t3ss$cf_rec_pro_01, "Yes" = 1, "No" = 0)


# -------------------------------
# chi-square test of independence

# contingency table
contin_table <- table(t3ss$lab_ial_01, t3ss$ho_abd_pain_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_mucoid_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_bloody_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_re_str_01)
contin_table <- table(t3ss$lab_sen, t3ss$ho_mucoid_01)
contin_table <- table(t3ss$lab_sen, t3ss$ho_bloody_01)
contin_table <- table(t3ss$lab_sen, t3ss$ho_re_str_01)

contin_table <- table(t3ss$lab_ipaBCD_01, t3ss$ho_abd_pain_01)
contin_table <- table(t3ss$lab_set_01, t3ss$cf_temp_fever_01)
contin_table <- table(t3ss$lab_set_01, t3ss$cf_dh)
contin_table <- table(t3ss$lab_spa24, t3ss$ho_re_str_01)
contin_table <- table(t3ss$lab_set_01, t3ss$ho_cough_01)
contin_table <- table(t3ss$lab_ipgD_01, t3ss$ho_cough_01)
contin_table <- table(t3ss$lab_set_01, t3ss$cf_mod_sev_dis)

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

# final models - unadjusted 
t3ssRegUnadj <- glm(cf_temp_fever_01 ~ reg_toxin, family = "binomial", data = t3ss) 
t3ssRegUnadj <- glm(ho_bloody_01 ~ reg_toxin, family = "binomial", data = t3ss) 
t3ssRegUnadj <- glm(ho_bloody_01 ~ reg_ipgA_icsB, family = "binomial", data = t3ss) 
t3ssRegUnadj <- glm(ho_cough_01 ~ reg_toxin, family = "binomial", data = t3ss)
t3ssRegUnadj <- glm(ho_mucoid_01 ~ reg_toxin, family = "binomial", data = t3ss)
t3ssRegUnadj <- glm(ho_mucoid_01 ~ reg_mxiC, family = "binomial", data = t3ss)
t3ssRegUnadj <- glm(ho_re_str_01 ~ reg_toxin, family = "binomial", data = t3ss)
t3ssRegUnadj <- glm(ho_re_str_01 ~ reg_ipgA_icsB, family = "binomial", data = t3ss)
t3ssRegUnadj <- glm(ho_re_str_01 ~ reg_mxiC, family = "binomial", data = t3ss)
t3ssRegUnadj <- glm(ho_re_str_01 ~ reg_ipgB1_spa15, family = "binomial", data = t3ss)

summary(t3ssRegUnadj)

# OR & 95% CI
round(exp(cbind(coef(t3ssRegUnadj), confint(t3ssRegUnadj))), 3)


# -------------------------------
# # Binary logistic regression - adjusted

# t3ssRegAdj <- glm(cf_temp_fever_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + 
#                     reg_mxiK + reg_mxiE + reg_mxiC + 
#                     reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss) # Warning message:
# # glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 
# summary(t3ssRegAdj)

# # working combinations 
# t3ssRegAdj <- glm(ho_bloody_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_mxiC + reg_spa47, 
#                   family = "binomial", 
#                   data = t3ss) 
# 
# t3ssRegAdj <- glm(ho_bloody_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_bloody_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiC + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_cough_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_mxiC + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_cough_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_mxiC + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_mucoid_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiC + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_mucoid_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_mucoid_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_mxiC + reg_spa47, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_re_str_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiC + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_re_str_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_spa47 + reg_spa32_spa24, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# t3ssRegAdj <- glm(ho_re_str_01 ~ reg_ial + reg_toxin + reg_virB + 
#                     reg_ipaBCD_ipgC + reg_ipgB1_spa15 + reg_ipgA_icsB + 
#                     reg_ipgD_ipgE + reg_ipgF + reg_mxiH_mxiI + reg_mxiK + 
#                     reg_mxiE + reg_mxiC + reg_spa47, 
#                   family = "binomial", 
#                   data = t3ss)
# 
# summary(t3ssRegAdj)


#   final models
t3ssRegAdj <- glm(ho_bloody_01 ~ reg_toxin + reg_ipgA_icsB, 
                  family = "binomial", 
                  data = t3ss) 

t3ssRegAdj <- glm(ho_mucoid_01 ~ reg_toxin + reg_mxiC, 
                  family = "binomial", 
                  data = t3ss)

t3ssRegAdj <- glm(ho_re_str_01 ~ reg_toxin + reg_ipgA_icsB + reg_mxiC + 
                    reg_ipgB1_spa15, 
                  family = "binomial", 
                  data = t3ss)

summary(t3ssRegAdj)

# OR & 95% CI
round(exp(cbind(coef(t3ssRegAdj), confint(t3ssRegAdj))), 3)


# -------------------------------
# plotting 
coefplot(t3ssRegAdj, innerCI = 0, outerCI = 1.96, intercept = F, 
         title = "", 
         xlab = "Regression coefficient at 95% CI", 
         ylab = "Predictor genes", 
         decreasing = T, 
         col = "skyblue2", 
         newNames = c(reg_toxin = "set | sen", 
                      reg_ipgA_icsB = "ipgA & icsB", 
                      reg_mxiC = "mxiC", 
                      reg_ipgB1_spa15 = "ipgB1 & spa15")) + 
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  geom_point(pch = 21)
