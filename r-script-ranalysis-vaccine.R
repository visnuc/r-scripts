# ---------------------------------
#     libraries
# ---------------------------------
library("arm")
library("car")
library("codebook")
library("coefplot")
library("datasets")
library("devtools")
library("dplyr")
library("e1071")
library("foreign")
library("ggplot2")
library(ggpubr)
library("haven")
library("MASS")
library("moments") 
library("plotrix")
library(plyr)
library(reshape)
library(rstatix)
library(tidyverse)
library(utils)


# ---------------------------------
#     working directory 
# ---------------------------------
setwd("~/Dropbox/projects_DSDC/study_vaccine_stunting_dsdc/data")
setwd("C:/Users/visnu.pritom/Dropbox/tmp_sync/tmp_shbt/poster_presentation/del_tmp")


# ---------------------------------
#     list objects in dir 
# ---------------------------------
dir()



# ---------------------------------
#     data import 
# ---------------------------------
vaccine <- read.spss(file.choose(), header=T)
vaccine <- read.csv(file.choose(), header=T)



# ---------------------------------
#     others 
# ---------------------------------
attach(vaccine)
names(vaccine)
nrow(vaccine)
View(vaccine)
dim(vaccine)
str(vaccine)


# ---------------------------------
#     data export 
# ---------------------------------
write.table(vaccine, 
            file = "later.csv", 
            sep = ",", 
            row.names = F)



# ---------------------------------
# Normality check 
# ---------------------------------

#   with histogram 
hist(vaccine$momwt) 
hist(vaccine$momht) 
hist(vaccine$income) 
hist(vaccine$nrgpro) 
hist(vaccine$blength) 
hist(vaccine$bweight) 
hist(vaccine$blaz) # 
hist(vaccine$bwaz) # 
hist(vaccine$bwlz) # 
hist(vaccine$avgLog2mea) # 
hist(vaccine$avgLog2tet) # 
hist(vaccine$avgLog2pert) #
hist(vaccine$avgLog2rotA) # 
hist(vaccine$avgLog2rotG) # 
hist(vaccine$avgLog2polG) # 
hist(vaccine$avgLog2pol1) # 
hist(vaccine$avgLog2pol2) # 
hist(vaccine$avgLog2pol3) # 

#   with Q-Q plot                               
qqnorm(vaccine$momwt, pch = 1, frame = F); qqline(vaccine$momwt, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$momht, pch = 1, frame = F); qqline(vaccine$momht, col = "skyblue4", lwd = 1) # normal
qqnorm(vaccine$income, pch = 1, frame = F); qqline(vaccine$income, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$nrgpro, pch = 1, frame = F); qqline(vaccine$nrgpro, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$blength, pch = 1, frame = F); qqline(vaccine$blength, col = "skyblue4", lwd = 1) # near
qqnorm(vaccine$bweight, pch = 1, frame = F); qqline(vaccine$bweight, col = "skyblue4", lwd = 1) # normal
qqnorm(vaccine$blaz, pch = 1, frame = F); qqline(vaccine$blaz, col = "skyblue4", lwd = 1) # near
qqnorm(vaccine$bwaz, pch = 1, frame = F); qqline(vaccine$bwaz, col = "skyblue4", lwd = 1) # near
qqnorm(vaccine$bwlz, pch = 1, frame = F); qqline(vaccine$bwlz, col = "skyblue4", lwd = 1) # normal
qqnorm(vaccine$avgLog2mea, pch = 1, frame = F); qqline(vaccine$avgLog2mea, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2tet, pch = 1, frame = F); qqline(vaccine$avgLog2tet, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2pert, pch = 1, frame = F); qqline(vaccine$avgLog2pert, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2rotA, pch = 1, frame = F); qqline(vaccine$avgLog2rotA, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2rotG, pch = 1, frame = F); qqline(vaccine$avgLog2rotG, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2polG, pch = 1, frame = F); qqline(vaccine$avgLog2polG, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2pol1, pch = 1, frame = F); qqline(vaccine$avgLog2pol1, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2pol2, pch = 1, frame = F); qqline(vaccine$avgLog2pol2, col = "skyblue4", lwd = 1) # not 
qqnorm(vaccine$avgLog2pol3, pch = 1, frame = F); qqline(vaccine$avgLog2pol3, col = "skyblue4", lwd = 1) # not

#   Shapiro–Wilk test
shapiro.test(vaccine$momwt) # p = 1.18e-12
shapiro.test(vaccine$momht) # p = 0.022, variable not normal
shapiro.test(vaccine$income) # p < 2.2e-16
shapiro.test(vaccine$nrgpro) # p = 6.833e-07
shapiro.test(vaccine$blength) # p = 0.002515
shapiro.test(vaccine$bweight) # p = 0.7243, normal
shapiro.test(vaccine$blaz) # p = 3.501e-05
shapiro.test(vaccine$bwaz) # p = 0.0002563
shapiro.test(vaccine$bwlz) # p = 0.4151, normal 
shapiro.test(vaccine$avgLog2mea) # p = 3.763e-13
shapiro.test(vaccine$avgLog2tet) # p < 2.2e-16
shapiro.test(vaccine$avgLog2pert) # p = 9.53e-12
shapiro.test(vaccine$avgLog2rotA) # p = 1.784e-11
shapiro.test(vaccine$avgLog2rotG) # p = 2.444e-11
shapiro.test(vaccine$avgLog2polG) # p = 1.808e-11
shapiro.test(vaccine$avgLog2pol1) # p < 2.2e-16
shapiro.test(vaccine$avgLog2pol2) # p < 2.2e-16
shapiro.test(vaccine$avgLog2pol3) # p < 2.2e-16


#   with stem-leaf plot 
stem(vaccine$momwt)

#   with boxplot
boxplot(vaccine$momwt)

#   with skewness
skewness(vaccine$momwt) 
# if close to 0, its close to normal
# if >0, distribution is skewed to right, more observations on left

#   with kurtosis
kurtosis(vaccine$momwt) 
# if kurtosis ~3, its near normal
# kurtosis >3, distro had higher peak and thin tails, not normal 
# kurtosis <3, distro has thicker tails and lower peak, not normal 




# ---------------------------------
#     subsetting 
# ---------------------------------
shock <- shock %>%
  select(pp_pid, pp_age_total_mnth, pp_sex_bivar, 
         socio_restype_notbuilding_bivar, socio_water_bivar, 
         pedi_imm_bivar, pedi_dev_bivar, pedi_birth_bivar, 
         pedi_feed_bivar, anthro_wt, anthro_ht, 
         hosp_hosp_stay, 
         cc_diarr_bivar, cc_diarr_d, cc_st_cons_bivar, cc_vis_bld_bivar, 
         cc_cough_bivar, cc_cough_d, cc_resp_dist_bivar, cc_resp_dist_d, 
         cc_fever_bivar, cc_fever_d, 
         prevrx_currdx_abx_bivar, past_chd_bivar, past_tri21_bivar, 
         cf_rr, cf_rr_fast_br_bivar, cf_lung_crepts_bivar, 
         cf_lcwi_bivar, cf_hypoxia_bivar, cf_p_rate, cf_p_char_bivar, 
         cf_map_bivar, cf_crt_bivar, cf_peri_bivar, cf_uo_bivar, 
         cf_dh_bivar, cf_ede_bivar, cf_hightemp_bivar, cf_abd_dist_bivar, 
         cf_bs_slug_bivar, cf_rbs_bivar, 
         mx_line_1_bivar, mx_line_2_bivar, mx_line_3_bivar, 
         mx_line_4_bivar, mx_steroid_bivar, 
         dx_sclere_bivar, dx_s_pneu_bivar, dx_haip_bivar, 
         dx_conv_bivar, dx_hypogly_bivar, dx_hyper_na_bivar, 
         dx_ei_oth_bivar, dx_ileus_bivar, dx_samuw_bivar, 
         dx_awd_bivar, dx_id_bivar, dx_pd_bivar, dx_aki_bivar,
         inv_bl_cs_bivar, inv_rs_cs_bivar, inv_st_cs_bivar,
         inv_st_rs_cs_bivar, inv_u_cs_bivar, inv_csf_cs_bivar,
         inv_trac_cs_bivar, inv_trac_cs_multi_org_bivar,
         inv_biochem_hyper_na_bivar, inv_biochem_hypo_na_bivar,
         inv_biochem_hyper_k_bivar, inv_biochem_hypo_k_bivar,
         inv_biochem_met_acido_bivar, inv_biochem_ag_high_bivar,
         inv_biochem_cr_hi_bivar, inv_biochem_cr_aki_bivar,
         inv_biochem_ca_low_bivar,
         inv_biochem_hi_mg_bivar, inv_biochem_low_mg_bivar,
         inv_hem_mod_anemia_9.3, inv_hem_leukocytosis_bivar,
         inv_hem_neutrophilia_bivar, inv_hem_neutropenia_bivar,
         inv_hem_lymphocytosis_bivar, inv_hem_lympho_penia_bivar,
         inv_hem_thombocytopenia_bivar,
         gap_shk_bt, gap_shk_bt_2h, gap_shk_bt_3h,
         gap_shk_bt_4h, gap_shk_bt_6h,
         outcome_bivar, bt_cause_bivar,
         reg_mod_anemia, reg_meropenem, reg_steroids, reg_outcome,
         reg_sclerema, reg_sev_pneumonia, reg_hai_hap) 



# ---------------------------------
#     removing var
# ---------------------------------
vaccine$later <- NULL


# ---------------------------------
#     renaming var
# ---------------------------------
names(shock)[4] <- "socio_restype_bivar"


# ---------------------------------
#     recoding 
# ---------------------------------
shock$past_tri21_bivar <- recode(shock$past_tri21_bivar, "yes" = 1, "no" = 0)


# ---------------------------------
#     two-way contingency table
# ---------------------------------
continTable <- table(vaccine$reversed, vaccine$momedcat)
continTable <- table(vaccine$reversed, vaccine$gender)
continTable <- table(vaccine$reversed, vaccine$treatwat)
continTable <- table(vaccine$reversed, vaccine$chick)

rownames(continTable) <- c("No", "Yes")
colnames(continTable) <- c("5yr_or_less", ">5_yr")
colnames(continTable) <- c("Female", "Male")
colnames(continTable) <- c("No_treatment", "Treats")  
colnames(continTable) <- c("No", "Yes")

continTable

# class(continTable)
barplot(continTable, 
        legend = F, 
        beside = T, 
        main = "Death & Survival")

# relative frequencies percentage 
prop.table(continTable)*100
prop.table(continTable, 1)*100 # conditional, row-wise
prop.table(continTable, 2)*100 # conditional, column-wise

barplot(prop.table(continTable, 2)*100, 
        legend = F, 
        beside = T, 
        main = "Death & Survival by Gender")



# ---------------------------------
#   Chi-square test of independence
# ---------------------------------
chisq.test(continTable)
chisq.test(continTable)$observed
chisq.test(continTable)$expected

# Monte Carlo simulation as expected freq <5
chisq.test(continTable, simulate.p.value = T, B = 10000) 


# -------------------------------
#   Fisher's Exact test
# ---------------------------------
fisher.test(continTable)



# ---------------------------------------
# Unpaired/Independent sample t-test 
# ---------------------------------------
#   3. calculate df
#         df = [(n1 - 1) + (n2 - 1)]
t.test(vaccine$bweight ~ vaccine$reversed, data=vaccine, var.equal = T)
t.test(vaccine$bwlz ~ vaccine$reversed, data=vaccine, var.equal = T)
# t.test(myDataDsdc$income ~ vaccine$reversed, data=myDataDsdc, var.equal = F)
# var.equal T or F, which one when  

vaccine %>%
  group_by(reversed) %>%
  get_summary_stats(bweight, type = "mean_sd")



# -------------------------------
#   Wilcoxon test
# -------------------------------
vaccine$reversed <- as.factor(vaccine$reversed)
vaccine$reversed <- revalue(vaccine$reversed, c("0"="Not-reversed", "1"="Reversed"))
boxplot(vaccine$momwt ~ vaccine$reversed)

# two sided test 
# exact = T, may produce errors if there are ties in ranks of observations 
wilcox.test(vaccine$avgLog2pol2 ~ vaccine$reversed, 
            mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, 
            exact = F, correct = T) 

vaccine %>% group_by(reversed) %>%
  get_summary_stats(avgLog2pol3, type = "median_iqr")


# -------------------------------
# Binary logistic regression - unadjusted
# ---------------------------------
shbtRegUnadj <- glm(reg_outcome ~ reg_meropenem, # Pr 0.0287
                  family = binomial(link = "logit"),
                  data = shbtSmall2)

shbtRegUnadj <- glm(reg_outcome ~ reg_steroids, # Pr 0.00106
                  family = binomial(link = "logit"),
                  data = shbtSmall2)
# 
# shbtRegUnadj <- glm(reg_outcome ~ reg_mod_anemia, # Pr 0.251
#                     family = binomial(link = "logit"), 
#                     data = shbtSmall2)

# shbtRegUnadj <- glm(reg_outcome ~ reg_sev_pneumonia, # Pr 0.2549
#                     family = binomial(link = "logit"), 
#                     data = shbtSmall2)
# 
# shbtRegUnadj <- glm(reg_outcome ~ reg_sclerema, # Pr 0.178
#                     family = binomial(link = "logit"), 
#                     data = shbtSmall2)

summary(shbtRegUnadj)

# OR & 95% CI
round(exp(cbind(coef(shbtRegUnadj), confint(shbtRegUnadj))), 3)

shkRegUnad <- glm(formula = reg_outcome ~ reg_meropenem, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_steroids, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_mod_anemia, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_hai_hap, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_sev_pneumonia, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_sclerema, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ gap_shk_bt_3h, family = "binomial", data = shock)

summary(shkRegUnad)

# OR & 95% CI
round(exp(cbind(coef(shkRegUnad), confint(shkRegUnad))), 3)


# -------------------------------
#   Binary logistic regression - adjusted
# ---------------------------------
# shbtRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + 
#                     reg_steroids + reg_mod_anemia + 
#                     reg_sev_pneumonia + reg_sclerema, 
#                   family = binomial(link = "logit"), # ?? link = "logit"
#                   data = shbtSmall2)

shkRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + reg_steroids + 
                    reg_mod_anemia + reg_hai_hap + reg_sev_pneumonia + 
                    reg_sclerema + gap_shk_bt_3h, 
                  family = "binomial" (link="logit"), 
                  data = shock)

shkRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + reg_steroids + 
                   reg_mod_anemia + reg_hai_hap + reg_sev_pneumonia + 
                   reg_sclerema + gap_shk_bt_3h, 
                  family = "binomial" (link="logit"), 
                  data = shock)

summary(shkRegAdj)


# OR & 95% CI
round(exp(cbind(coef(shkRegAdj), confint(shkRegAdj))), 3)



# ---------------------------------
#   plotting 
# ---------------------------------
coefplot(shkRegAdj, 
         innerCI = 0, outerCI = 2, lwdInner = 0, lwdOuter = 0.4, 
         intercept = F, 
         zeroColor = "darkgrey", zeroLWD = 0.5, zeroType = 9, 
         title = "", 
         xlab = "Regression coefficient at 95% CI", 
         ylab = "Predictors", 
         decreasing = T, 
         col = "skyblue2", 
         newNames = c(reg_sclerema = "Sclerema", 
                      reg_meropenem = "Fourth-line antibiotics", 
                      reg_steroids = "Corticosteroids", 
                      reg_mod_anemia = "Moderate anemia", 
                      reg_sev_pneumonia = "Severe Pneumonia", 
                      reg_hai_hap = "Hosp. Acquired Infection", 
                      gap_shk_bt_3h = "Shock-BT gap 3hrs")) + 
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  geom_point(pch = 21)
