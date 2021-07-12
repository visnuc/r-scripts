# ---------------------------------
#     libraries
# ---------------------------------
# library(arm)
# library(car); 
# library(codebook); 
library(coefplot)
library(datasets); 
# library(devtools); 
library(dplyr)
library(e1071)
library(foreign)
library(ggplot2); 
# library(ggpubr); 
library(ggthemes); library(grid); library(gridExtra)
library(haven); library(hrbrthemes)
library(MASS); library(moments)
library(pastecs); library(plotrix); library(plyr); 
# library(psych)
library(RColorBrewer); library(reshape); 
# library(rstatix)
library(tidyverse)
library(utils)
# library(Rcmdr) # do not load unless needed, messes with exporting plots

# ---------------------------------
#     working directory 
# ---------------------------------
setwd("/home/visnu/MEGA/projects/icddrb/study_retro_shock/manuscript_shock/data")
setwd("C:/Users/visnu.pritom/Dropbox/tmp_sync/tmp_shbt/poster_presentation/del_tmp")

# ---------------------------------
#     list objects in dir 
# ---------------------------------
dir()

# ---------------------------------
#     data import 
# ---------------------------------
shock <- read.csv(file.choose(), header = T)
shock <- read.delim(file.choose(), header = T)
shock <- read.dta(file.choose())
shock <- read.spss(file.choose(), header = T)

# ---------------------------------
#     others 
# ---------------------------------
attach(shock); names(shock); View(shock)
View(shock)
nrow(shock)
dim(shock)

# ---------------------------------
# merging data sets 
s12 <- merge(shock, shockTmp, by = "pp_pid", all.x = T)

# ---------------------------------
#     export data 
# ---------------------------------
write.table(shock, file = "new.csv", sep = ",", row.names = F)

# ---------------------------------
#     subsetting 
# ---------------------------------
shock2 <- shockTmp %>% select(pp_pid, mx_ino) 

# ---------------------------------
#     removing var
# ---------------------------------
shock$socio_restype_notbuilding_bivar <- NULL
shock$socio_water_bivar <- NULL 

shock$bt_cause_coded <- NULL
shock$pp_location <- NULL
shock$bt <- NULL
shock$bt_asso_cause <- NULL
shock$BT_AS0 <- NULL
shock$cc_disori <- NULL
shock$cc_lethargy <- NULL
shock$cc_other_symp_coded <- NULL
shock$cc_other_symp_bivar <- NULL
shock$prev_rx_curr_dx <- NULL
shock$prev_rx_curr_dx_coded <- NULL
shock$PREV_0 <- NULL
shock$cf_rr_coded <- NULL
shock$cf_lung <- NULL
shock$cf_spo2 <- NULL
shock$cf_o2supp_bivar <- NULL
shock$cf_bp_sys <- NULL
shock$cf_bp_dia <- NULL
shock$cf_dh <- NULL
shock$cf_temp_coded <- NULL
shock$cf_bs <- NULL

shock$mx_ampi_bivar <- NULL
shock$mx_genta_bivar <- NULL
shock$mx_ceftri_bivar <- NULL
shock$mx_levoflox_bivar <- NULL
shock$mx_ceftaz_bivar <- NULL
shock$mx_amika_bivar <- NULL
shock$mx_mero_imi_bivar <- NULL
shock$mx_vanco_bivar <- NULL
shock$mx_metro_bivar <- NULL
shock$mx_floclox_bivar <- NULL

shock$gap_shk_bt_8h <- NULL
shock$gap_shk_bt_10h <- NULL
shock$gap_shk_bt_12h <- NULL
shock$gap_shk_bt_14h <- NULL
shock$gap_shk_bt_16h <- NULL
shock$gap_shk_bt_18h <- NULL
shock$gap_shk_bt_20h <- NULL
shock$gap_shk_bt_22h <- NULL
shock$gap_shk_bt_24h <- NULL
shock$gap_shk_bt_30h <- NULL
shock$gap_shk_bt_36h <- NULL
shock$gap_shk_bt_48h <- NULL
shock$gap_shk_bt_60h <- NULL
shock$gap_shk_bt_72h <- NULL

shock$outcome <- NULL
shock$dx_s_shock <- NULL
shock$dx_oth_bivar <- NULL

shock$inv_biochem_na_coded <- NULL
shock$inv_biochem_k_coded <- NULL
shock$inv_biochem_cl_coded <- NULL
shock$inv_biochem_tco2_coded <- NULL
shock$inv_biochem_ag_coded <- NULL
shock$inv_biochem_cr_coded <- NULL
shock$inv_biochem_ca_coded <- NULL
shock$inv_biochem_mg_coded <- NULL
shock$inv_hem_tc_coded <- NULL
shock$inv_hem_neut_coded <- NULL
shock$inv_hem_band_coded <- NULL
shock$inv_hem_lympho_coded <- NULL
shock$inv_hem_mono_coded <- NULL
shock$inv_hem_pc_coded <- NULL
shock$inv_hem_band <- NULL
shock$inv_hem_lympho <- NULL
shock$inv_hem_mono <- NULL
shock$inv_hem_pc <- NULL

# ---------------------------------
#     renaming var
# ---------------------------------
names(shock)[4] <- "socio_restype_bivar"

# ---------------------------------
#     recoding 
# ---------------------------------
shock$pp_sex_bivar <- recode(shock$pp_sex_bivar, "male" = 1, "female" = 0)
shock$socio_restype_bivar <- recode(shock$socio_restype_bivar, "building" = 0, "not building" = 1)
shock$socio_water_bivar <- recode(shock$socio_water_bivar, "safe" = 0, "unsafe" = 1)
shock$socio_safe_water_bivar <- recode(shock$socio_water_bivar, "0" = 1, "1" = 0)

shock$pedi_imm_bivar <- recode(shock$pedi_imm_bivar, "immunized" = 1, "not" = 0)
shock$pedi_dev_bivar <- recode(shock$pedi_dev_bivar, "appropriate" = 1, "delayed" = 0)
shock$pedi_birth_bivar <- recode(shock$pedi_birth_bivar, "LUCS" = 1, "NVD" = 0)
shock$pedi_birth_bivar <- recode(shock$pedi_birth_bivar, "1" = 0, "0" = 1)
shock$pedi_feed_bivar <- recode(shock$pedi_feed_bivar, "EBF" = 1, "not" = 0)

shock$cc_diarr_bivar <- recode(shock$cc_diarr_bivar, "yes" = 1, "no" = 0)
shock$cc_st_cons_bivar <- recode(shock$cc_st_cons_bivar, "watery" = 1, "mucoid" = 0)
shock$cc_vis_bld_bivar <- recode(shock$cc_vis_bld_bivar, "yes" = 1, "no" = 0)
shock$cc_cough_bivar <- recode(shock$cc_cough_bivar,  "yes" = 1, "no" = 0)
shock$cc_resp_dist_bivar <- recode(shock$cc_resp_dist_bivar,  "yes" = 1, "no" = 0)
shock$cc_fever_bivar <- recode(shock$cc_fever_bivar,  "yes" = 1, "no" = 0)

shock$prevrx_currdx_abx_bivar <- recode(shock$prevrx_currdx_abx_bivar, "abx" = 1, "no" = 0)
shock$past_chd_bivar <- recode(shock$past_chd_bivar, "murmur" = 1, "no" = 0)
shock$past_tri21_bivar <- recode(shock$past_tri21_bivar, "yes" = 1, "no" = 0)

shock$cf_rr_fast_br_bivar <- recode(shock$cf_rr_fast_br_bivar, "fast br" = 1, "no" = 0)
shock$cf_lung_crepts_bivar <- recode(shock$cf_lung_crepts_bivar, "crepts" = 1, "no" = 0)
shock$cf_lcwi_bivar <- recode(shock$cf_lcwi_bivar, "yes" = 1, "no" = 0)
shock$cf_hypoxia_bivar <- recode(shock$cf_hypoxia_bivar, "yes" = 1, "no" = 0)
shock$cf_p_char_bivar <- recode(shock$cf_p_char_bivar, "normal" = 0, "low" = 1)
shock$cf_map_bivar <- recode(shock$cf_map_bivar, "maintaining" = 0, "not" = 1)
shock$cf_crt_bivar <- recode(shock$cf_crt_bivar, "normal" = 0, "delayed" = 1)
shock$cf_peri_bivar <- recode(shock$cf_peri_bivar, "warm" = 0, "cold" = 1)
shock$cf_uo_bivar <- recode(shock$cf_uo_bivar, "passed" = 0, "diminished" = 1)
shock$cf_dh_bivar <- recode(shock$cf_dh_bivar, "dh" = 1, "no" = 0)
shock$cf_ede_bivar <- recode(shock$cf_ede_bivar, "yes" = 1, "no" = 0)
shock$cf_hightemp_bivar <- recode(shock$cf_hightemp_bivar, "yes" = 1, "no" = 0)
shock$cf_abd_dist_bivar <- recode(shock$cf_abd_dist_bivar, "normal" = 0, "distended" = 1)
shock$cf_bs_slug_bivar <- recode(shock$cf_bs_slug_bivar, "sluggish" = 1, "not" = 0)
shock$cf_rbs_bivar <- recode(shock$cf_rbs_bivar, "no" = 0, "hypo" = 1)

shock$mx_line_1_bivar <- recode(shock$mx_line_1_bivar, "yes" = 1, "no" = 0)
shock$mx_line_2_bivar <- recode(shock$mx_line_2_bivar, "yes" = 1, "no" = 0)	
shock$mx_line_3_bivar <- recode(shock$mx_line_3_bivar, "yes" = 1, "no" = 0)
shock$mx_line_4_bivar <- recode(shock$mx_line_4_bivar, "yes" = 1, "no" = 0)
shock$mx_ino <- recode(shock$mx_ino, "0" = 0, "one" = 1, "two" = 2, "all three" = 3)
shock$mx_vasopressor <- recode(shock$mx_ino, "0" = 0, "1" = 0, "2" = 1, "3" = 1)
shock$mx_dopamine <- recode(shock$mx_ino, "0" = 0, "1" = 1, "2" = 0, "3" = 0)
shock$mx_adrenaline <- recode(shock$mx_ino, "0" = 0, "1" = 0, "2" = 1, "3" = 0)
shock$mx_noradrenaline <- recode(shock$mx_ino, "0" = 0, "1" = 0, "2" = 0, "3" = 1)
shock$mx_steroid_bivar <- recode(shock$mx_steroid_bivar, "yes" = 1, "no" = 0)

shock$dx_sclere_bivar <- recode(shock$dx_sclere_bivar, "yes" = 1, "no" = 0)
shock$dx_s_pneu_bivar <- recode(shock$dx_s_pneu_bivar, "yes" = 1, "no" = 0)
shock$dx_haip_bivar <- recode(shock$dx_haip_bivar, "yes" = 1, "no" = 0)
shock$dx_conv_bivar <- recode(shock$dx_conv_bivar, "yes" = 1, "no" = 0)
shock$dx_hypogly_bivar <- recode(shock$dx_hypogly_bivar, "yes" = 1, "no" = 0)
shock$dx_hyper_na_bivar <- recode(shock$dx_hyper_na_bivar, "yes" = 1, "no" = 0)
shock$dx_ei_oth_bivar <- recode(shock$dx_ei_oth_bivar, "yes" = 1, "no" = 0)
shock$dx_ileus_bivar <- recode(shock$dx_ileus_bivar, "yes" = 1, "no" = 0)
shock$dx_samuw_bivar <- recode(shock$dx_samuw_bivar, "yes" = 1, "no" = 0)
shock$dx_awd_bivar <- recode(shock$dx_awd_bivar, "yes" = 1, "no" = 0)
shock$dx_id_bivar <- recode(shock$dx_id_bivar, "yes" = 1, "no" = 0)
shock$dx_pd_bivar <- recode(shock$dx_pd_bivar, "yes" = 1, "no" = 0) 
shock$dx_aki_bivar <- recode(shock$dx_aki_bivar, "yes" = 1, "no" = 0)

shock$inv_bl_cs_bivar <- recode(shock$inv_bl_cs_bivar, "1" = 1, "2" = 0)
shock$inv_biochem_hyper_na_bivar <- recode(shock$inv_biochem_hyper_na_bivar, "hyperna" = 1, "not" = 0)
shock$inv_biochem_hypo_na_bivar <- recode(shock$inv_biochem_hypo_na_bivar,  "hypona" = 1, "not" = 0)
shock$inv_biochem_hyper_k_bivar <- recode(shock$inv_biochem_hyper_k_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_hypo_k_bivar <- recode(shock$inv_biochem_hypo_k_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_met_acido_bivar <- recode(shock$inv_biochem_met_acido_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_ag_high_bivar <- recode(shock$inv_biochem_ag_high_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_cr_hi_bivar <- recode(shock$inv_biochem_cr_hi_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_cr_aki_bivar <- recode(shock$inv_biochem_cr_aki_bivar, "AKI" = 1, "no" = 0)
shock$inv_biochem_ca_low_bivar <- recode(shock$inv_biochem_ca_low_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_hi_mg_bivar <- recode(shock$inv_biochem_hi_mg_bivar, "yes" = 1, "no" = 0)
shock$inv_biochem_low_mg_bivar <- recode(shock$inv_biochem_low_mg_bivar, "yes" = 1, "no" = 0)
shock$inv_hem_mod_anemia_9.3 <- recode(shock$inv_hem_mod_anemia_9.3, "mod anemia" = 1, "no" = 0)
shock$inv_hem_leukocytosis_bivar <- recode(shock$inv_hem_leukocytosis_bivar, "yes" = 1, "no" = 0)
shock$inv_hem_neutrophilia_bivar <- recode(shock$inv_hem_neutrophilia_bivar, "yes" = 1, "no" = 0)
shock$inv_hem_neutropenia_bivar <- recode(shock$inv_hem_neutropenia_bivar, "yes" = 1, "no" = 0)
shock$inv_hem_lymphocytosis_bivar <- recode(shock$inv_hem_lymphocytosis_bivar, "yes" = 1, "no" = 0)
shock$inv_hem_lympho_penia_bivar <- recode(shock$inv_hem_lympho_penia_bivar, "yes" = 1, "no" = 0)
shock$inv_hem_thombocytopenia_bivar <- recode(shock$inv_hem_thombocytopenia_bivar, "yes" = 1, "no" = 0)

shock$gap_shk_bt_2h <- recode(shock$gap_shk_bt_2h, "2h or less" = 1, ">2h" = 0)
shock$gap_shk_bt_3h <- recode(shock$gap_shk_bt_3h, "3h or less" = 1, ">3h" = 0)
shock$gap_shk_bt_4h <- recode(shock$gap_shk_bt_4h, "4h or less" = 1, ">4h" = 0)
shock$gap_shk_bt_6h <- recode(shock$gap_shk_bt_6h, "6h or less" = 1, ">6h" = 0)
shock$gap_shk_bt_12h_bi <- recode(shock$gap_shk_bt_12h, "12h or less" = 1, ">12h" = 0)

shock$outcome_bivar <- recode(shock$outcome_bivar, "death" = 1, "survival" = 0)
shock$bt_cause_bivar <- recode(shock$bt_cause_bivar, "shock" = 1, "other" = 0)

shock$reg_mod_anemia <- recode(shock$reg_mod_anemia, "yes" = 1, "no" = 0)
shock$reg_meropenem <- recode(shock$reg_meropenem, "yes" = 1, "no" = 0)
shock$reg_steroids <- recode(shock$reg_steroids, "yes" = 1, "no" = 0)
shock$reg_outcome <- recode(shock$reg_outcome, "yes (death)" = 1, "no" = 0) 
shock$reg_sclerema <- recode(shock$reg_sclerema, "yes (death)" = 1, "no" = 0)
shock$reg_sev_pneumonia <- recode(shock$reg_sev_pneumonia, "yes (death)" = 1, "no" = 0)
shock$reg_hai_hap <- recode(shock$reg_hai_hap, "yes (death)" = 1, "no" = 0)


# ---------------------------------
#     two-way contingency table
# ---------------------------------
continTable <- table(shock$reg_outcome, shock$pp_sex_bivar)
continTable <- table(shock$reg_outcome, shock$socio_safe_water_bivar)
# sum(is.na(shock$socio_water_bivar))
# sNoNA <- na.omit(shock)
continTable <- table(shock$reg_outcome, shock$pedi_imm_bivar)
continTable <- table(shock$reg_outcome, shock$pedi_feed_bivar)

continTable <- table(shock$reg_outcome, shock$cc_diarr_bivar)
continTable <- table(shock$reg_outcome, shock$cf_lung_crepts_bivar)
continTable <- table(shock$reg_outcome, shock$cf_lcwi_bivar)
continTable <- table(shock$reg_outcome, shock$cf_dh_bivar)
continTable <- table(shock$reg_outcome, shock$cf_ede_bivar)
continTable <- table(shock$reg_outcome, shock$cf_abd_dist_bivar)
continTable <- table(shock$reg_outcome, shock$dx_conv_bivar)

continTable <- table(shock$reg_outcome, shock$inv_bl_cs_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_hyper_na_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_hypo_na_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_hyper_k_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_hypo_k_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_met_acido_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_cr_aki_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_ca_low_bivar)
continTable <- table(shock$reg_outcome, shock$inv_biochem_hi_mg_bivar)
continTable <- table(shock$reg_outcome, shock$inv_hem_mod_anemia_9.3)
continTable <- table(shock$reg_outcome, shock$inv_hem_thombocytopenia_bivar)

continTable <- table(shock$reg_outcome, shock$pedi_birth_bivar)
continTable <- table(shock$reg_outcome, shock$cc_cough_bivar)
continTable <- table(shock$reg_outcome, shock$cf_rr_fast_br_bivar)
continTable <- table(shock$reg_outcome, shock$cf_hypoxia_bivar)
continTable <- table(shock$reg_outcome, shock$cf_rbs_bivar)

continTable <- table(shock$reg_outcome, shock$reg_sclerema)
continTable <- table(shock$reg_outcome, shock$reg_sev_pneumonia)
continTable <- table(shock$reg_outcome, shock$dx_ileus_bivar)
continTable <- table(shock$reg_outcome, shock$dx_awd_bivar)
continTable <- table(shock$reg_outcome, shock$dx_id_bivar)

continTable <- table(shock$reg_outcome, shock$mx_line_1_bivar)
continTable <- table(shock$reg_outcome, shock$mx_line_2_bivar)
continTable <- table(shock$reg_outcome, shock$mx_line_3_bivar)
continTable <- table(shock$reg_outcome, shock$mx_vasopressor)
continTable <- table(shock$reg_outcome, shock$mx_dopamine); continTable
continTable <- table(shock$reg_outcome, shock$mx_adrenaline); continTable
continTable <- table(shock$reg_outcome, shock$mx_noradrenaline); continTable
continTable <- table(shock$reg_outcome, shock$reg_meropenem)
continTable <- table(shock$reg_outcome, shock$reg_steroids)
continTable <- table(shock$reg_outcome, shock$gap_shk_bt_3h)
continTable <- table(shock$reg_outcome, shock$gap_shk_bt_4h)
continTable <- table(shock$reg_outcome, shock$gap_shk_bt_6h)
continTable <- table(shock$reg_outcome, shock$gap_shk_bt_12h)
continTable <- table(shock$reg_outcome, shock$gapShkBt12h)

rownames(continTable) <- c("Survival", "Death"); continTable
colnames(continTable) <- c("Female", "Male")
colnames(continTable) <- c("No_org", "Growth")
colnames(continTable) <- c("LUCS", "NVD")
colnames(continTable) <- c("No", "Yes"); continTable

# relative frequencies percentage 
prop.table(continTable)*100
prop.table(continTable, 1)*100 # conditional, row-wise
prop.table(continTable, 2)*100 # conditional, column-wise


# ---------------------------------
#   Chi-square test of independence
# ---------------------------------
chisq.test(continTable)
chisq.test(continTable)$observed
chisq.test(continTable)$expected

# Monte Carlo simulation as expected freq <5
chisq.test(continTable, simulate.p.value = T, B = 10000) 

# ---------------------------------
#     Fisher's Exact test
# ---------------------------------
fisher.test(continTable)


# ---------------------------------
#     normality check
# ---------------------------------
qqnorm(shock$inv_biochem_na, pch = 1, frame = F); qqline(shock$inv_biochem_na, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_na)
qqnorm(shock$inv_biochem_k, pch = 1, frame = F); qqline(shock$inv_biochem_k, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_k) # NN
qqnorm(shock$inv_biochem_cl, pch = 1, frame = F); qqline(shock$inv_biochem_cl, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_cl) # n
qqnorm(shock$inv_biochem_tco2, pch = 1, frame = F); qqline(shock$inv_biochem_tco2, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_tco2)
qqnorm(shock$inv_biochem_cr, pch = 1, frame = F); qqline(shock$inv_biochem_cr, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_cr) # NN
qqnorm(shock$inv_biochem_ca, pch = 1, frame = F); qqline(shock$inv_biochem_ca, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_ca) # n
qqnorm(shock$inv_biochem_mg, pch = 1, frame = F); qqline(shock$inv_biochem_mg, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_biochem_mg) # NN

qqnorm(shock$inv_hem_hb, pch = 1, frame = F); qqline(shock$inv_hem_hb, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_hem_hb) # n
qqnorm(shock$inv_hem_tc, pch = 1, frame = F); qqline(shock$inv_hem_tc, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_hem_tc) 
qqnorm(shock$inv_hem_neut, pch = 1, frame = F); qqline(shock$inv_hem_neut, col = "skyblue4", lwd = 1); shapiro.test(shock$inv_hem_neut) # n

qqnorm(shock$gap_shk_bt, pch = 1, frame = F); qqline(shock$gap_shk_bt, col = "skyblue4", lwd = 1); shapiro.test(shock$gap_shk_bt) # NN

qqnorm(shock$cf_spo2, pch = 1, frame = F); qqline(shock$cf_spo2, col = "skyblue4", lwd = 1); shapiro.test(shock$cf_spo2) # NN
qqnorm(shock$cf_map, pch = 1, frame = F); qqline(shock$cf_map, col = "skyblue4", lwd = 1); shapiro.test(shock$cf_map) # NN
qqnorm(shock$cf_temp_c, pch = 1, frame = F); qqline(shock$cf_temp_c, col = "skyblue4", lwd = 1); shapiro.test(shock$cf_temp) # n
qqnorm(shock$cf_rbs, pch = 1, frame = F); qqline(shock$cf_rbs, col = "skyblue4", lwd = 1); shapiro.test(shock$cf_rbs) # NN, if p<0.05 not normal

qqnorm(shock$anthro_wt, pch = 1, frame = F); qqline(shock$anthro_wt, col = "skyblue4", lwd = 1); shapiro.test(shock$anthro_wt)

qqnorm(shock$hosp_hosp_stay, pch = 1, frame = F); qqline(shock$hosp_hosp_stay, col = "skyblue4", lwd = 1); shapiro.test(shock$hosp_hosp_stay) # NN


# ---------------------------------------
# Unpaired/Independent sample t-test 
# ---------------------------------------
#   3. df = [(n1 - 1) + (n2 - 1)]
t.test(shock$anthro_wt ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$cf_rr ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$cf_spo2 ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$inv_biochem_na ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$inv_biochem_cl ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$inv_biochem_tco2 ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$inv_biochem_ca ~ shock$reg_outcome, data=shock, var.equal = T) # p=0.07668
t.test(shock$inv_hem_hb ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$inv_hem_tc ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$inv_hem_neut ~ shock$reg_outcome, data=shock, var.equal = T)
t.test(shock$cf_temp_c ~ shock$reg_outcome, data=shock, var.equal = T)
# var.equal T or F, which one when  

shock %>% group_by(reg_outcome) %>% get_summary_stats(anthro_wt, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(cf_rr, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(cf_spo2, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_biochem_na, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_biochem_tco2, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_biochem_ca, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_hem_hb, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_hem_tc, type = "mean_sd")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_hem_neut, type = "mean_sd")


# -------------------------------
#   Wilcoxon test
# -------------------------------
# vaccine$reversed <- as.factor(vaccine$reversed)
# vaccine$reversed <- revalue(vaccine$reversed, c("0"="Not-reversed", "1"="Reversed"))
boxplot(shock$pp_age_total_mnth ~ shock$pp_age_total_mnth)

# two sided test 
# exact = T, may produce errors if there are ties in ranks of observations 
wilcox.test(shock$cf_rbs ~ shock$reg_outcome, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = F, correct = T) 
wilcox.test(shock$inv_biochem_k ~ shock$reg_outcome, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = F, correct = T) 
wilcox.test(shock$inv_biochem_cr ~ shock$reg_outcome, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = F, correct = T)
wilcox.test(shock$inv_biochem_mg ~ shock$reg_outcome, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = F, correct = T)
wilcox.test(shock$gap_shk_bt ~ shock$reg_outcome, mu = 0, alt = "two.sided", conf.int = T, conf.level = 0.95, paired = F, exact = F, correct = T)

shock %>% group_by(reg_outcome) %>% get_summary_stats(cf_rbs, type = "median_iqr")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_biochem_k, type = "median_iqr")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_biochem_cr, type = "median_iqr")
shock %>% group_by(reg_outcome) %>% get_summary_stats(inv_biochem_mg, type = "median_iqr")
shock %>% group_by(reg_outcome) %>% get_summary_stats(gap_shk_bt, type = "median_iqr")


# ---------------------------------
#   binary logistic regression - unadjusted
# ---------------------------------
shkRegUnad <- glm(formula = reg_outcome ~ reg_meropenem, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_steroids, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ mx_dopamine, family = "binomial", data = shock); summary(shkRegUnad)
shkRegUnad <- glm(formula = reg_outcome ~ mx_vasopressor, family = "binomial", data = shock); summary(shkRegUnad)
shkRegUnad <- glm(formula = reg_outcome ~ reg_mod_anemia, family = "binomial" (link="logit"), data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_hai_hap, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_sev_pneumonia, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ reg_sclerema, family = "binomial", data = shock)
shkRegUnad <- glm(formula = reg_outcome ~ gap_shk_bt_3h, family = "binomial", data = shock); summary(shkRegUnad)

# OR & 95% CI
round(exp(cbind(coef(shkRegUnad), confint(shkRegUnad))), 3)


# ---------------------------------
#   Binary logistic regression - adjusted
# ---------------------------------
# shbtRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + 
#                     reg_steroids + reg_mod_anemia + 
#                     reg_sev_pneumonia + reg_sclerema, 
#                   family = binomial(link = "logit"), # ?? link = "logit"
#                   data = shbtSmall2)

# shkRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + reg_steroids + 
#                    reg_mod_anemia + reg_hai_hap + reg_sev_pneumonia + 
#                    reg_sclerema + gap_shk_bt_3h, 
#                   family = "binomial" (link="logit"), 
#                   data = shock)

shkRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + mx_dopamine + reg_steroids + gap_shk_bt_3h
                 + reg_hai_hap + reg_sev_pneumonia + reg_sclerema
                 + inv_hem_hb + inv_hem_neut 
                 + inv_biochem_na + inv_biochem_k + inv_biochem_ca + inv_biochem_mg + inv_biochem_cr
                 + anthro_wt + pp_age_total_mnth, 
                 family = "binomial" (link="logit"), 
                 data = shock); summary(shkRegAdj)

# shkRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + mx_dopamine + reg_steroids + inv_biochem_ca,
#                  family = "binomial" (link="logit"), data = shock); summary(shkRegAdj)


# OR & 95% CI
round(exp(cbind(coef(shkRegAdj), confint(shkRegAdj))), 3)


# ---------------------------------
#   regression - logistic - binomial
# ---------------------------------
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_na, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_k, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_cl, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_tco2, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_cr, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_ca, data = shock, family = binomial); summary(glmFit) # p<0.1
glmFit <- glm(shock$reg_outcome ~ shock$inv_biochem_mg, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_hem_hb, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_hem_tc, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$inv_hem_neut, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$gap_shk_bt, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$gap_shk_bt_4h, data = shock, family = binomial); summary(glmFit) 
glmFit <- glm(shock$reg_outcome ~ shock$cf_spo2, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$anthro_wt, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$hosp_hosp_stay, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$cf_map, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$cf_temp_c, data = shock, family = binomial); summary(glmFit)
glmFit <- glm(shock$reg_outcome ~ shock$cf_rbs, data = shock, family = binomial); summary(glmFit)


# ---------------------------------
#     plotting - coefficients
# ---------------------------------
coefplot(shkRegAdj, 
         innerCI = 1, outerCI = 2, lwdInner = 1.3, lwdOuter = 0.4, 
         intercept = F, 
         zeroColor = "darkgrey", zeroLWD = 0.7, zeroType = 3, 
         title = "", 
         xlab = "Regression coefficients", 
         ylab = "Predictors", 
         decreasing = T, 
         col = "skyblue2") + 
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  geom_point(pch = 21)

coefplot(shkRegAdj,
         innerCI = 1, outerCI = 2, lwdInner = 1.3, lwdOuter = 0.4,
         intercept = F,
         zeroColor = "grey2", zeroLWD = 0.7, zeroType = 3,
         title = "",
         xlab = "Regression coefficients",
         ylab = "Predictors",
         decreasing = T,
         col = "skyblue2",
         newNames = c(reg_sclerema = "Sclerema",
                      reg_meropenem = "Fourth-line antibiotics",
                      reg_steroids = "Corticosteroids",
                      mx_dopamine = "Dopamine",
                      mx_vasopressor = "Vasopressors",
                      reg_mod_anemia = "Moderate anemia",
                      reg_sev_pneumonia = "Severe Pneumonia",
                      reg_hai_hap = "Hosp. acquired infection",
                      gap_shk_bt_3h = "Early BT (in 3hrs)",
                      inv_biochem_ca = "Serum Calcium (mmol/L)",
                      inv_hem_neut = "Neutrophil (%)",
                      inv_hem_hb = "Hemoglobin (gm/dL)",
                      anthro_wt = "Weight (in kg)",
                      pp_age_total_mnth = "Age (in months)",
                      inv_biochem_k = "Serum Potassium (mmol/L)",
                      inv_biochem_na = "Serum Sodium (mmol/L)",
                      inv_biochem_cr = "Serum Creatinine (μmol/L)",
                      inv_biochem_mg = "Serum Magnesium (mmol/L)")) +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_point(pch = 21)

# ---------------------------------
#   plotting logistic regression model
# ---------------------------------
plot(reg_outcome ~ inv_biochem_ca, 
     data = shock, col="red4", 
     xlab = "Serum Calcium (mmol/L)", ylab = "Probability of Death") # plot(x, y)
abline(h =.5, lty = 3, col="red4"); abline(h = 0, lty = 3, col="red4"); abline(h = 1, lty = 3, col="red4")
model <- glm(formula = reg_outcome ~ inv_biochem_ca, 
             family = "binomial"  (link="logit"), data = shock); summary(model)
curve(predict(model, data.frame(inv_biochem_ca = x), 
              type = "response"), add = T) # curve based on prediction from model
coefficients(model)
abline(v = -coef(model)[1] / coef(model)[2], lty = 3, col="red4")
v = -coef(model)[1] / coef(model)[2]; v


# # testing
# fit = glm(vs ~ hp, data=mtcars, family=binomial)
# newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
# newdat$vs = predict(fit, newdata=newdat, type="response")
# plot(vs~hp, data=mtcars, col="red4")
# lines(vs ~ hp, newdat, col="green4", lwd=2)
