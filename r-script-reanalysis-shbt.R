# ---------------------------------
#     get packages 
install.packages("dplyr")

# ---------------------------------
#     libraries
library("plyr")
library("tidyverse")
library("reshape")
library("datasets")
library("foreign")
library("coefplot")
library("ggplot2")
library("plotrix")
library("dplyr")
library("utils")


# ---------------------------------
#     working directory 
setwd("~/Dropbox/tmp_sync/tmp_shbt/poster_presentation/del_tmp")
setwd("C:/Users/visnu.pritom/Dropbox/tmp_sync/tmp_shbt/poster_presentation/del_tmp")


# ---------------------------------
#     list objects in dir 
dir()

# ---------------------------------
#     data import 
shock <- read.spss(file.choose(), header=T)
shock <- read.csv(file.choose(), header=T)


# ---------------------------------
#     others 
attach(shock)
names(shock)
nrow(shock)
View(shock)
dim(shock)
str(shbtSmall2$pp_sex_bivar)


# ---------------------------------
#     data export 
write.table(shock, 
            file = "data_shock_20210317.csv", 
            sep = ",", 
            row.names = F)


# ---------------------------------
#     subsetting 
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
shbtSmall2$socio_restype_notbuilding_bivar <- NULL
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
shock$mx_ampi_bivar	<- NULL
shock$mx_genta_bivar <- NULL
shock$mx_ceftri_bivar	<- NULL
shock$mx_levoflox_bivar	<- NULL
shock$mx_ceftaz_bivar	<- NULL
shock$mx_amika_bivar <- NULL
shock$mx_mero_imi_bivar	<- NULL
shock$mx_vanco_bivar <- NULL
shock$mx_metro_bivar <- NULL
shock$mx_floclox_bivar <- NULL
shock$gap_shk_bt_8h	<- NULL
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
#     renaming col/var
names(shock)[4] <- "socio_restype_bivar"


# ---------------------------------
#     recoding 
shock$pp_sex_bivar <- recode(shock$pp_sex_bivar, "male" = 1, "female" = 0)
shock$socio_restype_bivar <- recode(shock$socio_restype_bivar, "building" = 0, "not building" = 1)
shock$socio_water_bivar <- recode(shock$socio_water_bivar, "safe" = 0, "unsafe" = 1)

shock$pedi_imm_bivar <- recode(shock$pedi_imm_bivar, "immunized" = 1, "not" = 0)
shock$pedi_dev_bivar <- recode(shock$pedi_dev_bivar, "appropriate" = 1, "delayed" = 0)
shock$pedi_birth_bivar <- recode(shock$pedi_birth_bivar, "LUCS" = 1, "NVD" = 0)
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

shock$mx_line_1_bivar	<- recode(shock$mx_line_1_bivar, "yes" = 1, "no" = 0)
shock$mx_line_2_bivar <- recode(shock$mx_line_2_bivar, "yes" = 1, "no" = 0)	
shock$mx_line_3_bivar <- recode(shock$mx_line_3_bivar, "yes" = 1, "no" = 0)
shock$mx_line_4_bivar <- recode(shock$mx_line_4_bivar, "yes" = 1, "no" = 0)
shock$mx_ino <- recode(shock$mx_ino, "0" = 0, "one" = 1, "two" = 2, "all three" = 3)
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
continTable <- table(shock$outcome_bivar, shock$pp_sex_bivar)

continTable <- table(shock$outcome_bivar, shock$mx_line_1_bivar)
continTable <- table(shock$outcome_bivar, shock$mx_line_2_bivar)
continTable <- table(shock$outcome_bivar, shock$mx_line_3_bivar)
continTable <- table(shock$outcome_bivar, shock$reg_meropenem)
continTable <- table(shock$outcome_bivar, shock$reg_steroids)
continTable <- table(shock$outcome_bivar, shock$gap_shk_bt_3h)

colnames(continTable) <- c("Female", "Male")

colnames(continTable) <- c("No", "Yes")
rownames(continTable) <- c("Survival", "Death")

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
#     Chi-square of independence
chisq.test(continTable)
chisq.test(continTable)$observed
chisq.test(continTable)$expected

# Monte Carlo simulation as expected freq <5
chisq.test(continTable, simulate.p.value = T, B = 10000) 


# -------------------------------
#     Fisher's Exact test
contin_table
fisher.test(continTable)


# -------------------------------
#     Binary logistic regression - unadjusted
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


# -------------------------------
#     Binary logistic regression - adjusted
# shbtRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + 
#                     reg_steroids + reg_mod_anemia + 
#                     reg_sev_pneumonia + reg_sclerema, 
#                   family = binomial(link = "logit"), # ?? link = "logit"
#                   data = shbtSmall2)

shbtRegAdj <- glm(formula = reg_outcome ~ reg_meropenem + 
                    reg_steroids + reg_mod_anemia + reg_hai_hap + 
                    reg_sev_pneumonia + reg_sclerema + 
                    gap_shk_bt_3h, 
                  family = "binomial", 
                  data = shbtSmall2)

summary(shbtRegAdj)

# OR & 95% CI
round(exp(cbind(coef(shbtRegAdj), confint(shbtRegAdj))), 3)


# -------------------------------
#     plotting 
coefplot(shbtRegAdj, innerCI = 2, outerCI = 0, intercept = F, 
         title = "", 
         xlab = "95% Confidence Interval", 
         ylab = "Predictors", 
         decreasing = T, 
         col = "skyblue2", 
         newNames = c(reg_sclerema = "Sclerema", 
                      reg_meropenem = "Fourth-line antibiotics", 
                      reg_steroids = "Corticosteroids", 
                      reg_mod_anemia = "Moderate anemia", 
                      reg_sev_pneumonia = "Severe Pneumonia", 
                      reg_hai_hap = "Hospital Acquired Infection", 
                      gap_shk_bt_3h = "Gap between Shock & BT 3h")) + 
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  geom_point(pch = 21)

