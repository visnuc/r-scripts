# ---------------------------------
# install packages 
install.packages("plotrix")

# ---------------------------------
# libraries
library("tidyverse")
library("reshape")
library("datasets")
library("foreign")
library("coefplot")
library("ggplot2")
library("plotrix")

# ---------------------------------
# working directory 
setwd("C:/Users/visnu.pritom/Dropbox/tmp_sync/tmp_shbt/poster_presentation/del_tmp")

# ---------------------------------
# data import 
shockBt <- read.spss(file.choose(), header=T)
shockBt <- read.csv(file.choose(), header=T)

attach(shockBt)
names(shockBt)
nrow(shockBt)
View(shockBt)
dim(shockBt)
str(shockBt)

# ---------------------------------
# data export 
write.table(shbt, 
            file = "data_small_shock_20210313.csv", 
            sep = ",", 
            row.names = F)

# subsetting 
shbt <- shockBt %>%
  select(pp_pid, pp_age_total_mnth, pp_sex_bivar, 
         socio_restype_notbuilding_bivar, socio_water_bivar, 
         pedi_imm_bivar, pedi_dev_bivar, pedi_birth_bivar, 
         pedi_feed_bivar, anthro_wt, anthro_ht, 
         hosp_hosp_stay, bt_cause_coded, 
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

shbtSmall2 <- shbt %>%
  select(pp_pid, pp_age_total_mnth, pp_sex_bivar, 
         socio_restype_notbuilding_bivar, socio_water_bivar, 
         pedi_imm_bivar, pedi_dev_bivar, pedi_birth_bivar, 
         pedi_feed_bivar, anthro_wt, anthro_ht, 
         hosp_hosp_stay, bt_cause_coded, 
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
         reg_sclerema, reg_sev_pneumonia, reg_hai_hap) %>%
  mutate(reg_mod_anemia = recode(reg_mod_anemia, yes = 1, no = 0), 
         reg_meropenem = recode(reg_meropenem, yes = 1, no = 0),
         reg_steroids = recode(reg_steroids, yes = 1, no = 0),
         reg_outcome = recode(reg_outcome, "yes (death)" = 1, no = 0), 
         reg_sclerema = recode(reg_sclerema, "yes (death)" = 1, no = 0),
         reg_sev_pneumonia = recode(reg_sev_pneumonia, "yes (death)" = 1, no = 0),
         reg_hai_hap = recode(reg_hai_hap, "yes (death)" = 1, no = 0), 
         gap_shk_bt_3h = recode(gap_shk_bt_3h, "3h or less" = 1, ">3h" = 0))

attach(shbt)
names(shbt)
dim(shbt)
View(shbtSmall2)


# -------------------------------
# Binary logistic regression - unadjusted
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
# Binary logistic regression - adjusted
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
# plotting 
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

