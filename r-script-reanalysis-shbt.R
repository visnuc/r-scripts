# ---------------------------------
# libraries
library("tidyverse")
library("reshape")
library("datasets")
library("foreign")

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

attach(shbt)
names(shbt)
dim(shbt)
