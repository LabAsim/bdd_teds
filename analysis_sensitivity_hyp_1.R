library(lavaan)

df_essential_vars_without_ED <- df_essential_vars |>
  filter(
    eating_diagnosis_fct_26_1 == "No"
  )

df_essential_vars_without_ED <- remove_twins_with_this_level(
  df = df_essential_vars,
  group_var = "fam_id",
  sex_var = "sex_1",
  target_var = "eating_diagnosis_26_1",
  level = "No",
  keep_different_cotwin = F
)

model_scaled_32_without_covid <- "
    # DCQ
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32

    # Mpvs
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32

    # Age
    dcq_total_26_1 ~ age_26_1
    mpvs_total_12_1_scaled_32 ~ age_child_12_1
    mpvs_total_child_14_1_scaled_32 ~ age_child_14_1
    mpvs_total_16_1_scaled_32 ~ age_child_web_16_1
    mpvs_total_phase_2_21_1_scaled_32 ~ age_phase2_child_21_1

    # Sex
    dcq_total_26_1 ~ sex_1_fct
    mpvs_total_12_1_scaled_32 ~ sex_1_fct
    mpvs_total_child_14_1_scaled_32 ~ sex_1_fct
    mpvs_total_16_1_scaled_32 ~ sex_1_fct
    mpvs_total_phase_2_21_1_scaled_32 ~ sex_1_fct
"
fit_fiml_scaled_32_without_covid <- sem(
  model = model_scaled_32_without_covid,
  data = df_essential_vars_without_ED,
  cluster = "fam_id",
  missing = "fiml"
)
summary(fit_fiml_scaled_32_without_covid, standardized = T)
lavaanPlot::lavaanPlot(
  model = fit_fiml_scaled_32_without_covid,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(rankdir = "TB", fontsize = "15"),
  stars = c("regress", "latent", "covs"),
  stand = T
)
