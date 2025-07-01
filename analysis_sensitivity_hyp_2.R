library(lavaan)
source("constants.R")
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


df_all_diffs_without_eating <- create_df_subtract_mz_twins_values_decorated(
  df = df_essential_vars_without_ED,
  group_var = "fam_id",
  vars = c(
    "mpvs_total_12_1",
    "mpvs_total_child_14_1",
    "mpvs_total_16_1",
    "mpvs_total_phase_2_21_1",
    "mpvs_total_cov1_21_1",
    "mpvs_total_cov2_21_1",
    "mpvs_total_cov3_21_1",
    "mpvs_total_cov4_21_1",
    "dcq_total_26_1",
    colnames(df_essential_vars)[grepl(pattern = "scaled", x = colnames(df_essential_vars))]
  )
)
# Excluding COVID period #
twin_diff_model_scaled2 <- "
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
"

fit_ml_diff_without_covid_ΜΖ_sensitivity <- sem(
  model = twin_diff_model_scaled2, data = df_all_diffs_without_eating
)
summary(fit_ml_diff_without_covid_ΜΖ_sensitivity)


fit_fiml_diff_without_covid_ΜΖ_sensitivity <- sem(
  model = twin_diff_model_scaled2, data = df_all_diffs_without_eating,
  missing = "fiml"
)

summary(fit_fiml_diff_without_covid_ΜΖ_sensitivity, standardized = T)

parameters_fit_fiml_without_covid_ΜΖ_sensitivity <- modify_parameter_estimates(
  df = parameterestimates(
    fit_fiml_diff_without_covid_ΜΖ_sensitivity,
    standardized = T
  ),
  round_digits = 3
)
labels <- list(
  dcq_total_26_1 = "DCQ (26y)",
  mpvs_total_12_1_scaled_32 = "MPVS (12y)",
  mpvs_total_child_14_1_scaled_32 = "MPVS (14y)",
  mpvs_total_16_1_scaled_32 = "MPVS (16y)",
  mpvs_total_phase_2_21_1_scaled_32 = "MPVS (21y)"
)

plot_fit_fiml_diff_without_covid_ΜΖ_sensitivity <- lavaanPlot::lavaanPlot(
  model = fit_fiml_diff_without_covid_ΜΖ_sensitivity,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(
    rankdir = "TB", fontsize = "15",
    overlap = "true",
    labelloc = "b", label = footnote
  ),
  stars = c("regress", "latent", "covs"),
  labels = labels,
  stand = F,
  conf.int = T
)
plot_fit_fiml_diff_without_covid_ΜΖ_sensitivity

plot_fit_fiml_diff_without_covid_standardized_ΜΖ_sensitivity <- lavaanPlot::lavaanPlot(
  model = fit_fiml_diff_without_covid_ΜΖ_sensitivity,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(
    rankdir = "TB", fontsize = "15",
    overlap = "true",
    labelloc = "b", label = footnote
  ),
  stars = c("regress", "latent", "covs"),
  labels = labels,
  stand = T,
  conf.int = T
)
plot_fit_fiml_diff_without_covid_standardized_ΜΖ_sensitivity


#######################
# Latent growth model #
#######################

latent_growth_model <- "
  # intercept and slope with fixed coefficients
  intercept =~ 1*mpvs_total_12_1_scaled_32 + 1*mpvs_total_child_14_1_scaled_32 + 1*mpvs_total_16_1_scaled_32 + 1*mpvs_total_phase_2_21_1_scaled_32
  slope =~ 0*mpvs_total_12_1_scaled_32 + 1*mpvs_total_child_14_1_scaled_32 + 2*mpvs_total_16_1_scaled_32 + 3*mpvs_total_phase_2_21_1_scaled_32

  # Regressions
  dcq_total_26_1 ~ intercept
  dcq_total_26_1 ~ slope

  intercept ~ slope
"
fit <- growth(
  latent_growth_model,
  data = df_all_diffs,
  missing = "fiml"
)
summary(fit, standardized = T)


fit_without_ED <- growth(
  latent_growth_model,
  data = df_all_diffs_without_eating,
  missing = "fiml"
)
summary(fit_without_ED, standardized = T)

lavaanPlot::lavaanPlot(
  model = fit_without_ED,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(rankdir = "TB")
  # stars = c("regress", "latent", "covs")
)
