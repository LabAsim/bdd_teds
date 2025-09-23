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
fit_fiml_scaled_32_without_covid_without_ED <- sem(
  model = model_scaled_32_without_covid,
  data = df_essential_vars_without_ED,
  cluster = "fam_id",
  missing = "fiml"
)
summary(fit_fiml_scaled_32_without_covid_without_ED, standardized = T)

resid(fit_fiml_scaled_32_without_covid_without_ED, type = "cor.bollen")
modindices(fit_fiml_scaled_32_without_covid_without_ED, sort. = T)



model_scaled_32_without_covid_modified <- "
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

    # Added
    mpvs_total_12_1_scaled_32 ~~ mpvs_total_16_1_scaled_32
    mpvs_total_child_14_1_scaled_32 ~~ mpvs_total_16_1_scaled_32
    mpvs_total_16_1_scaled_32   ~~ mpvs_total_phase_2_21_1_scaled_32
"
fit_fiml_scaled_32_without_covid_without_ED <- sem(
  model = model_scaled_32_without_covid_modified,
  data = df_essential_vars_without_ED,
  cluster = "fam_id",
  missing = "fiml"
)
summary(fit_fiml_scaled_32_without_covid_without_ED, standardized = T)

resid(fit_fiml_scaled_32_without_covid_without_ED, type = "cor.bollen")
modindices(fit_fiml_scaled_32_without_covid_without_ED, sort. = T)





labels <- list(
  dcq_total_26_1 = "DCQ (26y)",
  mpvs_total_12_1_scaled_32 = "MPVS (12y)",
  mpvs_total_child_14_1_scaled_32 = "MPVS (14y)",
  mpvs_total_16_1_scaled_32 = "MPVS (16y)",
  mpvs_total_phase_2_21_1_scaled_32 = "MPVS (21y)",
  sex_1_fct = "Sex",
  age_child_12_1 = "Age (12y)",
  age_child_14_1 = "Age (14y)",
  age_child_web_16_1 = "Age (16y)",
  age_phase2_child_21_1 = "Age (21y)",
  age_26_1 = "Age (26y)"
)
plot_fit_fiml_scaled_32_without_covid_without_ED <- lavaanPlot::lavaanPlot(
  model = fit_fiml_scaled_32_without_covid_without_ED,
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
  conf.int = T,
  edge_styles = T
)
plot_fit_fiml_scaled_32_without_covid_without_ED

plot_fit_fiml_scaled_32_without_covid_without_ED_standardized <- lavaanPlot::lavaanPlot(
  model = fit_fiml_scaled_32_without_covid_without_ED,
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
  conf.int = T,
  edge_styles = T
)
plot_fit_fiml_scaled_32_without_covid_without_ED_standardized
