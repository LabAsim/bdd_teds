library(umx)
library(lavaan)

if (sys.nframe() == 0) {
  source("data_management_hyp_1.R")
}
################
# Hypothesis 1 #
################
# To test hypothesis 1,
# we will use Structural Equation Modeling (SEM)
# in order to determine the longitudinal phenotypic association
# of earlier peer victimization (MPVS score) with later BDD symptoms (DCQ score).
# Potential confounders, namely age and sex


# Without covid
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
  data = df_essential_vars,
  cluster = "fam_id",
  missing = "fiml"
)
summary(fit_fiml_scaled_32_without_covid, standardized = T)

fit_plot <- lavaanPlot::lavaanPlot(
  model = fit_fiml_scaled_32_without_covid,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(rankdir = "TB", fontsize = "15"),
  stars = c("regress", "latent", "covs"),
  stand = T
)
fit_plot

# The rows in the analysis above are the following;
# test <- df_essential_vars %>%
#   select(
#     c(
#       "twin_id",
#       "fam_id",
#       "sex_1",
#       "age_child_12_1",
#       "age_child_14_1",
#       "age_child_web_16_1",
#       "age_phase2_child_21_1",
#       "age_26_1",
#       "mpvs_total_12_1_scaled_32",
#       "mpvs_total_child_14_1_scaled_32",
#       "mpvs_total_16_1_scaled_32",
#       "mpvs_total_phase_2_21_1_scaled_32",
#       "dcq_total_26_1"
#     )
#   )
# dim(test[complete.cases(test[, c(
#   "sex_1",
#   "age_child_12_1",
#   "age_child_14_1",
#   "age_child_web_16_1",
#   "age_phase2_child_21_1",
#   "age_26_1"
# )]), ])

fit_ml_scaled_32_without_covid <- sem(
  model = model_scaled_32_without_covid,
  data = df_essential_vars,
  cluster = "fam_id"
)
summary(fit_ml_scaled_32_without_covid)

# The rows in the complete case analysis above are the following;
# dim(test[complete.cases(test), ])

# With covid
model_scaled_32_with_covid <- "
    # DCQ
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32

    # Mpvs
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
    mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
    mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
    mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32

    # Age
    dcq_total_26_1 ~ age_26_1
    mpvs_total_12_1_scaled_32 ~ age_child_12_1
    mpvs_total_child_14_1_scaled_32 ~ age_child_14_1
    mpvs_total_16_1_scaled_32 ~ age_child_web_16_1
    mpvs_total_phase_2_21_1_scaled_32 ~ age_phase2_child_21_1
    mpvs_total_cov2_21_1_scaled_32 ~ age_cov1_child_21_1
    mpvs_total_cov3_21_1_scaled_32 ~ age_cov2_child_21_1
    mpvs_total_cov3_21_1_scaled_32 ~ age_cov3_child_21_1
    mpvs_total_cov4_21_1_scaled_32 ~ age_cov4_child_21_1

    # Sex
    dcq_total_26_1 ~ sex_1
    mpvs_total_12_1_scaled_32 ~ sex_1
    mpvs_total_child_14_1_scaled_32 ~ sex_1
    mpvs_total_16_1_scaled_32 ~ sex_1
    mpvs_total_phase_2_21_1_scaled_32 ~ sex_1
    mpvs_total_cov2_21_1_scaled_32 ~ sex_1
    mpvs_total_cov3_21_1_scaled_32 ~ sex_1
    mpvs_total_cov3_21_1_scaled_32 ~ sex_1
    mpvs_total_cov4_21_1_scaled_32 ~ sex_1
"
fit_fiml_scaled_32_with_covid <- sem(
  model = model_scaled_32_with_covid,
  data = df_essential_vars,
  cluster = "fam_id",
  missing = "fiml"
)
summary(fit_fiml_scaled_32_with_covid)
beepr::beep("mario")

############
# Using MI #
############

if (sys.nframe() == 0) {
  if (exists("imp_derived") == F) {
    if (file.exists("G:\\imp_derived.Rdata")) {
      load("G:\\imp_derived.Rdata")
    } else {
      source("imputation_derived.R")
    }
  }

  # Do not use semTools, it's deprecated
  library(lavaan.mi)

  fit_mi <- sem.mi(model = model_scaled_32_without_covid, data = imp_data_derived)
  summary(fit_mi)
  parameterEstimates.mi(fit_mi)
}
