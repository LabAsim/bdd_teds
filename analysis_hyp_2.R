if (sys.nframe() == 0) {
  source("data_management_hyp_2.R")
}
library(umx)
library(lavaan)
library(lavaanPlot)
library(lavaan.mi)

#######################
# Using original data #
#######################

# fiml
twin_diff_model_with_covid_scaled <- "
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
    mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
    mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
    mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32
"

fit_fiml_diff_model_with_covid_scaled <- sem(
  model = twin_diff_model_with_covid_scaled, data = df_all_diffs,
  missing = "fiml"
)
summary(fit_fiml_diff_model_with_covid_scaled)


lavaanPlot::lavaanPlot(
  model = fit_fiml_diff_model_with_covid_scaled,
  edge_options = list(color = "grey"),
  coefs = TRUE, covs = TRUE,
  graph_options = list(rankdir = "LR")
)

# ml
twin_diff_model_scaled <- "
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
    mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
    mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
    mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32
"

fit_ml <- sem(
  model = twin_diff_model_scaled, data = df_all_diffs
)
summary(fit_ml)

# Excluding COVID period #
twin_diff_model_scaled_without_covid <- "
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
"

fit_ml_without_covid <- sem(
  model = twin_diff_model_scaled_without_covid, data = df_all_diffs
)
summary(fit_ml_without_covid)


fit_fiml_without_covid <- sem(
  model = twin_diff_model_scaled_without_covid, data = df_all_diffs,
  missing = "fiml"
)
summary(fit_fiml_without_covid, standardized = T)
parameterestimates(fit_fiml_without_covid)

plot_fit_fiml <- lavaanPlot::lavaanPlot(
  model = fit_fiml_without_covid,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(rankdir = "TB"),
  stars = c("regress", "latent", "covs"),
  stand = F
)
plot_fit_fiml

plot_fit_fiml_standardized <- lavaanPlot::lavaanPlot(
  model = fit_fiml_without_covid,
  edge_options = list(color = "grey"),
  coefs = TRUE, # covs = TRUE,
  graph_options = list(rankdir = "TB"),
  stars = c("regress", "latent", "covs"),
  stand = T
)
plot_fit_fiml_standardized
######################
# Using imputed data #
######################
if (exists("imp_derived") == F) {
  if (file.exists("G:\\imp_derived.Rdata")) {
    load("G:\\imp_derived.Rdata")
  } else {
    source("imputation_derived.R")
  }
}
if (sys.nframe() == 0) {
  if (exists("imp_derived") == F) {
    if (file.exists("G:\\imp_derived.Rdata")) {
      load("G:\\imp_derived.Rdata")
    } else {
      source("imputation_derived.R")
    }
  }

  data_imp_original <- imp_data_derived

  data_imp_original <- lapply(
    X = data_imp_original,
    FUN = function(x) {
      scale_mpvs(
        df = x,
        scale_size = 32,
        from_vars = c(
          "mpvs_total_12_1",
          "mpvs_total_parent_14_1",
          "mpvs_total_child_14_1",
          "mpvs_total_teacher_14_1",
          "mpvs_total_16_1",
          "mpvs_total_phase_2_21_1",
          "mpvs_total_cov1_21_1",
          "mpvs_total_cov2_21_1",
          "mpvs_total_cov3_21_1",
          "mpvs_total_cov4_21_1"
        )
      )
    }
  )
}
##############################
# Create a df of differences #
##############################
if (sys.nframe() == 0) {
  library(parallel)
  start_time <- Sys.time()
  # This needs to be in global environment
  cl <- makeCluster(detectCores() - 2)
  # Don't forget to call stopCluster() after calling this custom function
  # objs <- ls()
  clusterExport(
    cl = cl,
    varlist = list(
      "create_df_subtract_mz_twins_values",
      "subtract_mz_twins_values",
      "tibble",
      "left_join_multiple_df_diff_twin_values",
      "left_join_df_diff_twin_values",
      "cl",
      "left_join",
      "join_by"
    )
  )

  data_imp_diff <- parLapplyLB(
    cl = cl,
    X = data_imp_original,
    fun = function(x) {
      create_df_subtract_mz_twins_values(
        df = x,
        group_var = "fam_id",
        vars = c(
          # "mpvs_total_12_1",
          # "mpvs_total_parent_14_1",
          # "mpvs_total_child_14_1",
          # "mpvs_total_teacher_14_1",
          # "mpvs_total_16_1",
          # "mpvs_total_phase_2_21_1",
          # "mpvs_total_cov1_21_1",
          # "mpvs_total_cov2_21_1",
          # "mpvs_total_cov3_21_1",
          # "mpvs_total_cov4_21_1",
          "dcq_total_26_1",
          colnames(x)[grepl(pattern = "scaled", x = colnames(x))]
        )
      )
    }
  )
  stopCluster(cl)
  print(Sys.time() - start_time)
  beepr::beep()
  #
  # data_imp_diff <- lapply(
  #   X=data_imp[1],
  #   FUN = function(x){
  #     # print(colnames(x)[grepl(pattern="scaled",x=x)])
  #     create_df_subtract_twins_values_multiple_vars(
  #       df=x,
  #       group_var="fam_id",
  #       vars = c(
  #         "mpvs_total_12_1",
  #         "mpvs_total_parent_14_1",
  #         "mpvs_total_child_14_1",
  #         "mpvs_total_teacher_14_1",
  #         "mpvs_total_16_1",
  #         "mpvs_total_phase_2_21_1",
  #         "mpvs_total_cov1_21_1",
  #         "mpvs_total_cov2_21_1",
  #         "mpvs_total_cov3_21_1",
  #         "mpvs_total_cov4_21_1",
  #         "dcq_total_26_1",
  #         colnames(x)[grepl(pattern="scaled",x=colnames(x))]
  #       )
  #     )
  #   }
  # )
}
if (sys.nframe() == 0) {
  twin_diff_model <- "
      dcq_total_26_1 ~ mpvs_total_12_1
      dcq_total_26_1 ~ mpvs_total_child_14_1
      dcq_total_26_1 ~ mpvs_total_16_1
      dcq_total_26_1 ~ mpvs_total_phase_2_21_1
      dcq_total_26_1 ~ mpvs_total_cov1_21_1
      dcq_total_26_1 ~ mpvs_total_cov2_21_1
      dcq_total_26_1 ~ mpvs_total_cov3_21_1
      dcq_total_26_1 ~ mpvs_total_cov4_21_1
      mpvs_total_child_14_1 ~ mpvs_total_12_1
      mpvs_total_16_1 ~ mpvs_total_child_14_1
      mpvs_total_phase_2_21_1 ~ mpvs_total_16_1
      mpvs_total_cov1_21_1 ~ mpvs_total_phase_2_21_1
      mpvs_total_cov2_21_1 ~ mpvs_total_cov1_21_1
      mpvs_total_cov3_21_1 ~ mpvs_total_cov2_21_1
      mpvs_total_cov4_21_1 ~ mpvs_total_cov3_21_1
  "
  # Do not use semTools, it's deprecated
  library(lavaan.mi)

  fit_mi <- sem.mi(model = twin_diff_model, data = data_imp_diff)
  summary(fit_mi)
  parameterEstimates.mi(fit_mi)


  twin_diff_model_scaled <- "
      dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32
      mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32
  "

  fit_mi <- sem.mi(model = twin_diff_model_scaled, data = data_imp_diff)
  summary(fit_mi)
  parameterEstimates.mi(fit_mi)

  twin_diff_model_scaled_without_covid <- "
      dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_parent_14_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32
      mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32
  "

  fit_mi <- sem.mi(model = twin_diff_model_scaled_without_covid, data = data_imp_diff[1:2])
  summary(fit_mi)
  parameterEstimates.mi(fit_mi)



  twin_diff_full_model_scaled <- "
      dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov1_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov2_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov3_21_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_cov4_21_1_scaled_32
      mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_16_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_12_1_scaled_32

      mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32


      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32

      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32


      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32

      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32

      mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
      mpvs_total_cov1_21_1_scaled_32 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_cov2_21_1_scaled_32 ~ mpvs_total_cov1_21_1_scaled_32
      mpvs_total_cov3_21_1_scaled_32 ~ mpvs_total_cov2_21_1_scaled_32
      mpvs_total_cov4_21_1_scaled_32 ~ mpvs_total_cov3_21_1_scaled_32
  "

  fit_mi <- sem.mi(model = twin_diff_full_model_scaled, data = data_imp_diff)
  summary(fit_mi)
  parameterEstimates.mi(fit_mi)




  twin_diff_model_scaled_without_covid <- "
      dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
      dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
      mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
      mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
      mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
"

  fit_mi <- sem.mi(
    model = twin_diff_model_scaled_without_covid, data = data_imp_diff
  )
  summary(fit_mi)
  parameterEstimates.mi(fit_mi)
}
