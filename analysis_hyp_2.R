library(umx)
library(lavaan)


source("data_management_hyp_2.R")

#######################
# Using original data #
#######################

# fiml
twin_diff_model_scaled <- '
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
'

fit_fiml <- sem(
  model=twin_diff_model_scaled, data=df_all_diffs,
  missing = 'fiml'
)
summary(fit_fiml)


lavaanPlot::lavaanPlot(
  model=fit_ml,
  edge_options = list(color = "grey"), 
  coefs = TRUE, covs = TRUE,
  graph_options = list(rankdir = "LR")
)

# ml
twin_diff_model_scaled <- '
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
'

fit_ml <- sem(
  model=twin_diff_model_scaled, data=df_all_diffs
)
summary(fit_ml)

# Excluding COVID period #
twin_diff_model_scaled2 <- '
    dcq_total_26_1 ~ mpvs_total_12_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_child_14_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_16_1_scaled_32
    dcq_total_26_1 ~ mpvs_total_phase_2_21_1_scaled_32
    mpvs_total_child_14_1_scaled_32 ~ mpvs_total_12_1_scaled_32
    mpvs_total_16_1_scaled_32 ~ mpvs_total_child_14_1_scaled_32
    mpvs_total_phase_2_21_1_scaled_32 ~ mpvs_total_16_1_scaled_32
'

fit_fiml <- sem(
  model=twin_diff_model_scaled2, data=df_all_diffs,
  missing = 'fiml'
)
summary(fit_fiml)

lavaanPlot::lavaanPlot(
  model=fit_fiml,
  edge_options = list(color = "grey"), 
  coefs = TRUE,# covs = TRUE,
  graph_options = list(rankdir = "LR"),
  stars = c("regress", "latent", "covs")
)
######################
# Using imputed data #
######################
if (exists("imp") == F){
  source("imputations.R")
}

data_imp <- complete(imp, action="all")
data_imp_original <- data_imp

data_imp <- lapply(
  X=data_imp, 
  FUN = function(x){
    scale_mpvs(
      df=x,
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
##############################
# Create a df of differences #
##############################
start_time <- Sys.time()
# This needs to be in global environment
cl <- makeCluster(detectCores() - 2)
# Don't forget to call stopCluster() after calling this custom function
# objs <- ls()
clusterExport(
  cl=cl, 
  varlist=list(
    "create_df_subtract_twins_values_multiple_vars", 
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
  X=data_imp, 
  fun = function(x){
    create_df_subtract_twins_values_multiple_vars(
      df=x,
      group_var="fam_id",
      vars = c(
        "mpvs_total_12_1",
        "mpvs_total_parent_14_1",
        "mpvs_total_child_14_1",
        "mpvs_total_teacher_14_1",
        "mpvs_total_16_1",
        "mpvs_total_phase_2_21_1", 
        "mpvs_total_cov1_21_1", 
        "mpvs_total_cov2_21_1", 
        "mpvs_total_cov3_21_1",
        "mpvs_total_cov4_21_1",
        "dcq_total_26_1",
        colnames(x)[grepl(pattern="scaled",x=colnames(x))]
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
twin_diff_model <- '
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
'
# Do not use semTools, it's deprecated
library(lavaan.mi) 

fit_mi <- sem.mi(model=twin_diff_model, data=data_imp_diff)
summary(fit_mi)
parameterEstimates.mi(fit_mi) 


twin_diff_model_scaled <- '
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
'

fit_mi <- sem.mi(model=twin_diff_model_scaled, data=data_imp_diff)
summary(fit_mi)
parameterEstimates.mi(fit_mi) 

twin_diff_model_scaled2 <- '
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
'

fit_mi <- sem.mi(model=twin_diff_model_scaled2, data=data_imp_diff[1:2])
summary(fit_mi)
parameterEstimates.mi(fit_mi) 



twin_diff_full_model_scaled <- '
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
'

fit_mi <- sem.mi(model=twin_diff_full_model_scaled, data=data_imp_diff)
summary(fit_mi)
parameterEstimates.mi(fit_mi) 
