############
# Load dfs #
############

# No need to recreate dfs, just load them from Hyp1
source("data_management_hyp_1.R")


df_all_diffs <- create_df_subtract_mz_twins_values_decorated(
  df = df_essential_vars,
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
