library(mice)
library(miceadds)
library(micemd)

SEED <- 123
set.seed(seed = SEED)

impute_total_scaled_df <- function(
    df,
    parallel = T,
    maxit = 1,
    m = 1,
    n.core = 1,
    keep.collinear = T,
    lower_threshold = 0.1,
    upper_threshold = 0.99,
    donors = 5,
    print_flag = F,
    imp_method_low_level = "pmm",
    imp_method_top_level = "2lonly.pmm") {
  ######################
  # Impute variables   #
  ######################
  df_imp <- df %>%
    dplyr::select(
      all_of(
        c(
          "twin_id",
          "fam_id",
          "sex_1",
          "ses_1st_contact",
          "ethnic_fct",
          "age_child_12_1",
          "age_child_14_1",
          "age_child_web_16_1",
          "age_phase2_child_21_1",
          "age_cov1_child_21_1",
          "age_cov2_child_21_1",
          "age_cov3_child_21_1",
          "age_cov4_child_21_1",
          "age_26_1",
          "mpvs_total_12_1_scaled_32",
          "mpvs_total_parent_14_1_scaled_32",
          "mpvs_total_child_14_1_scaled_32",
          "mpvs_total_teacher_14_1_scaled_32",
          "mpvs_total_16_1_scaled_32",
          "mpvs_total_phase_2_21_1_scaled_32",
          "mpvs_total_cov1_21_1_scaled_32",
          "mpvs_total_cov2_21_1_scaled_32",
          "mpvs_total_cov3_21_1_scaled_32",
          "mpvs_total_cov4_21_1_scaled_32",
          "dcq_total_26_1"
        )
      )
    )

  df_imp$fam_id <- as.integer(df_imp$fam_id)
  predMatrix <- make.predictorMatrix(data = df_imp)
  impMethod <- make.method(data = df_imp)
  # post <- make.post(data=df_imp)

  ###############################################
  # Removing manually high/low correlated pairs #
  ###############################################
  predMatrix <- suppressWarnings( #
    exclude_collinear_vars(
      pred_matrix = predMatrix,
      corr_mat = as.data.frame(
        cor(
          df_imp %>% select(
            -all_of(c("twin_id", "fam_id", "sex_1", "ethnic_fct", "ses_1st_contact"))
          ),
          use = "pairwise.complete.obs"
        )
      ),
      lower_threshold = lower_threshold,
      upper_threshold = upper_threshold
    )
  )

  ####################################
  # method for lower-level variables #
  ####################################
  for (
    var in colnames(df_imp)[grepl(pattern = "mpvs_total", x = colnames(df_imp))]
  ) {
    if (is.na(var) == F) {
      impMethod[var] <- imp_method_low_level # "poisson" #
      # Alternatively, use "poisson". However, note that it's memory intensive
      # and takes a lot of time
    }
  }
  #####################################
  # method for variables at top level #
  #####################################

  # remove cluster indicator variables from predictor matrix
  predMatrix[, c("fam_id")] <- 0 # Should not predict anything
  predMatrix[c("fam_id"), ] <- 0 # Should not be predicted by anything


  for (
    var in colnames(df_imp)[grepl(pattern = "age", x = colnames(df_imp))]
  ) {
    print(glue::glue("Two level variable; {var}"))
    if (is.na(var) == F) {
      impMethod[var] <- imp_method_top_level
      # specify cluster indicator (2lonly.norm)
      predMatrix[var, "fam_id"] <- -2
    }
  }
  predMatrix[c("sex_1"), ] <- 0

  predMatrix[, c("twin_id")] <- 0
  predMatrix[c("twin_id"), ] <- 0


  level <- character(ncol(df_imp))
  names(level) <- colnames(df_imp)

  # specify cluster indicators (as list)
  scale_items <- c(colnames(df_imp)[grepl(pattern = "item", x = colnames(df_imp))])
  cluster <- vector(mode = "list", length = length(scale_items))
  names(cluster) <- scale_items
  cluster <- lapply(cluster, function(x) {
    "fam_id"
  })


  # https://stackoverflow.com/a/7219371
  # If mice throws error for the labels, uncomment the follow line
  df_imp <- labelled::remove_val_labels(df_imp)
  #
  # impMethod["age_12_1"] <- "2lonly.norm"
  # impMethod["age_14_1"] <- "2lonly.norm"
  # impMethod["age_16_1"] <- "2lonly.norm"
  # impMethod["age_21_1"] <- "2lonly.norm"
  # impMethod["age_26_1"] <- "2lonly.norm"
  #
  # predMatrix[c("sex_1"),] <- 0
  #
  # # remove cluster indicator variables from predictor matrix
  # predMatrix[, c("fam_id")] <- 0
  #
  # # ... specify cluster indicator (2lonly.norm)
  # predMatrix["age_12_1", "fam_id"] <- -2
  # predMatrix["age_14_1", "fam_id"] <- -2
  # predMatrix["age_16_1", "fam_id"] <- -2
  # predMatrix["age_21_1", "fam_id"] <- -2
  # predMatrix["age_26_1", "fam_id"] <- -2
  #
  # level <- character(ncol(df_imp))
  # names(level) <- colnames(df_imp)
  #
  # # specify cluster indicators (as list)
  # scale_items <- c(colnames(df_imp)[grepl(pattern="mpvs",x=colnames(df_imp))])
  # cluster <- vector(mode="list",length=length(scale_items))
  # names(cluster) <- scale_items
  # cluster <- lapply(cluster, function(x){"fam_id"}) # Specify the level two
  # # cluster[["dcq_26_1"]] <- c("fam_id") # Specify the level two
  #
  # # https://stackoverflow.com/a/7219371
  # # If mice throws error for the labels, uncomment the follow line
  # df_imp <- labelled::remove_val_labels(df_imp)
  print(impMethod)
  #############
  # Call mice #
  #############
  gc()
  startTime <- Sys.time()
  if (parallel == F) {
    imp <- mice(
      df_imp,
      method = impMethod, predictorMatrix = predMatrix, maxit = maxit,
      m = m, levels_id = cluster, variables_levels = level, seed = SEED,
      post = post, remove.collinear = !keep.collinear,
      donors = donors
    )
  } else if (parallel == T) {
    # Run in parallel
    print(parallelly::availableCores(logical = TRUE))
    imp <- futuremice(
      df_imp,
      method = impMethod, predictorMatrix = predMatrix, maxit = maxit,
      m = m, levels_id = cluster, variables_levels = level, parallelseed = SEED,
      post = post, n.core = n.core, donors = donors,
      print = print_flag, remove.collinear = !keep.collinear
    )
    beepr::beep("mario")
    Sys.sleep(0.5)
  }
  print(Sys.time() - startTime)
  return(imp)
}

imp_derived <- impute_total_scaled_df(
  df = df_essential_vars, parallel = T, maxit = 30, m = 112, n.core = 14,
  keep.collinear = T,
  lower_threshold = 0.1,
  upper_threshold = 0.99,
  donors = 5
)
# Plots look good!
plot(imp_derived)
print(imp_derived$loggedEvents)
imp_data_derived <- complete(imp_derived, action = "all")
save(imp_derived, imp_data_derived, file = "G:\\imp_derived.Rdata")
