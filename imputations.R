library(mice)
library(miceadds)
library(micemd)
library(beepr)
mice.impute.poisson <- countimp::mice.impute.poisson

SEED <- 123
set.seed(seed = SEED)

impute_items <- function(
    df,
    parallel = T,
    maxit = 1,
    m = 1,
    n.core = 1,
    keep.collinear = T,
    lower_threshold = 0.1,
    upper_threshold = 0.99,
    donors = 5,
    print_flag = F) {
  ######################
  # Impute scale items #
  ######################
  df_imp <- df_1 %>%
    dplyr::select(
      twin_id,
      fam_id,
      sex_1,
      # Ses & ethnicity serve as auxiliary vars,
      # because they have many obs and are loosely associated with
      # Bullying and BDD
      ses_1st_contact,
      ethnic,
      age_parent_12,
      age_teach_12_1,
      age_child_12_1,
      age_parent_14,
      age_teach_14_1,
      age_child_14_1,
      age_child_web_16_1,
      age_phase1_parent_21,
      age_phase1_child_21_1,
      age_phase2_child_21_1,
      age_cov1_child_21_1,
      age_cov2_child_21_1,
      age_cov3_child_21_1,
      age_cov4_child_21_1,
      age_26_1,
      colnames(df)[grepl(pattern = "item", x = colnames(df))]
    )

  df_imp <- cbind(
    df_imp,
    mpvs_total_12_1 = NA,
    mpvs_total_parent_14_1 = NA,
    mpvs_total_child_14_1 = NA,
    mpvs_total_teacher_14_1 = NA,
    mpvs_total_16_1 = NA,
    mpvs_total_phase_2_21_1 = NA,
    mpvs_total_cov1_21_1 = NA,
    mpvs_total_cov2_21_1 = NA,
    mpvs_total_cov3_21_1 = NA,
    mpvs_total_cov4_21_1 = NA,
    dcq_total_26_1 = NA
  )
  df_imp$fam_id <- as.integer(df_imp$fam_id)
  predMatrix <- make.predictorMatrix(data = df_imp)
  impMethod <- make.method(data = df_imp)

  ###############################################
  # Removing manually high/low correlated pairs #
  ###############################################
  predMatrix <- suppressWarnings( #
    exclude_collinear_vars(
      pred_matrix = predMatrix,
      corr_mat = as.data.frame(
        cor(
          df_imp %>% select(
            -all_of(c("twin_id", "fam_id", "sex_1", "ethnic", "ses_1st_contact"))
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
    var in colnames(df_imp)[grepl(pattern = "mpvs_item", x = colnames(df_imp))]
  ) {
    if (is.na(var) == F) {
      impMethod[var] <- "pmm" # "poisson" #
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
      impMethod[var] <- "2lonly.pmm"
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
  scale_items <- c(colnames(df_1)[grepl(pattern = "item", x = colnames(df_1))])
  cluster <- vector(mode = "list", length = length(scale_items))
  names(cluster) <- scale_items
  cluster <- lapply(cluster, function(x) {
    "fam_id"
  }) # Specify the level two
  # cluster[["dcq_26_1"]] <- c("fam_id") # Specify the level two

  # Define post processing funcs
  post <- make.post(df_imp)
  post["mpvs_item_2_12_1"] <- "imp[[j]][data$mpvs_item_2_12_1[!r[, j]] > 2, i] <- as.numeric(2)"
  for (
    var in colnames(df_imp)[grepl(pattern = "mpvs_item", x = colnames(df_imp))]
  ) {
    if (is.na(var) == F) {
      post[var] <- glue::glue("imp[[j]][data${var}[!r[, j]] > 2, i] <- as.numeric(2)")
    }
  }
  for (
    var in colnames(df_imp)[grepl(pattern = "dcq_item", x = colnames(df_imp))]
  ) {
    if (is.na(var) == F) {
      post[var] <- glue::glue("imp[[j]][data${var}[!r[, j]] > 7, i] <- as.numeric(7)")
    }
  }
  # https://stackoverflow.com/a/7219371
  # If mice throws error for the labels, uncomment the follow line
  df_imp <- labelled::remove_val_labels(df_imp)

  ##################
  # Add sum scores #
  ##################
  # `mice: Multivariate Imputation by Chained Equations in R`
  # https://www.jstatsoft.org/article/view/v045i03
  # See `Sum scores` section

  # https://stefvanbuuren.name/fimd/sec-knowledge.html
  # See `Sum scores` section
  # Sum scores will be added as mentioned in url above
  impMethod["mpvs_total_12_1"] <- "~I(as.integer(mpvs_item_1_12_1)+as.integer(mpvs_item_2_12_1)+as.integer(mpvs_item_3_12_1)+as.integer(mpvs_item_4_12_1)+as.integer(mpvs_item_5_12_1)+as.integer(mpvs_item_6_12_1)+as.integer(mpvs_item_7_12_1)+as.integer(mpvs_item_8_12_1)+as.integer(mpvs_item_9_12_1)+as.integer(mpvs_item_10_12_1)+as.integer(mpvs_item_11_12_1)+as.integer(mpvs_item_12_12_1)+as.integer(mpvs_item_13_12_1)+as.integer(mpvs_item_14_12_1)+as.integer(mpvs_item_15_12_1)+as.integer(mpvs_item_16_12_1))"
  impMethod["mpvs_total_parent_14_1"] <- "~I(as.integer(mpvs_item_1_parent_14_1)+as.integer(mpvs_item_2_parent_14_1)+as.integer(mpvs_item_3_parent_14_1)+as.integer(mpvs_item_4_parent_14_1)+as.integer(mpvs_item_5_parent_14_1)+as.integer(mpvs_item_6_parent_14_1)+as.integer(mpvs_item_7_parent_14_1)+as.integer(mpvs_item_8_parent_14_1)+as.integer(mpvs_item_9_parent_14_1)+as.integer(mpvs_item_10_parent_14_1)+as.integer(mpvs_item_11_parent_14_1)+as.integer(mpvs_item_12_parent_14_1)+as.integer(mpvs_item_13_parent_14_1)+as.integer(mpvs_item_14_parent_14_1)+as.integer(mpvs_item_15_parent_14_1)+as.integer(mpvs_item_16_parent_14_1))"
  impMethod["mpvs_total_child_14_1"] <- "~I(as.integer(mpvs_item_1_child_14_1)+as.integer(mpvs_item_2_child_14_1)+as.integer(mpvs_item_3_child_14_1)+as.integer(mpvs_item_4_child_14_1)+as.integer(mpvs_item_5_child_14_1)+as.integer(mpvs_item_6_child_14_1)+as.integer(mpvs_item_7_child_14_1)+as.integer(mpvs_item_8_child_14_1)+as.integer(mpvs_item_9_child_14_1)+as.integer(mpvs_item_10_child_14_1)+as.integer(mpvs_item_11_child_14_1)+as.integer(mpvs_item_12_child_14_1)+as.integer(mpvs_item_13_child_14_1)+as.integer(mpvs_item_14_child_14_1)+as.integer(mpvs_item_15_child_14_1)+as.integer(mpvs_item_16_child_14_1))"
  impMethod["mpvs_total_teacher_14_1"] <- "~I(as.integer(mpvs_item_1_teacher_14_1)+as.integer(mpvs_item_2_teacher_14_1)+as.integer(mpvs_item_3_teacher_14_1)+as.integer(mpvs_item_4_teacher_14_1)+as.integer(mpvs_item_5_teacher_14_1)+as.integer(mpvs_item_6_teacher_14_1)+as.integer(mpvs_item_7_teacher_14_1)+as.integer(mpvs_item_8_teacher_14_1)+as.integer(mpvs_item_9_teacher_14_1)+as.integer(mpvs_item_10_teacher_14_1)+as.integer(mpvs_item_11_teacher_14_1)+as.integer(mpvs_item_12_teacher_14_1)+as.integer(mpvs_item_13_teacher_14_1)+as.integer(mpvs_item_14_teacher_14_1)+as.integer(mpvs_item_15_teacher_14_1)+as.integer(mpvs_item_16_teacher_14_1))"
  impMethod["mpvs_total_16_1"] <- "~I(as.integer(mpvs_item_1_16_1)+ as.integer(mpvs_item_2_16_1)+ as.integer(mpvs_item_3_16_1)+ as.integer(mpvs_item_4_16_1)+ as.integer(mpvs_item_5_16_1)+ as.integer(mpvs_item_6_16_1))"
  impMethod["mpvs_total_phase_2_21_1"] <- "~I(as.integer(mpvs_item_1_phase_2_21_1)+as.integer(mpvs_item_2_phase_2_21_1)+as.integer(mpvs_item_3_phase_2_21_1)+as.integer(mpvs_item_4_phase_2_21_1)+as.integer(mpvs_item_5_phase_2_21_1)+as.integer(mpvs_item_6_phase_2_21_1)+as.integer(mpvs_item_7_phase_2_21_1)+as.integer(mpvs_item_8_phase_2_21_1)+as.integer(mpvs_item_9_phase_2_21_1)+as.integer(mpvs_item_10_phase_2_21_1)+as.integer(mpvs_item_11_phase_2_21_1)+as.integer(mpvs_item_12_phase_2_21_1)+as.integer(mpvs_item_13_phase_2_21_1)+as.integer(mpvs_item_14_phase_2_21_1)+as.integer(mpvs_item_15_phase_2_21_1)+as.integer(mpvs_item_16_phase_2_21_1))"
  impMethod["mpvs_total_cov1_21_1"] <- "~I(as.integer(mpvs_item_1_cov1_21_1)+as.integer(mpvs_item_2_cov1_21_1)+as.integer(mpvs_item_3_cov1_21_1)+as.integer(mpvs_item_4_cov1_21_1)+as.integer(mpvs_item_5_cov1_21_1)+as.integer(mpvs_item_6_cov1_21_1)+as.integer(mpvs_item_7_cov1_21_1)+as.integer(mpvs_item_8_cov1_21_1)+as.integer(mpvs_item_9_cov1_21_1)+as.integer(mpvs_item_10_cov1_21_1)+as.integer(mpvs_item_11_cov1_21_1)+as.integer(mpvs_item_12_cov1_21_1))"
  impMethod["mpvs_total_cov2_21_1"] <- "~I(as.integer(mpvs_item_1_cov2_21_1)+as.integer(mpvs_item_2_cov2_21_1)+as.integer(mpvs_item_3_cov2_21_1)+as.integer(mpvs_item_4_cov2_21_1)+as.integer(mpvs_item_5_cov2_21_1)+as.integer(mpvs_item_6_cov2_21_1)+as.integer(mpvs_item_7_cov2_21_1)+as.integer(mpvs_item_8_cov2_21_1)+as.integer(mpvs_item_9_cov2_21_1)+as.integer(mpvs_item_10_cov2_21_1)+as.integer(mpvs_item_11_cov2_21_1)+as.integer(mpvs_item_12_cov2_21_1))"
  impMethod["mpvs_total_cov3_21_1"] <- "~I(as.integer(mpvs_item_1_cov3_21_1)+as.integer(mpvs_item_2_cov3_21_1)+as.integer(mpvs_item_3_cov3_21_1)+as.integer(mpvs_item_4_cov3_21_1)+as.integer(mpvs_item_5_cov3_21_1)+as.integer(mpvs_item_6_cov3_21_1)+as.integer(mpvs_item_7_cov3_21_1)+as.integer(mpvs_item_8_cov3_21_1)+as.integer(mpvs_item_9_cov3_21_1)+as.integer(mpvs_item_10_cov3_21_1)+as.integer(mpvs_item_11_cov3_21_1)+as.integer(mpvs_item_12_cov3_21_1))"
  impMethod["mpvs_total_cov4_21_1"] <- "~I(as.integer(mpvs_item_1_cov4_21_1)+as.integer(mpvs_item_2_cov4_21_1)+as.integer(mpvs_item_3_cov4_21_1)+as.integer(mpvs_item_4_cov4_21_1)+as.integer(mpvs_item_5_cov4_21_1)+as.integer(mpvs_item_6_cov4_21_1)+as.integer(mpvs_item_7_cov4_21_1)+as.integer(mpvs_item_8_cov4_21_1)+as.integer(mpvs_item_9_cov4_21_1)+as.integer(mpvs_item_10_cov4_21_1)+as.integer(mpvs_item_11_cov4_21_1)+as.integer(mpvs_item_12_cov4_21_1))"
  impMethod["dcq_total_26_1"] <- "~I(as.integer(dcq_item_1_26_1)+as.integer(dcq_item_2_26_1)+as.integer(dcq_item_3_26_1)+as.integer(dcq_item_4_26_1)+as.integer(dcq_item_5_26_1)+as.integer(dcq_item_6_26_1)+as.integer(dcq_item_7_26_1))"


  # Suppose that the data consist of an outcome variable out,
  # a background variable bck, a scale a with ten items a1-a10,
  # a scale b with twelve items b1-b12,
  # and that all variables contain missing values
  # Impute out given bck, a, b, where a and b are the summed scale scores from b1-b10 and b1-b12;
  # Impute bck given out, a and b;
  # Impute a1 given out, bck, b and a2-a10;
  # Impute a2 given out, bck, b and a1, a3-a10;
  # Impute a3-a10 along the same way;
  # Impute b1 given out, bck, a and b2-b12, where a is the updated summed scale score;
  # Example: items at 12 should predictor one another, but not items from
  # a different wave! Total score at 12 should only predict items from another
  # wave!

  # Sum scores should not be predicted by other vars,
  # but they can predict others, including items from other waves.
  # Individual items can predict ONLY items from the same wave, nothing else
  # Individual items can be predicted by other waves' total and other vars
  predMatrix <- modify_pred_matrix_items_predict_nothing(
    df = predMatrix, target_phrase = "mpvs",
    target_phrase2 = "item"
  )
  predMatrix <- modify_pred_matrix_items_predict_nothing(
    df = predMatrix, target_phrase = "dcq",
    target_phrase2 = "item"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "12_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "parent_14_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "child_14_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "teacher_14_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "16_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "phase_2_21_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "cov1_21_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "cov2_21_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "cov3_21_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "cov4_21_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  predMatrix <- modify_pred_matrix(
    df = predMatrix, target_phrase = "26_1",
    target_phrase2 = "item",
    target_phrase_total = "total"
  )
  View(predMatrix)
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


imp_items <- impute_items(
  df = df_1, parallel = T, maxit = 40, m = 100, n.core = 14,
  keep.collinear = T,
  lower_threshold = 0.1,
  upper_threshold = 0.99,
  donors = 5
)
print(imp_items$loggedEvents)

imp_data_items <- complete(imp_items, action = "all")

imp_data_items <- lapply(
  imp_data_items,
  function(dataset) {
    dataset <- scale_mpvs(
      df = dataset,
      scale_size = 32,
      from_vars = colnames(dataset)[grepl(pattern = "mpvs_total", x = colnames(dataset))]
    )
  }
)

save(imp_items, imp_data_items, file = "G:\\imp_items.Rdata")


# imp_items <- impute_items(
#   df=df_1,parallel=F, maxit=1, m=2, keep.collinear=T,
#   lower_threshold=0.1,
#   upper_threshold=0.99,
#   donors=3
# )
# print(imp_items$loggedEvents)

# summary(data_imp_items)
# print(imp_items$loggedEvents)


# Recalculate all total items
# data_imp_calc <- data_imp %>%
#   calculate_items(target_phrase = "_12_1", output_var = "mpvs_total_12_1") %>%
#   calculate_items(target_phrase = "14_parent_1", output_var = "mpvs_total_14_parent_1") %>%
#   calculate_items(target_phrase = "14_child_1", output_var = "mpvs_total_14_child_1") %>%
#   calculate_items(target_phrase = "14_teacher_1", output_var = "mpvs_total_14_teacher_1") %>%
#   calculate_items(target_phrase = "16_1", output_var = "mpvs_total_16_1")  %>%
#   calculate_items(target_phrase = "_21_phase_2_1", output_var = "mpvs_total_21_phase_2_1")%>%
#   calculate_items(target_phrase = "_21_cov1_2_1", output_var = "mpvs_total_21_cov1_1")%>%
#   calculate_items(target_phrase = "_21_cov2_2_1", output_var = "mpvs_total_21_cov2_1")%>%
#   calculate_items(target_phrase = "_21_cov3_2_1", output_var = "mpvs_total_21_cov3_1") %>%
#   calculate_items(target_phrase = "_21_cov4_2_1", output_var = "mpvs_total_21_cov4_1")%>%
#   calculate_items(target_phrase = "_26_1", output_var = "dcq_26_1")



# df_temp <- df_1 %>%
#   dplyr::select(
#     sex_1,
#     ses_1st_contact,
#     ethnic,
#     # age_12_1,
#     age_parent_12,
#     age_teach_12_1,
#     age_child_12_1,
#     # age_14_1,
#     age_parent_14,
#     age_teach_14_1,
#     age_child_14_1,
#     # age_16_1,
#     age_web_16_1,
#     age_child_16_1,
#     age_parent_16,
#     age_leap_study_parent_16,
#     # age_21_1,
#     age_phase1_parent_21,
#     age_phase1_child_21_1,
#     age_phase2_child_21_1,
#     age_cov1_child_21_1,
#     age_cov2_child_21_1,
#     age_cov3_child_21_1,
#     age_cov4_child_21_1,
#     age_26_1,
#     colnames(df_1)[grepl(pattern="item",x=colnames(df_1))]
#   )
# fluxplot(df_temp)
