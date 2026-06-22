library(countimp)
library(mice)
library(miceadds)
library(micemd)
library(beepr)
# install.packages('remotes')
# remotes::install_github('kkleinke/countimp')
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
  print_flag = F
) {
  ######################
  # Impute scale items #
  ######################

  # The df is a long format (each row is a Twin)
  # We need to convert it to wide (each row is a Family)
  df_wide <- df %>%
    mutate(pair_order = ifelse(pair_order == 1, "A", "B")) %>%
    pivot_wider(
      id_cols = fam_id,
      names_from = pair_order,
      values_from = c(
        twin_id,
        sex_1,
        age_child_14_1,
        age_child_web_16_1,
        age_phase2_child_21_1,
        age_26_1,
        colnames(df)[
          grepl(pattern = "mpvs_item_\\d+_12_1", x = colnames(df)) &
            !grepl(pattern = "cov", x = colnames(df)) &
            !grepl(pattern = "teacher", x = colnames(df)) &
            !grepl(pattern = "parent", x = colnames(df))
        ],
        mpvs_total_12_1,
        colnames(df)[
          grepl(pattern = "mpvs_item_\\d+_child_14_1", x = colnames(df)) &
            !grepl(pattern = "cov", x = colnames(df)) &
            !grepl(pattern = "teacher", x = colnames(df)) &
            !grepl(pattern = "parent", x = colnames(df))
        ],
        mpvs_total_child_14_1,
        colnames(df)[
          grepl(pattern = "mpvs_item_\\d+_16_1", x = colnames(df)) &
            !grepl(pattern = "cov", x = colnames(df)) &
            !grepl(pattern = "teacher", x = colnames(df)) &
            !grepl(pattern = "parent", x = colnames(df))
        ],
        mpvs_total_16_1,
        colnames(df)[
          grepl(pattern = "mpvs_item_\\d+_phase_2_21_1", x = colnames(df)) &
            !grepl(pattern = "cov", x = colnames(df)) &
            !grepl(pattern = "teacher", x = colnames(df)) &
            !grepl(pattern = "parent", x = colnames(df))
        ],
        mpvs_total_phase_2_21_1, # dcq_item_\\d+_26_1
        colnames(df)[
          grepl(pattern = "dcq_item_\\d+_26_1", x = colnames(df)) &
            !grepl(pattern = "cov", x = colnames(df)) &
            !grepl(pattern = "teacher", x = colnames(df)) &
            !grepl(pattern = "parent", x = colnames(df))
        ],
        dcq_total_26_1
      )
    )

  # Add these afterwards,
  # Because they contain the same value for each twin within the family

  df_wide <- left_join(
    x = df_wide,
    y = (
      # We need to group the 2 rows (row=twin_id) to 1 (row = fam_id)
      df_1 %>%
        group_by(fam_id) %>%
        summarise(
          ses_1st_contact = first(ses_1st_contact),
          ethnic = first(ethnic),
          age_child_12_1 = first(age_child_12_1),
          .groups = "drop"
        )
    ),
    by = join_by("fam_id" == "fam_id")
  )


  impMethod <- make.method(df_wide)
  pred_matrix <- make.predictorMatrix(df_wide)

  #####################
  # Prediction Matrix #
  #####################

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
  # Individual items can be predicted by other waves' total and other   vars

  pred_matrix <- pred_matrix %>%
    modify_pred_matrix_scales_AB(
      item_pattern = "mpvs_item_\\d+_12_1",
      total_pattern = "mpvs_total_12_1",
      twin1_pattern = "_A$",
      twin2_pattern = "_B$"
    ) %>%
    modify_pred_matrix_scales_AB(
      item_pattern = "mpvs_item_\\d+_child_14_1",
      total_pattern = "mpvs_total_child_14_1",
      twin1_pattern = "_A$",
      twin2_pattern = "_B$"
    ) %>%
    modify_pred_matrix_scales_AB(
      item_pattern = "mpvs_item_\\d+_16_1",
      total_pattern = "mpvs_total_16_1",
      twin1_pattern = "_A$",
      twin2_pattern = "_B$"
    ) %>%
    modify_pred_matrix_scales_AB(
      item_pattern = "mpvs_item_\\d+_phase_2_21_1",
      total_pattern = "mpvs_total_phase_2_21_1",
      twin1_pattern = "_A$",
      twin2_pattern = "_B$"
    ) %>%
    modify_pred_matrix_scales_AB(
      item_pattern = "dcq_item_\\d+_26_1",
      total_pattern = "dcq_total_26_1",
      twin1_pattern = "_A$",
      twin2_pattern = "_B$"
    )


  pred_matrix[, c("twin_id_A", "twin_id_B")] <- 0
  pred_matrix[c("twin_id_A", "twin_id_B"), ] <- 0

  pred_matrix["fam_id", ] <- 0
  pred_matrix[, "fam_id"] <- 0

  pred_matrix[c("age_26_1_A", "age_26_1_B"), ] <- 0
  diag(pred_matrix) <- 0

  ###########
  # Methods #
  ###########
  # impMethod[] <- "pmm"
  impMethod[c("twin_id_A", "twin_id_B", "fam_id", "age_26_1_A", "age_26_1_B")] <- ""
  ##################
  # Visit Sequence #
  ##################


  visit_order < c(
    "ses_1st_contact",
    "ethnic",
    "age_child_12_1",
    "age_child_14_1_A", "age_child_14_1_B",
    "age_child_web_16_1_A", "age_child_web_16_1_B",
    "age_phase2_child_21_1_A", "age_phase2_child_21_1_B",
    "age_26_1_A", "age_26_1_B",
    grep("mpvs_item_.*_12_1_A$", colnames(df_wide), value = TRUE),
    "mpvs_total_12_1_A",
    grep("mpvs_item_.*_12_1_B$", colnames(df_wide), value = TRUE),
    "mpvs_total_12_1_B",
    grep("mpvs_item_.*_child_14_1_A$", colnames(df_wide), value = TRUE),
    "mpvs_total_child_14_1_A",
    grep("mpvs_item_.*_child_14_1_B$", colnames(df_wide), value = TRUE),
    "mpvs_total_child_14_1_B",
    grep("mpvs_item_.*_16_1_A$", colnames(df_wide), value = TRUE),
    "mpvs_total_16_1_A",
    grep("mpvs_item_.*_16_1_B$", colnames(df_wide), value = TRUE),
    "mpvs_total_16_1_B",
    grep("mpvs_item_.*_phase_2_21_1_A$", colnames(df_wide), value = TRUE),
    "mpvs_total_phase_2_21_1_A",
    grep("mpvs_item_.*_phase_2_21_1_B$", colnames(df_wide), value = TRUE),
    "mpvs_total_phase_2_21_1_B",
    grep("dcq_item_.*_26_1_A$", colnames(df_wide), value = TRUE),
    "dcq_total_26_1_A",
    grep("dcq_item_.*_26_1_B$", colnames(df_wide), value = TRUE),
    "dcq_total_2  6_1_B"
  )

  ###############################################
  # Removing manually high/low correlated pairs #
  ###############################################
  pred_matrix <- suppressWarnings( #
    exclude_collinear_vars(
      pred_matrix = pred_matrix,
      corr_mat = as.data.frame(
        cor(
          df_wide %>% dplyr::select(
            !all_of(
              c("twin_id_A", "twin_id_B", "fam_id", "sex_1_A", "sex_1_B", "ethnic", "ses_1st_contact")
            )
          ),
          use = "pairwise.complete.obs"
        )
      ),
      lower_threshold = lower_threshold,
      upper_threshold = upper_threshold
    )
  )

  ##################
  # Add sum scores #
  ##################
  # `mice: Multivariate Imputation by Chained Equations in R`
  # https://www.jstatsoft.org/article/view/v045i03
  # See `Sum scores` section

  # https://stefvanbuuren.name/fimd/sec-knowledge.html
  # See `Sum scores` section
  # Sum scores will be added as mentioned in url above

  make_mpvs_total <- function(prefix, suffix, n_items = 16) {
    items <- paste0("mpvs_item_", 1:n_items, "_", prefix, "_", suffix)
    paste0("~I(", paste0("as.integer(", items, ")", collapse = " + "), ")")
  }

  impMethod["mpvs_total_12_1_A"] <- make_mpvs_total("12_1", "A")
  impMethod["mpvs_total_12_1_B"] <- make_mpvs_total("12_1", "B")

  impMethod["mpvs_total_child_14_1_A"] <- make_mpvs_total("child_14_1", "A")
  impMethod["mpvs_total_child_14_1_B"] <- make_mpvs_total("child_14_1", "B")

  impMethod["mpvs_total_16_1_A"] <- make_mpvs_total("16_1", "A", n_items = 6)
  impMethod["mpvs_total_16_1_B"] <- make_mpvs_total("16_1", "B", n_items = 6)

  impMethod["mpvs_total_phase_2_21_1_A"] <- make_mpvs_total("phase_2_21_1", "A")
  impMethod["mpvs_total_phase_2_21_1_B"] <- make_mpvs_total("phase_2_21_1", "B")

  make_dcq_total <- function(prefix, suffix, n_items) {
    items <- paste0("dcq_item_", 1:n_items, "_", prefix, "_", suffix)
    paste0("~I(", paste0("as.integer(", items, ")", collapse = " + "), ")")
  }
  impMethod["dcq_total_26_1_A"] <- make_dcq_total("26_1", "A", 7)
  impMethod["dcq_total_26_1_B"] <- make_dcq_total("26_1", "B", 7)

  # View(pred_matrix)
  # View(impMethod)


  # https://stackoverflow.com/a/7219371
  # If mice throws error for the labels, uncomment the follow line
  df_wide <- labelled::remove_val_labels(df_wide)


  #############
  # Call mice #
  #############
  gc()
  startTime <- Sys.time()
  if (parallel == F) {
    imp <- mice(
      df_wide,
      method = impMethod, predictorMatrix = pred_matrix, maxit = maxit,
      m = m, seed = SEED, remove.collinear = !keep.collinear,
      donors = donors, visitSequence = visit_order
    )
  } else if (parallel == T) {
    # Run in parallel
    print(parallelly::availableCores(logical = TRUE))
    imp <- futuremice(
      df_wide,
      method = impMethod, predictorMatrix = pred_matrix, maxit = maxit,
      m = m, parallelseed = SEED, n.core = n.core, donors = donors,
      print = print_flag, remove.collinear = !keep.collinear,
      visitSequence = visit_order,
      packages = c("mice", "miceadds", "micemd")
    )
    beepr::beep("mario")
    Sys.sleep(0.5)
  }
  print(Sys.time() - startTime)
  return(imp)
}

imp_items <- impute_items(
  df = df_1, parallel = F, maxit = 10, m = 50, n.core = 10,
  keep.collinear = T,
  lower_threshold = 0.1,
  upper_threshold = 0.99,
  donors = 5,
  print_flag = F
)
print(class(imp_items))
print(names(imp_items))
print(str(imp_items$imp, max.level = 1))
print(imp_items$m)
print(imp_items$loggedEvents)


# imp_data_items <- complete(imp_items, "long", include = F, order = "first")

long_list <- lapply(1:imp_items$m, function(i) {
  complete(imp_items, i)
})


# pivot each imputed dataset separately
long_list <- lapply(
  long_list, function(df) {
    df %>%
      pivot_longer(
        cols = matches("_(A|B)$"),
        names_to = c(".value", "pair_order"),
        names_pattern = "^(.*)_(A|B)$"
      )
  }
)

long_list <- lapply(
  long_list, function(df) {
    scale_mpvs(
      df = df,
      scale_size = 32,
      from_vars = colnames(df)[grepl(
        pattern = "mpvs_total",
        x = colnames(df)
      )]
    )
  }
)


# save(imp_items, imp_data_items, file = "G:\\imp_items.Rta")
