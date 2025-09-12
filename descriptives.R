library(gt)
# It's faster than running data_wrangling.R
if (sys.nframe() == 0) {
  # if (exists("df_1") == F) {
  #   load(file = "G:\\data_wrangling.Rdata")
  # }
  # if (exists("df_essential_vars") == F) {
  #   source("data_management_hyp_1.R")
  # }
  if (exists("df_all_diffs") == F) {
    source("data_management_hyp_2.R")
  }
}

library(gtsummary)
library(tidyverse)
library(lavaan)

corr_mat <- cor(
  df_essential_vars %>% select(
    all_of(
      c(
        "dcq_total_26_1",
        colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))]
      )
    )
  ),
  use = "pairwise.complete.obs",
  method = "spearman"
)

corr_mat_mpvs_dcq <- cor(
  df_essential_vars %>% select(
    all_of(
      c(
        "dcq_total_26_1", "mpvs_total_12_1", "mpvs_total_child_14_1",
        "mpvs_total_16_1", "mpvs_total_phase_2_21_1"
      )
    )
  ),
  use = "pairwise.complete.obs"
)

table_corr_mat_mpvs_dcq <- corr_mat_mpvs_dcq %>%
  as.data.frame() %>%
  gt::gt(
    # https://stackoverflow.com/questions/75260770/how-to-add-the-row-names-to-my-gt-table
    rownames_to_stub = T,
    caption = "Table 2. Correlation matrix of DCQ and MPVS across waves"
  ) |>
  gt::text_case_match(
    "dcq_total_26_1" ~ "DCQ",
    "mpvs_total_12_1" ~ "MPVS (12y)",
    "mpvs_total_child_14_1" ~ "MPVS (14y)",
    "mpvs_total_16_1" ~ "MPVS (16y)",
    "mpvs_total_phase_2_21_1" ~ "MPVS (21y)"
  ) %>%
  gt::tab_header(
    title = "",
    subtitle = "Table 2. Spearman correlation matrix of DCQ and MPVS across waves"
  ) %>%
  gt::cols_label(
    dcq_total_26_1 = "DCQ",
    mpvs_total_12_1 = "MPVS (12y)",
    mpvs_total_child_14_1 = "MPVS (14y)",
    mpvs_total_16_1 = "MPVS (16y)",
    mpvs_total_phase_2_21_1 = "MPVS (21y)"
  ) |>
  gt::text_case_match(
    "dcq_total_26_1" ~ "DCQ",
    "mpvs_total_12_1" ~ "MPVS (12y)",
    "mpvs_total_child_14_1" ~ "MPVS (14y)",
    "mpvs_total_16_1" ~ "MPVS (16y)",
    "mpvs_total_phase_2_21_1" ~ "MPVS (21y)",
    .locations = gt::cells_stub()
  ) %>%
  gt::fmt_number(
    decimals = 3
  ) %>%
  gt::tab_options(heading.subtitle.font.size = "20pt") %>%
  gt::tab_footnote(
    footnote = "DCQ: Dysmorphic concerns questionnaire, MPVS: Multidimensional peer victimization scale, y: year(s)",
    locations = NULL
  )


summary <- gtsummary::tbl_summary(
  data = df_essential_vars %>% select(
    colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))],
    dcq_total_26_1, zygosity_binary_fct, sex_1
  ),
  by = zygosity_binary_fct,
  statistic = list(
    all_categorical() ~ "{n}    ({p}%)",
    all_continuous() ~ "{mean} ({sd})"
  ),
  missing_text = "Missing"
) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  modify_caption("Participant characteristics, by zygosity") %>%
  bold_labels() %>%
  # Include an "overall" column
  add_overall(
    last = T,
    # The ** make it bold
    col_label = "**All participants**<br>N = {N}"
  )

summary_ages <- gtsummary::tbl_summary(
  data = df_essential_vars %>% select(
    -c(
      colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))],
      dcq_total_26_1, sex_1
    )
  ),
  by = zygosity_binary_fct,
  statistic = list(
    all_categorical() ~ "{n}    ({p}%)",
    all_continuous() ~ "{mean} ({sd})"
  ),
  missing_text = "Missing",
  missing_stat = "{N_miss} ({p_miss}%)",
) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  modify_caption("Participant characteristics, by zygosity") %>%
  bold_labels() %>%
  # Include an "overall" column
  add_overall(
    last = T,
    # The ** make it bold
    col_label = "**All participants**<br>N = {N}"
  )


summary_ages_by_cohort <- gtsummary::tbl_summary(
  data = df_essential_vars %>% select(
    -c(
      colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))],
      dcq_total_26_1, sex_1
    )
  ),
  by = school_cohort_fct,
  statistic = list(
    all_categorical() ~ "{n}    ({p}%)",
    all_continuous() ~ "{mean} ({sd})"
  ),
  missing_text = "Missing",
  missing_stat = "{N_miss} ({p_miss}%)",
) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  modify_caption("Participant characteristics, by cohort") %>%
  bold_labels() %>%
  # Include an "overall" column
  add_overall(
    last = T,
    # The ** make it bold
    col_label = "**All participants**<br>N = {N}"
  )


summary_by_cohort <- gtsummary::tbl_summary(
  data = df_essential_vars %>% select(
    c(
      colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))],
      dcq_total_26_1, sex_1, school_cohort_fct
    )
  ),
  by = school_cohort_fct,
  statistic = list(
    all_categorical() ~ "{n}    ({p}%)",
    all_continuous() ~ "{mean} ({sd})"
  ),
  missing_text = "Missing",
  missing_stat = "{N_miss} ({p_miss}%)",
) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  modify_caption("Participant characteristics, by cohort") %>%
  bold_labels() %>%
  # Include an "overall" column
  add_overall(
    last = T,
    # The ** make it bold
    col_label = "**All participants**<br>N = {N}"
  )




summary_all <- df_essential_vars %>%
  select(
    c(
      "zygosity_binary_fct",
      # "school_cohort_fct",
      "sex_1_fct",
      "eating_diagnosis_fct_26_1",
      "age_child_12_1",
      "age_child_14_1",
      "age_child_web_16_1",
      "age_phase2_child_21_1",
      "age_26_1",
      "mpvs_total_12_1",
      "mpvs_total_child_14_1",
      "mpvs_total_16_1",
      "mpvs_total_phase_2_21_1",
      "dcq_total_26_1"
    )
  ) %>%
  gtsummary::tbl_summary(
    by = zygosity_binary_fct,
    statistic = list(
      all_categorical() ~ "{n}    ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",
    label = list(
      # school_cohort_fct ~ "School cohort",
      sex_1_fct ~ "Sex",
      eating_diagnosis_fct_26_1 ~ "Eating disorder diagnosis",
      age_child_12_1 ~ "wave 12y",
      age_child_14_1 ~ "wave 14y",
      age_child_web_16_1 ~ "wave 16y",
      age_phase2_child_21_1 ~ "wave 21y",
      age_26_1 ~ "wave 26y",
      mpvs_total_12_1 ~ "wave 12y",
      mpvs_total_child_14_1 ~ "wave 14y",
      mpvs_total_16_1 ~ "wave 16y",
      mpvs_total_phase_2_21_1 ~ "wave 21y",
      dcq_total_26_1 ~ "DCQ total score"
    )
  ) %>%
  bold_labels() %>%
  # See: https://stackoverflow.com/questions/65665465/grouping-rows-in-gtsummary
  modify_table_body(
    mutate,
    groupname_col = case_when(
      variable %in% c(
        "age_child_12_1", "age_child_14_1", "age_child_web_16_1",
        "age_phase2_child_21_1", "age_26_1"
      ) ~ "Age",
      variable %in% c(
        "mpvs_total_12_1",
        "mpvs_total_child_14_1",
        "mpvs_total_16_1",
        "mpvs_total_phase_2_21_1"
      ) ~ "MPVS total score"
    )
  ) %>%
  modify_column_indent(
    columns = label,
    rows = variable %in% c(
      "age_child_12_1", "age_child_14_1", "age_child_web_16_1",
      "age_phase2_child_21_1", "age_26_1",
      "mpvs_total_12_1",
      "mpvs_total_child_14_1",
      "mpvs_total_16_1",
      "mpvs_total_phase_2_21_1"
    )
  ) %>%
  modify_column_indent(
    columns = label,
    rows = row_type %in% c("Missing", "missing"),
    indent = 8
  ) %>%
  modify_header(
    label = "**Variable**"
  ) %>%
  modify_caption("Table 1. Participant characteristics") %>%
  # Include an "overall" column
  add_overall(
    last = T,
    # The ** make it bold
    col_label = "**All participants**<br> \n N = {N}"
  ) %>%
  add_p(
    test = list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"),
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  # This functions converts gtsummary to gt
  bstfun::bold_italicize_group_labels(bold = T) %>%
  gt::tab_header(
    title = "",
    subtitle = "Table 1. Participants characteristics"
  ) %>%
  gt::tab_options(heading.subtitle.font.size = "20px") %>%
  gt::tab_footnote(
    footnote = "N refers to individuals, MZ: Monozygotic, DZ: Dizygotic, DCQ: Dysmorphic concerns questionnaire, MPVS: Multidimensional peer victimization scale, y: year(s)",
    locations = NULL
  )


summary_all


df_essential_vars_long <- pivot_longer(
  df_essential_vars,
  colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))],
  names_to = "timepoint",
  values_to = "mpvs"
)

df_essential_vars_long$timepoint <- stringr::str_remove_all(
  pattern = "mpvs_total_",
  string = df_essential_vars_long$timepoint
)
df_essential_vars_long$timepoint <- factor(
  df_essential_vars_long$timepoint,
  stringr::str_remove_all(
    pattern = "mpvs_total_",
    string = c(colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))])
  )
)

mpvs_boxplot <- df_essential_vars_long %>%
  ggplot(aes(x = timepoint, y = mpvs)) +
  geom_boxplot() +
  theme(
    title = element_text("MPVS scale at different timepoints"),
    axis.title.x = element_text("Score")
  )


var_vec <- colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))]

spot_age <- function(string) {
  if (str_detect(string = string, pattern = "_12$")) {
    string <- str_replace(string = string, pattern = "_12", replacement = "")
    return(12)
  }
  if (str_detect(string = string, pattern = "_14$")) {
    string <- str_replace(string = string, pattern = "_14", replacement = "")
    return(14)
  }
  if (str_detect(string = string, pattern = "_16$")) {
    string <- str_replace(string = string, pattern = "_16", replacement = "")
    return(16)
  }
  if (str_detect(string = string, pattern = "_21$")) {
    string <- str_replace(string = string, pattern = "_21", replacement = "")
    if (str_detect(string = string, pattern = "cov1")) {
      return(21.2)
    }
    if (str_detect(string = string, pattern = "cov2")) {
      return(21.4)
    }
    if (str_detect(string = string, pattern = "cov3")) {
      return(21.6)
    }
    if (str_detect(string = string, pattern = "cov4")) {
      return(21.8)
    }
    print(string)
    return(21)
  }
  if (str_detect(string = string, pattern = "_26$")) {
    string <- str_replace(string = string, pattern = "_26", replacement = "")
    return(26)
  }
}

spot_informant <- function(string) {
  if (str_detect(string = string, pattern = "child")) {
    return("child")
  }
  if (str_detect(string = string, pattern = "parent")) {
    return("parent")
  }
  if (str_detect(string = string, pattern = "teacher")) {
    return("teacher")
  } else { # No specifier -> the informant is the twin
    return("child")
  }
}

extract_mpvs_info <- function(string) {
  to_return <- c()
  print(string)
  string <- str_replace(string = string, pattern = "_scaled_32$", replacement = "")
  string <- str_replace(string = string, pattern = "_1$", replacement = "")
  to_return <- cbind(to_return, spot_age(string = string))
  to_return <- cbind(to_return, spot_informant(string = string))
  print(string)
  return(to_return)
}

create_summary_df <- function(df, var_vec) {
  inner_df <- data.frame(
    mean = NA,
    sd = NA,
    median = NA,
    age = NA,
    informant = NA
  )
  for (num in 1:length(var_vec)) {
    mean_ <- mean(df[, var_vec[num]], na.rm = T)
    median_ <- median(df[, var_vec[num]], na.rm = T)
    sd_ <- sd(df[, var_vec[num]], na.rm = T)
    inner_df[num, ] <- c(
      mean_, median_, sd_, extract_mpvs_info(string = var_vec[num])
    )
  }
  inner_df[, c("mean", "median", "sd", "age")] <- lapply(
    X = inner_df[, c("mean", "median", "sd", "age")],
    FUN = as.numeric
  )
  inner_df$informant <- as.factor(inner_df$informant)
  return(inner_df)
}

summary_df <- create_summary_df(
  df = df_1,
  var_vec = colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))]
)


var_vec <- colnames(df_essential_vars)[grepl(pattern = "mpvs_total", x = colnames(df_essential_vars))]

summary_scaled_df <- create_summary_df(
  df = df_essential_vars,
  var_vec = var_vec[grepl(pattern = "scaled_32", x = var_vec)]
)


df_essential_vars_long_scaled <- pivot_longer(
  df_essential_vars,
  colnames(df_essential_vars)[grepl(pattern = "scaled_32", x = colnames(df_essential_vars))],
  names_to = "timepoint",
  values_to = "mpvs"
)

df_essential_vars_long_scaled$timepoint <- stringr::str_remove_all(
  pattern = "mpvs_total_",
  string = df_essential_vars_long_scaled$timepoint
)

df_essential_vars_long_scaled$timepoint <- stringr::str_remove_all(
  pattern = "_scaled_32",
  string = df_essential_vars_long_scaled$timepoint
)

df_essential_vars_long_scaled$timepoint <- factor(
  df_essential_vars_long_scaled$timepoint,
  stringr::str_remove_all(
    pattern = "mpvs_total_",
    string = c(colnames(df_1)[grepl(pattern = "mpvs_total", x = colnames(df_1))])
  )
)

mpvs_boxplot_scaled <- df_essential_vars_long_scaled %>%
  ggplot(aes(x = timepoint, y = mpvs)) +
  geom_boxplot() +
  theme(
    title = element_text("MPVS scale at different timepoints"),
    axis.title.x = element_text("Score")
  )


df_1_compl_cases <- df_essential_vars %>%
  dplyr::select(all_of(vars))

vars_to_subset <- vars[!grepl(pattern = "cov", x = vars)]
vars_to_subset <- vars_to_subset[!grepl(pattern = "ses", x = vars_to_subset)]
vars_to_subset <- vars_to_subset[!grepl(pattern = "ethnic", x = vars_to_subset)]
vars_to_subset <- vars_to_subset[!grepl(pattern = "teach", x = vars_to_subset)]
vars_to_subset <- vars_to_subset[!grepl(pattern = "parent", x = vars_to_subset)]
vars_to_subset <- vars_to_subset[!grepl(pattern = "leap", x = vars_to_subset)]


df_1_compl_cases <- df_1_compl_cases[
  complete.cases(df_1_compl_cases[, vars_to_subset]),
]



df_all_diffs_long <- pivot_longer(
  data = df_all_diffs %>% select(
    all_of(
      c(
        "fam_id",
        "dcq_total_26_1",
        "mpvs_total_12_1_scaled_32",
        "mpvs_total_child_14_1_scaled_32",
        "mpvs_total_16_1_scaled_32",
        "mpvs_total_phase_2_21_1_scaled_32"
      )
    )
  ),
  cols = c(
    "mpvs_total_12_1_scaled_32",
    "mpvs_total_child_14_1_scaled_32",
    "mpvs_total_16_1_scaled_32",
    "mpvs_total_phase_2_21_1_scaled_32",
  ),
  names_to = "mpvs_age",
  values_to = "mpvs"
)


# Means across waves
vars <- colnames(df_essential_vars)[grepl(pattern = "scaled", x = colnames(df_essential_vars))]
vars <- c(
  "mpvs_total_12_1_scaled_32",
  "mpvs_total_child_14_1_scaled_32",
  "mpvs_total_16_1_scaled_32",
  "mpvs_total_phase_2_21_1_scaled_32",
  "mpvs_total_cov1_21_1_scaled_32",
  "mpvs_total_cov2_21_1_scaled_32",
  "mpvs_total_cov3_21_1_scaled_32",
  "mpvs_total_cov4_21_1_scaled_32"
)
df_without_NA <- df_essential_vars %>%
  # filter(!if_all(colnames(df), is.na))
  filter(
    !if_any(
      vars,
      is.na
    )
  )

summary_df_without_NA_21 <- create_summary_df(
  df = (
    df_essential_vars %>%
      # filter(!if_all(colnames(df), is.na))
      filter(
        !if_any(
          c(
            "mpvs_total_phase_2_21_1_scaled_32",
            "mpvs_total_cov1_21_1_scaled_32",
            "mpvs_total_cov2_21_1_scaled_32",
            "mpvs_total_cov3_21_1_scaled_32",
            "mpvs_total_cov4_21_1_scaled_32"
          ),
          is.na
        )
      )
  ),
  var_vec = c(
    "mpvs_total_phase_2_21_1_scaled_32",
    "mpvs_total_cov1_21_1_scaled_32",
    "mpvs_total_cov2_21_1_scaled_32",
    "mpvs_total_cov3_21_1_scaled_32",
    "mpvs_total_cov4_21_1_scaled_32"
  )
)
summary_df_without_NA <- create_summary_df(
  df = df_without_NA,
  var_vec = c(
    "mpvs_total_12_1_scaled_32",
    "mpvs_total_child_14_1_scaled_32",
    "mpvs_total_16_1_scaled_32",
    "mpvs_total_phase_2_21_1_scaled_32",
    "mpvs_total_cov1_21_1_scaled_32",
    "mpvs_total_cov2_21_1_scaled_32",
    "mpvs_total_cov3_21_1_scaled_32",
    "mpvs_total_cov4_21_1_scaled_32"
  )
)

twin_pairs_incompleteness <- find_complete(df_1, var = "dcq_total_26_1")
twin_pairs_incompleteness <- find_complete(
  twin_pairs_incompleteness,
  var = "mpvs_total_12_1"
)
twin_pairs_incompleteness <- find_complete(
  twin_pairs_incompleteness,
  var = "mpvs_total_child_14_1"
)
twin_pairs_incompleteness <- find_complete(
  twin_pairs_incompleteness,
  var = "mpvs_total_16_1"
)
twin_pairs_incompleteness <- find_complete(
  twin_pairs_incompleteness,
  var = "mpvs_total_phase_2_21_1"
)
summary_twin_pairs_incompleteness <- twin_pairs_incompleteness %>%
  select(
    c(
      "zygosity_binary_fct",
      "pairs_flag_dcq_total_26_1",
      "pairs_flag_mpvs_total_12_1",
      "pairs_flag_mpvs_total_child_14_1",
      "pairs_flag_mpvs_total_16_1",
      "pairs_flag_mpvs_total_phase_2_21_1"
    )
  ) %>%
  gtsummary::tbl_summary(
    by = zygosity_binary_fct,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",
    label = list(
      pairs_flag_dcq_total_26_1 ~ "DCQ total score",
      pairs_flag_mpvs_total_12_1 ~ "MPVS 12y",
      pairs_flag_mpvs_total_child_14_1 ~ "MPVS 14y",
      pairs_flag_mpvs_total_16_1 ~ "MPVS 16y",
      pairs_flag_mpvs_total_phase_2_21_1 ~ "MPVS 21y"
    )
  ) %>%
  # Include an "overall" column
  add_overall(
    last = T,
    # The ** make it bold
    col_label = "**All participants**<br> \n N = {N}"
  ) %>%
  # This functions converts gtsummary to gt
  bstfun::bold_italicize_group_labels(bold = T) %>%
  gt::tab_header(
    title = "",
    subtitle = "Table 1. Participants characteristics"
  ) %>%
  gt::tab_options(heading.subtitle.font.size = "20px") %>%
  gt::tab_footnote(
    footnote = footnote_acronyms_2,
    locations = NULL
  )

summary_twin_pairs_incompleteness
