# See here https://rforhr.com/cronbachsalpha.html




cronbach_mpvs_12 <- psych::alpha(
  df_1[
    , colnames(df_1)[str_detect(
      string = colnames(df_1),
      pattern = "mpvs_item_[\\d]+_12_1"
    )]
  ]
)


cronbach_mpvs_14 <- psych::alpha(
  df_1[
    , colnames(df_1)[str_detect(
      string = colnames(df_1),
      pattern = "mpvs_item_[\\d]+_child_14_1"
    )]
  ]
)


cronbach_mpvs_16 <- psych::alpha(
  df_1[
    , colnames(df_1)[str_detect(
      string = colnames(df_1),
      pattern = "mpvs_item_[\\d]+_16_1"
    )]
  ]
)

cronbach_mpvs_21_phase2 <- psych::alpha(
  df_1[
    , colnames(df_1)[str_detect(
      string = colnames(df_1),
      pattern = "mpvs_item_[\\d]+_phase_2_21_1"
    )]
  ]
)


cronbach_dcq <- psych::alpha(
  df_1[
    , colnames(df_1)[str_detect(
      string = colnames(df_1),
      pattern = "dcq_item_[\\d]+_26_1"
    )]
  ]
)
