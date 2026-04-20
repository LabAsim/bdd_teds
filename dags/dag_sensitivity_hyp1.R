library(ggplot2)
library(ggdag)
source("dags\\helper.R")
hyp1_modified <- ggdag::dagify(
  BDD ~ MPVS12, # The form is effect ~ cause
  BDD ~ MPVS14,
  BDD ~ MPVS16,
  BDD ~ MPVS21,
  MPVS14 ~ MPVS12,
  MPVS16 ~ MPVS14,
  MPVS21 ~ MPVS16,
  BDD ~ Sex + Age26,
  MPVS12 ~ Sex + Age12,
  MPVS14 ~ Sex + Age14,
  MPVS16 ~ Sex + Age16,
  MPVS21 ~ Sex + Age21,
  MPVS16 ~ MPVS12,
  MPVS21 ~ MPVS12,
  MPVS21 ~ MPVS14,
  outcome = "BDD",
  coords = list(
    x = c(
      MPVS12 = 0,
      MPVS14 = 4,
      MPVS16 = 8,
      MPVS21 = 12,
      BDD = 14,
      Sex = 2,
      Age12 = 0,
      Age14 = 4,
      Age16 = 8,
      Age21 = 12,
      Age26 = 14
    ),
    y = c(
      MPVS12 = 4,
      MPVS14 = 4,
      MPVS16 = 4,
      MPVS21 = 4,
      BDD = 1,
      Sex = 1,
      Age12 = 7,
      Age14 = 7,
      Age16 = 7,
      Age21 = 7,
      Age26 = 7
    )
  ),
  labels = c(
    MPVS12 = "MPVS (12y)",
    MPVS14 = "MPVS (14y)",
    MPVS16 = "MPVS (16y)",
    MPVS21 = "MPVS (21y)",
    BDD = "DCQ (26y)",
    Sex = "Sex",
    Age12 = "Age (12y)",
    Age14 = "Age (14y)",
    Age16 = "Age (16y)",
    Age21 = "Age (21y)",
    Age26 = "Age (26y)"
  )
)

tidy_hyp1_modified <- tidy_dagitty(hyp1_modified)


nodes_hyp1 <- tidy_hyp1_modified$data %>%
  filter(!duplicated(name)) %>%
  transmute(
    node_id = name,
    x = x,
    y = y,
    label = label
  )


#############################################
# Without eating disorders - unstandardised #
#############################################

# Add curvature selectively
edges <- tidy_hyp1_modified$data %>%
  filter(!is.na(to)) %>%
  mutate(
    curvature = case_when(
      name == "MPVS12" & to == "MPVS16" ~ 1,
      name == "MPVS12" & to == "MPVS21" ~ 1,
      name == "MPVS14" & to == "MPVS21" ~ 1,
      name == "MPVS16" & to == "BDD" ~ 0,
      TRUE ~ 0 # default: straight
    ),
    estimate = round(runif(n(), -0.5, 0.5), 2)
  )


edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_scaled_32_without_covid_without_ED_modified_phenotypic
    )[1:20, c("est", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

edges <- edges %>%
  filter(!is.na(to)) %>%
  transmute(
    from = name,
    to = to,
    curvature = curvature,
    pvalue = pvalue,
    est = est,
    ci.lower = ci.lower,
    ci.upper = ci.upper
  )

edges <- edges %>%
  mutate(
    hjust = case_when(
      curvature == 1 & pvalue <= 0.05 ~ 0.6,
      curvature == 1 & pvalue > 0.05 ~ 0.5,
      pvalue <= 0.05 ~ 0.5,
      TRUE ~ 0.5,
    )
  ) %>%
  mutate(
    hjust = case_when(
      # Granular control over relationships
      from == "MPVS12" & to == "MPVS16" ~ 0.35,
      from == "MPVS12" & to == "MPVS21" ~ 0.5,
      from == "MPVS14" & to == "MPVS21" ~ 0.65,
      from == "Sex" & to == "MPVS21" ~ 0.4,
      from == "MPVS12" & to == "BDD" ~ 0.6,
      from == "MPVS14" & to == "BDD" ~ 0.6,
      from == "Age14" & to == "MPVS14" ~ 0.5,
      from == "Age16" & to == "MPVS16" ~ 0.5,
      .default = hjust
    )
  ) %>%
  mutate(
    vjust = case_when(
      from == "MPVS12" & to == "MPVS16" ~ 0.8,
      from == "MPVS14" & to == "MPVS21" ~ 0.8,
      from == "Sex" & to == "MPVS12" ~ 0.5,
      .default = 0.5
    )
  ) %>%
  mutate(
    vertical_label_position = case_when(
      from == "Age14" & to == "MPVS14" ~ 0.4,
      from == "Age16" & to == "MPVS16" ~ 0.4,
      .default = 0.5
    )
  )



p <- plot_dag(
  nodes = nodes_hyp1,
  edges = edges,
  label_size = 15 * 2,
  label_size_unit = "pt",
  text_size = 7,
  xlim = c(-0.3, 14.2),
  ylim = c(0.5, 7.5),
  footnote = foonote_acronymns_for_dag_plots,
  footnote_size = 12
)

save_dag(
  path = "img\\plot_fit_fiml_scaled_32_without_covid_without_ED.png",
  plot = p,
  width = 50,
  height = 25
)

###########################################
# Without eating disorders - Standardised #
###########################################


# Add curvature selectively
edges <- tidy_hyp1_modified$data %>%
  filter(!is.na(to)) %>%
  mutate(
    curvature = case_when(
      name == "MPVS12" & to == "MPVS16" ~ 1,
      name == "MPVS12" & to == "MPVS21" ~ 1,
      name == "MPVS14" & to == "MPVS21" ~ 1,
      name == "MPVS16" & to == "BDD" ~ 0,
      TRUE ~ 0 # default: straight
    ),
    estimate = round(runif(n(), -0.5, 0.5), 2)
  )


edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_scaled_32_without_covid_without_ED_modified_standardised
    )[1:20, c("est.std", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

edges <- edges %>%
  filter(!is.na(to)) %>%
  transmute(
    from = name,
    to = to,
    curvature = curvature,
    pvalue = pvalue,
    est = est.std,
    ci.lower = ci.lower,
    ci.upper = ci.upper
  )

edges <- edges %>%
  mutate(
    hjust = case_when(
      curvature == 1 & pvalue <= 0.05 ~ 0.6,
      curvature == 1 & pvalue > 0.05 ~ 0.5,
      pvalue <= 0.05 ~ 0.5,
      TRUE ~ 0.5,
    )
  ) %>%
  mutate(
    hjust = case_when(
      # Granular control over relationships
      from == "MPVS12" & to == "MPVS16" ~ 0.35,
      from == "MPVS12" & to == "MPVS21" ~ 0.5,
      from == "MPVS14" & to == "MPVS21" ~ 0.65,
      from == "Sex" & to == "MPVS21" ~ 0.4,
      from == "MPVS12" & to == "BDD" ~ 0.6,
      from == "MPVS14" & to == "BDD" ~ 0.6,
      from == "Age14" & to == "MPVS14" ~ 0.5,
      from == "Age16" & to == "MPVS16" ~ 0.5,
      .default = hjust
    )
  ) %>%
  mutate(
    vjust = case_when(
      from == "MPVS12" & to == "MPVS16" ~ 0.8,
      from == "MPVS14" & to == "MPVS21" ~ 0.8,
      from == "Sex" & to == "MPVS12" ~ 0.5,
      .default = 0.5
    )
  ) %>%
  mutate(
    vertical_label_position = case_when(
      from == "Age14" & to == "MPVS14" ~ 0.4,
      from == "Age16" & to == "MPVS16" ~ 0.4,
      .default = 0.5
    )
  )


p <- plot_dag(
  nodes = nodes_hyp1,
  edges = edges,
  label_size = 15 * 2,
  label_size_unit = "pt",
  text_size = 7,
  xlim = c(-0.3, 14.2),
  ylim = c(0.5, 7.5),
  footnote = foonote_acronymns_for_dag_plots,
  footnote_size = 12
)


save_dag(
  path = "img\\plot_fit_fiml_scaled_32_without_covid_without_ED_standardized.png",
  plot = plot_fit_fiml_scaled_32_without_covid_without_ED_standardized
)
