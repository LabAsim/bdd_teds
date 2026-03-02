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
      MPVS12 = -2,
      MPVS14 = 2,
      MPVS16 = 6,
      MPVS21 = 10,
      BDD = 14,
      Sex = 2,
      Age12 = -2,
      Age14 = 2,
      Age16 = 6,
      Age21 = 10,
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

node_radius <- 0.80
reduction_parameter <- 0.2
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


edges <- adjust_edges(
  edges_df = edges,
  node_radius = node_radius,
  reduction_parameter = reduction_parameter
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


plot_fit_fiml_scaled_32_without_covid_without_ED <- draw_dag(
  edges = edges, tidy_model = tidy_hyp1_modified,
  footnote = foonote_acronymns_for_dag_plots
)

save_dag(
  path = "img\\plot_fit_fiml_scaled_32_without_covid_without_ED.png",
  plot = plot_fit_fiml_scaled_32_without_covid_without_ED
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


edges <- adjust_edges(
  edges_df = edges,
  node_radius = node_radius,
  reduction_parameter = reduction_parameter
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


plot_fit_fiml_scaled_32_without_covid_without_ED_standardized <- draw_dag(
  edges = edges, tidy_model = tidy_hyp1_modified,
  footnote = foonote_acronymns_for_dag_plots
)

save_dag(
  path = "img\\plot_fit_fiml_scaled_32_without_covid_without_ED_standardized.png",
  plot = plot_fit_fiml_scaled_32_without_covid_without_ED_standardized
)
