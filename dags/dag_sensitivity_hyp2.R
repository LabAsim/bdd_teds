library(ggplot2)
library(ggdag)
source("dags\\helper.R")
hyp2_modified <- ggdag::dagify(
  BDD ~ MPVS12, # The form is effect ~ cause
  BDD ~ MPVS14,
  BDD ~ MPVS16,
  BDD ~ MPVS21,
  MPVS14 ~ MPVS12,
  MPVS16 ~ MPVS14,
  MPVS21 ~ MPVS16,
  # Extra
  MPVS16 ~ MPVS12,
  MPVS21 ~ MPVS14,
  outcome = "BDD",
  coords = list(
    x = c(
      MPVS12 = -2,
      MPVS14 = 2,
      MPVS16 = 6,
      MPVS21 = 10,
      BDD = 14
    ),
    y = c(
      MPVS12 = 2,
      MPVS14 = 2,
      MPVS16 = 2,
      MPVS21 = 2,
      BDD = 1
    )
  ),
  labels = c(
    MPVS12 = "MPVS (12y)",
    MPVS14 = "MPVS (14y)",
    MPVS16 = "MPVS (16y)",
    MPVS21 = "MPVS (21y)",
    BDD = "DCQ (26y)"
  )
)

tidy_hyp2_modified <- tidy_dagitty(hyp2_modified)

#############################################
# Without eating disorders - unstandardised #
#############################################

edges <- tidy_hyp2_modified$data %>%
  filter(!is.na(to)) %>%
  mutate(
    curvature = case_when(
      name == "MPVS12" & to == "MPVS16" ~ 1,
      # name == "MPVS12" & to == "MPVS21" ~ 1,
      name == "MPVS14" & to == "MPVS21" ~ 1,
      name == "MPVS16" & to == "BDD" ~ 0,
      TRUE ~ 0 # default: straight
    ),
    estimate = round(runif(n(), -0.5, 0.5), 2)
  )

node_radius <- 0.8
# Calculate direction vector

edges <- adjust_edges(
  edges_df = edges,
  node_radius = node_radius,
  reduction_parameter = 0.2
)

edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_ΜΖ_sensitivity
    )[1:14, c("est", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

plot_fit_fiml_diff_without_covid_ΜΖ_sensitivity <- draw_dag(
  edges = edges, tidy_model = tidy_hyp2_modified,
  footnote = foonote_acronymns_for_dag_plots
) +
  coord_cartesian(xlim = c(-2, 14), ylim = c(1, 2.2))

save_dag(
  path = "img\\plot_fit_fiml_diff_without_covid_ΜΖ_sensitivity.png",
  plot = plot_fit_fiml_diff_without_covid_ΜΖ_sensitivity
)

#############################################
# Without eating disorders - Standardised #
#############################################

edges <- tidy_hyp2_modified$data %>%
  filter(!is.na(to)) %>%
  mutate(
    curvature = case_when(
      name == "MPVS12" & to == "MPVS16" ~ 1,
      # name == "MPVS12" & to == "MPVS21" ~ 1,
      name == "MPVS14" & to == "MPVS21" ~ 1,
      name == "MPVS16" & to == "BDD" ~ 0,
      TRUE ~ 0 # default: straight
    ),
    estimate = round(runif(n(), -0.5, 0.5), 2)
  )

node_radius <- 0.8
# Calculate direction vector

edges <- adjust_edges(
  edges_df = edges,
  node_radius = node_radius,
  reduction_parameter = 0.2
)

edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_ΜΖ_sensitivity_standardised
    )[1:14, c("est.std", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

plot_fit_fiml_diff_without_covid_standardized_ΜΖ_sensitivity <- draw_dag(
  edges = edges,
  tidy_model = tidy_hyp2_modified,
  footnote = foonote_acronymns_for_dag_plots
) +
  coord_cartesian(xlim = c(-2, 14), ylim = c(1, 2.2))

save_dag(
  path = "img\\plot_fit_fiml_diff_without_covid_standardized_ΜΖ_sensitivity.png",
  plot = plot_fit_fiml_diff_without_covid_standardized_ΜΖ_sensitivity
)
