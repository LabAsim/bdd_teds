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
      MPVS12 = 0,
      MPVS14 = 4,
      MPVS16 = 8,
      MPVS21 = 12,
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

################
# Standardised #
################
nodes_hyp2 <- tidy_hyp2_modified$data %>%
  filter(!duplicated(name)) %>%
  transmute(
    node_id = name,
    x = x,
    y = y,
    label = label
  )

edges_hyp2 <- tidy_hyp2_modified$data %>%
  mutate(
    curvature = case_when(
      name == "MPVS12" & to == "MPVS16" ~ 1,
      name == "MPVS14" & to == "MPVS21" ~ 1,
      name == "MPVS16" & to == "BDD" ~ 0,
      TRUE ~ 0 # default: straight
    ),
    estimate = round(runif(n(), -0.5, 0.5), 2)
  )

edges_hyp2 <- left_join(
  x = edges_hyp2,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_MZ_standardized
    )[1:14, c("est.std", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)


VERTICAL_LABEL_POSITION <- 0.3

edges_hyp2 <- edges_hyp2 %>%
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

edges_hyp2 <- edges_hyp2 %>%
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
      from == "MPVS12" & to == "MPVS16" ~ 0.5,
      .default = hjust
    )
  ) %>%
  mutate(
    vjust = case_when(
      from == "MPVS12" & to == "MPVS16" ~ 0.5,
      .default = 0.5
    )
  ) %>%
  mutate(
    vertical_label_position = case_when(
      from == "MPVS12" & to == "MPVS16" ~ 0.5,
      .default = 0.5
    )
  )


p <- plot_dag(
  nodes = nodes_hyp2,
  edges = edges_hyp2,
  label_size = 14 * 2,
  label_size_unit = "pt",
  text_size = 7,
  xlim = c(-0.5, 14.3),
  ylim = c(0.9, 2.2),
  footnote = foonote_acronymns_for_dag_plots,
  footnote_size = 14
)

save_dag(
  path = "img\\plot_fit_fiml_standardized.png",
  plot = p,
  width = 50,
  height = 25
)

##################
# Unstandardised #
##################
nodes_hyp2 <- tidy_hyp2_modified$data %>%
  filter(!duplicated(name)) %>%
  transmute(
    node_id = name,
    x = x,
    y = y,
    label = label
  )

edges_hyp2 <- tidy_hyp2_modified$data %>%
  mutate(
    curvature = case_when(
      name == "MPVS12" & to == "MPVS16" ~ 1,
      name == "MPVS14" & to == "MPVS21" ~ 1,
      name == "MPVS16" & to == "BDD" ~ 0,
      TRUE ~ 0 # default: straight
    ),
    estimate = round(runif(n(), -0.5, 0.5), 2)
  )

edges_hyp2 <- left_join(
  x = edges_hyp2,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_MZ
    )[1:14, c("est", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

edges_hyp2 <- edges_hyp2 %>%
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

edges_hyp2 <- edges_hyp2 %>%
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
      from == "MPVS12" & to == "MPVS16" ~ 0.5,
      .default = hjust
    )
  ) %>%
  mutate(
    vjust = case_when(
      from == "MPVS12" & to == "MPVS16" ~ 0.5,
      .default = 0.5
    )
  ) %>%
  mutate(
    vertical_label_position = case_when(
      from == "MPVS12" & to == "MPVS16" ~ 0.5,
      .default = 0.5
    )
  )


p <- plot_dag(
  nodes = nodes_hyp2,
  edges = edges_hyp2,
  label_size = 14 * 2,
  label_size_unit = "pt",
  text_size = 7,
  xlim = c(-0.5, 14.3),
  ylim = c(0.9, 2.2),
  footnote = foonote_acronymns_for_dag_plots,
  footnote_size = 14
)

save_dag(
  path = "img\\plot_fit_fiml.png",
  plot = p,
  width = 50,
  height = 25
)
