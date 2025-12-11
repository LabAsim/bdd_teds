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

node_radius <- 0.75
# Calculate direction vector
# edges <- edges %>%
#   rowwise() %>%
#   mutate(
#     dx = xend - x,
#     dy = yend - y,
#     length = sqrt(dx^2 + dy^2),
#     # shrink by node radius (example: 0.3 units)
#     xstart_adj = x + dx / length * node_radius,
#     ystart_adj = y + dy / length * node_radius,
#     xend_adj = xend - dx / length * node_radius,
#     yend_adj = yend - dy / length * node_radius
#   )
edges <- adjust_edges(
  edges_df = edges,
  node_radius = node_radius,
  reduction_parameter = 0.1
)

edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_phenotypic_modified_standardised
    )[1:20, c("est.std", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

# Plot using edge-specific curvature
fit_plot_scaled_32_without_covid_modified_standardized <- ggplot(data = edges) +
  # We use geom_label to wrap the text in rectangle box
  # See https://stackoverflow.com/a/44012702
  geom_label(
    data = tidy_hyp1_modified$data,
    aes(x = x, y = y, label = label),
    color = "black",
    size = 9, fontface = "bold",
    hjust = 0.5
  ) +
  # Otherwise, we could use a
  # geom_dag_node(
  #   data = tidy_hyp1_modified$data,
  #   aes(x = x, y = y),
  #   # see shapes
  #   # https://ggplot2.tidyverse.org/reference/scale_shape.html#details
  #   shape = 22,
  #   fill = "white", color = "black",
  #   size = 28
  # ) +
  # geom_dag_text(
  #   data = tidy_hyp1_modified$data,
  #   aes(x = x, y = y, label = label),
  #   color = "black",
  #   size = 3.5, fontface = "bold",
  #   hjust = 0.5
  # ) +
  geomtextpath::geom_textsegment(
    data = edges %>% filter(curvature != 1) %>%
      filter(pvalue <= 0.05),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj,
      label = paste0(
        "β=", est.std, "\n (", ci.lower, " — ", ci.upper, ")"
      )
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    # inherit.aes = F,
    hjust = 0.33,
    size = 8,
    angle = -190,
    linetype = 1
  ) +
  geomtextpath::geom_textsegment(
    data = edges %>% filter(curvature != 1) %>%
      filter(pvalue > 0.05),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj,
      label = paste0(
        "β=", est.std, "\n (", ci.lower, " — ", ci.upper, ")"
      )
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    # inherit.aes = F,
    hjust = 0.33,
    size = 8,
    angle = -190,
    linetype = 2
  ) +
  geomtextpath::geom_textcurve(
    data = (edges %>% filter(curvature == 1) %>%
      filter(pvalue > 0.05)),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj, label = paste0(
        "β=", est.std, "\n (", ci.lower, " — ", ci.upper, ")"
      )
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    curvature = -0.2,
    linetype = 2,
    size = 11,
    hjust = 0.63
  ) +
  geomtextpath::geom_textcurve(
    data = (edges %>% filter(curvature == 1) %>%
      filter(pvalue <= 0.05)),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj, label = paste0(
        "β=", est.std, "\n (", ci.lower, " — ", ci.upper, ")"
      )
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    curvature = -0.2,
    size = 11,
    hjust = 0.63
  ) +
  # coord_cartesian(xlim = c(-2, 14), ylim = c(0, 7.1)) +
  theme_dag()
fit_plot_scaled_32_without_covid_modified_standardized

fit_plot_scaled_32_without_covid_modified_standardized <- draw_dag(
  edges = edges, tidy_model = tidy_hyp1_modified,
  footnote = foonote_acronymns_for_dag_plots
)


# ggsave(
#   "img\\fit_plot_scaled_32_without_covid_modified_standardized.tiff",
#   width = 70,
#   height = 30,
#   units = "cm",
#   device = "tiff"
# )
#
#
# tiff(
#   "img\\fit_plot_scaled_32_without_covid_modified_standardized.tiff",
#   width = 70,
#   height = 30,
#   units = "cm",
#   res = 300
# )
# print(fit_plot_scaled_32_without_covid_modified_standardized)
# dev.off()
save_dag(
  path = "img\\fit_plot_scaled_32_without_covid_modified_standardized.png",
  plot = fit_plot_scaled_32_without_covid_modified_standardized
)


##################
# Unstandardised #
##################


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

node_radius <- 0.75

edges <- adjust_edges(
  edges_df = edges,
  node_radius = node_radius,
  reduction_parameter = 0.1
)

edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_modified_phenotypic
    )[1:20, c("est", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)


fit_plot_scaled_32_without_covid_modified <- draw_dag(
  edges = edges, tidy_model = tidy_hyp1_modified,
  footnote = foonote_acronymns_for_dag_plots
)

save_dag(
  path = "img\\fit_plot_scaled_32_without_covid_modified.png",
  plot = fit_plot_scaled_32_without_covid_modified
)
