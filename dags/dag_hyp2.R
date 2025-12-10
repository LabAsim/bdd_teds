library(ggplot2)
library(ggdag)
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

# Add curvature selectively
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

node_radius <- 0.6
# Calculate direction vector
edges <- edges %>%
  rowwise() %>%
  mutate(
    dx = xend - x,
    dy = yend - y,
    length = sqrt(dx^2 + dy^2),
    # shrink by node radius (example: 0.3 units)
    xstart_adj = x + dx / length * node_radius,
    ystart_adj = y + dy / length * node_radius,
    xend_adj = xend - dx / length * node_radius,
    yend_adj = yend - dy / length * node_radius
  )

edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_MZ_standardised
    )[1:14, c("est.std", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

# Plot using edge-specific curvature
plot_fit_fiml_standardized <- ggplot(data = edges) +
  # We use geom_label to wrap the text in rectangle box
  # See https://stackoverflow.com/a/44012702
  geom_label(
    data = tidy_hyp2_modified$data,
    aes(x = x, y = y, label = label),
    color = "black",
    size = 9, fontface = "bold",
    hjust = 0.5
  ) +
  # Otherwise, we could use a
  # geom_dag_node(
  #   data = tidy_hyp2_modified$data,
  #   aes(x = x, y = y),
  #   # see shapes
  #   # https://ggplot2.tidyverse.org/reference/scale_shape.html#details
  #   shape = 22,
  #   fill = "white", color = "black",
  #   size = 28
  # ) +
  # geom_dag_text(
  #   data = tidy_hyp2_modified$data,
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
    data = (edges %>% filter(curvature == 1)),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj, label = paste0(
        "β=", est.std, "\n (", ci.lower, " — ", ci.upper, ")"
      )
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    curvature = -0.2,
    size = 8,
    hjust = 0.6
  ) +
  coord_cartesian(xlim = c(-2, 14), ylim = c(1, 2.2)) +
  theme_dag()
plot_fit_fiml_standardized


# ggsave(
#   "img\\plot_fit_fiml_standardized.tiff",
#   width = 70,
#   height = 30,
#   units = "cm",
#   device = "tiff"
# )
#
#
# tiff(
#   "img\\plot_fit_fiml_standardized.tiff",
#   width = 70,
#   height = 30,
#   units = "cm",
#   res = 300
# )
# print(plot_fit_fiml_standardized)
# dev.off()

png(
  "img\\plot_fit_fiml_standardized.png",
  width = 75,
  height = 40,
  units = "cm",
  res = 300
)
print(plot_fit_fiml_standardized)
dev.off()


##################
# Unstandardised #
##################


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

node_radius <- 0.6
# Calculate direction vector
edges <- edges %>%
  rowwise() %>%
  mutate(
    dx = xend - x,
    dy = yend - y,
    length = sqrt(dx^2 + dy^2),
    # shrink by node radius (example: 0.3 units)
    xstart_adj = x + dx / length * node_radius,
    ystart_adj = y + dy / length * node_radius,
    xend_adj = xend - dx / length * node_radius,
    yend_adj = yend - dy / length * node_radius
  )


edges <- left_join(
  x = edges,
  y = data.frame(
    add_labels_cols(
      parameters_fit_fiml_without_covid_modified_phenotypic
    )[1:14, c("est", "pvalue", "from", "to", "ci.lower", "ci.upper")]
  ),
  by = join_by(
    name == from,
    to == to
  )
)

# Plot using edge-specific curvature
plot_fit_fiml <- ggplot(data = edges) +
  # We use geom_label to wrap the text in rectangle box
  # See https://stackoverflow.com/a/44012702
  geom_label(
    data = tidy_hyp2_modified$data,
    aes(x = x, y = y, label = label),
    color = "black",
    size = 9, fontface = "bold",
    hjust = 0.5
  ) +
  geomtextpath::geom_textsegment(
    data = edges %>% filter(curvature != 1) %>%
      filter(pvalue <= 0.05),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj,
      label = paste0(
        "β=", est, "\n (", ci.lower, " — ", ci.upper, ")"
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
        "β=", est, "\n (", ci.lower, " — ", ci.upper, ")"
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
    data = (edges %>% filter(curvature == 1)),
    aes(
      x = xstart_adj, y = ystart_adj,
      xend = xend_adj, yend = yend_adj, label = paste0(
        "β=", est, "\n (", ci.lower, " — ", ci.upper, ")"
      )
    ),
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    curvature = -0.2,
    size = 8,
    hjust = 0.6
  ) +
  coord_cartesian(xlim = c(-2, 14), ylim = c(1, 2.2)) +
  theme_dag()
plot_fit_fiml

png(
  "img\\plot_fit_fiml.png",
  width = 75,
  height = 40,
  units = "cm",
  res = 300
)
print(plot_fit_fiml)
dev.off()
