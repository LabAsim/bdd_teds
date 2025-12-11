adjust_edges <- function(edges_df, node_radius, reduction_parameter) {
  edges_df <- edges_df %>%
    rowwise() %>%
    mutate(
      dx = xend - x,
      dy = yend - y,
      length = sqrt(dx^2 + dy^2),
      # shrink by node radius (example: 0.3 units)
      xstart_adj = if (yend != y | curvature == 1) x + dx / length * node_radius else x + dx * reduction_parameter,
      ystart_adj = if (xend != x | curvature == 1) y + dy / length * node_radius else y + dy * reduction_parameter,
      xend_adj = if (yend != y | curvature == 1) xend - dx / length * node_radius else xend - dx * reduction_parameter,
      yend_adj = if (xend != x | curvature == 1) yend - dy / length * node_radius else yend - dy * reduction_parameter
    )
}


draw_dag <- function(edges, tidy_model, footnote = NULL) {
  to_return <- ggplot(data = edges) +
    # We use geom_label to wrap the text in rectangle box
    # See https://stackoverflow.com/a/44012702
    geom_label(
      data = tidy_model$data,
      aes(x = x, y = y, label = label),
      color = "black",
      size = 14, fontface = "bold",
      hjust = 0.5
    ) +
    geomtextpath::geom_textsegment(
      data = edges %>% filter(curvature != 1) %>%
        filter(pvalue <= 0.05),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj,
        label = paste0(
          "β=", if ("est" %in% colnames(edges)) est else est.std,
          "\n (", ci.lower, " — ", ci.upper, ")"
        )
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      # inherit.aes = F,
      hjust = 0.33,
      size = 11,
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
          "β=", if ("est" %in% colnames(edges)) est else est.std,
          "\n (", ci.lower, " — ", ci.upper, ")"
        )
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      # inherit.aes = F,
      hjust = 0.28,
      size = 11,
      angle = -190,
      linetype = 2
    ) +
    geomtextpath::geom_textcurve(
      data = (edges %>% filter(curvature == 1) %>%
        filter(pvalue > 0.05)),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj, label = paste0(
          "β=", if ("est" %in% colnames(edges)) est else est.std,
          "\n (", ci.lower, " — ", ci.upper, ")"
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
          "β=", if ("est" %in% colnames(edges)) est else est.std,
          "\n (", ci.lower, " — ", ci.upper, ")"
        )
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      curvature = -0.2,
      size = 11,
      hjust = 0.63
    ) +
    # coord_cartesian(xlim = c(-2, 14), ylim = c(0, 7.1)) +
    theme_dag()

  if (is.null(footnote) == F) {
    to_return <- to_return +
      labs(caption = footnote) +
      theme(plot.caption = element_text(hjust = 0.5, size = 30))
  }
}

save_dag <- function(path, plot) {
  png(
    path,
    width = 95,
    height = 50,
    units = "cm",
    res = 300
  )
  print(plot)
  dev.off()
}
