adjust_edges <- function(edges_df, node_radius, reduction_parameter) {
  edges_df <- edges_df %>%
    rowwise() %>%
    mutate(
      dx = xend - x,
      dy = yend - y,
      length = sqrt(dx^2 + dy^2),
      # shrink by node radius (example: 0.3 units)
      xstart_adj = if (curvature == 1) x + dx / length * node_radius else x + dx * reduction_parameter,
      ystart_adj = if (curvature == 1) y + dy / length * node_radius else y + dy * reduction_parameter,

      # x=xend then the line is vertical
      xend_adj = if (curvature == 1) xend - dx / length * node_radius else xend - dx * reduction_parameter,

      # If yend=y
      yend_adj = if (curvature == 1) yend - dy / length * node_radius else yend - dy * reduction_parameter,

      # xstart_adj = if (yend != y | curvature == 1) x + dx / length * node_radius else x + dx * reduction_parameter,
      # ystart_adj = if (xend != x | curvature == 1) y + dy / length * node_radius else y + dy * reduction_parameter,
      #
      # # x=xend then the line is vertical
      # xend_adj = if (yend != y | curvature == 1) xend - dx / length * node_radius else xend - dx * reduction_parameter,
      #
      # # If yend=y
      # yend_adj = if (xend != x | curvature == 1) yend - dy / length * node_radius else yend - dy * reduction_parameter
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
      size = 16.9, fontface = "bold",
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
      size = 13,
      # angle = -100,
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
      size = 13,
      # angle = -190,
      linetype = 2,
      # Larger dashes, larger gaps
      linewidth = 1.5
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
      linewidth = 1.5,
      size = 13,
      hjust = 0.50
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
      size = 13,
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

save_dag <- function(path, plot, width = 50, height = 25) {
  png(
    path,
    width = width,
    height = height,
    units = "cm",
    res = 300
  )
  print(plot)
  dev.off()
}


# -------------------------------------------------
# Measure label box sizes and convert to data units
# -------------------------------------------------
compute_node_boxes <- function(
    nodes,
    label_col = "label",
    text_size = 16.9,
    size.unit = "pt",
    fontface = "bold",
    padding_cm = 0.3,
    xlim,
    ylim,
    plot_width_cm = 20,
    plot_height_cm = 12) {
  fontsize_pt <- if (size.unit == "pt") {
    text_size
  } else if (size.unit == "mm") {
    text_size / .pt
  } else {
    stop("Only 'pt' and 'mm' are supported.")
  }

  dims <- lapply(nodes[[label_col]], function(lbl) {
    tg <- grid::textGrob(
      lbl,
      gp = grid::gpar(fontsize = fontsize_pt, fontface = fontface)
    )

    text_w_cm <- grid::convertWidth(grid::grobWidth(tg), "cm", valueOnly = TRUE)
    text_h_cm <- grid::convertHeight(grid::grobHeight(tg), "cm", valueOnly = TRUE)

    data.frame(
      box_w_cm = text_w_cm + 2 * padding_cm,
      box_h_cm = text_h_cm + 2 * padding_cm
    )
  })

  dims <- bind_rows(dims)

  x_data_per_cm <- diff(xlim) / plot_width_cm
  y_data_per_cm <- diff(ylim) / plot_height_cm

  nodes %>%
    bind_cols(dims) %>%
    mutate(
      half_w = (box_w_cm * x_data_per_cm) / 2,
      half_h = (box_h_cm * y_data_per_cm) / 2
    )
}

line_rect_intersection <- function(x0, y0, x1, y1, cx, cy, half_w, half_h) {
  # Rectangle bounds
  xmin <- cx - half_w
  xmax <- cx + half_w
  ymin <- cy - half_h
  ymax <- cy + half_h

  # Line directions
  dx <- x1 - x0
  dy <- y1 - y0

  candidates <- list()

  # Intersect with vertical sides: x = xmin, x = xmax
  if (dx != 0) {
    # Left side
    t_left <- (xmin - x0) / dx
    y_left <- y0 + t_left * dy
    if (y_left >= ymin && y_left <= ymax) {
      candidates[[length(candidates) + 1]] <- c(x = xmin, y = y_left)
    }

    # Right side
    t_right <- (xmax - x0) / dx
    y_right <- y0 + t_right * dy
    if (y_right >= ymin && y_right <= ymax) {
      candidates[[length(candidates) + 1]] <- c(x = xmax, y = y_right)
    }
  }

  # Intersect with horizontal sides: y = ymin, y = ymax
  if (dy != 0) {
    # Bottom
    t_bottom <- (ymin - y0) / dy
    x_bottom <- x0 + t_bottom * dx
    if (x_bottom >= xmin && x_bottom <= xmax) {
      candidates[[length(candidates) + 1]] <- c(x = x_bottom, y = ymin)
    }

    # Top
    t_top <- (ymax - y0) / dy
    x_top <- x0 + t_top * dx
    if (x_top >= xmin && x_top <= xmax) {
      candidates[[length(candidates) + 1]] <- c(x = x_top, y = ymax)
    }
  }

  # If no intersections found (shouldn’t really happen), return center
  if (length(candidates) == 0) {
    return(c(x = cx, y = cy))
  }

  # Pick the intersection closest to (x0, y0)
  dists <- sapply(candidates, function(pt) {
    (pt["x"] - x0)^2 + (pt["y"] - y0)^2
  })

  candidates[[which.min(dists)]]
}


# -------------------------------------------------
# 5. Adjust edge start and end so they touch the box border
# -------------------------------------------------
adjust_edges_by_box <- function(edges_df, nodes_df) {
  out <- edges_df %>%
    left_join(
      nodes_df %>%
        select(node_id, x, y, half_w, half_h) %>%
        rename(
          x_from = x,
          y_from = y,
          half_w_from = half_w,
          half_h_from = half_h
        ),
      by = c("from" = "node_id")
    ) %>%
    left_join(
      nodes_df %>%
        select(node_id, x, y, half_w, half_h) %>%
        rename(
          x_to = x,
          y_to = y,
          half_w_to = half_w,
          half_h_to = half_h
        ),
      by = c("to" = "node_id")
    )

  start_pts <- t(mapply(
    FUN = line_rect_intersection,
    x0 = out$x_to,
    y0 = out$y_to,
    x1 = out$x_from,
    y1 = out$y_from,
    cx = out$x_from,
    cy = out$y_from,
    half_w = out$half_w_from,
    half_h = out$half_h_from
  ))

  end_pts <- t(mapply(
    FUN = line_rect_intersection,
    x0 = out$x_from,
    y0 = out$y_from,
    x1 = out$x_to,
    y1 = out$y_to,
    cx = out$x_to,
    cy = out$y_to,
    half_w = out$half_w_to,
    half_h = out$half_h_to
  ))

  out <- out %>%
    mutate(
      xstart_adj = start_pts[, "x"],
      ystart_adj = start_pts[, "y"],
      xend_adj   = end_pts[, "x"],
      yend_adj   = end_pts[, "y"]
    )

  # Find horizontal and vertical lines
  out <- out %>%
    mutate(
      is_horizontal = abs(y_from - y_to) < 1e-6,
      is_vertical = abs(x_from - x_to) < 1e-6
    )

  horizontal <- out %>%
    filter(curvature != 1 & is_horizontal == T) %>%
    mutate(
      # In this way the horizontal
      # will much closer to the starting/ending boxes
      xstart_adj = x_from + half_w_from * 0.5, #
      ystart_adj = y_from, # Don't mess with y
      xend_adj   = x_to - half_w_to * 0.5,
      yend_adj   = y_to
    )

  rest_straight_lines <- out %>% filter(curvature != 1 & is_horizontal == F)

  curved <- out %>%
    filter(curvature == 1) %>%
    mutate(
      xstart_adj = x_from + half_w_from / 4, # A small offset
      ystart_adj = y_from + half_h_from,
      xend_adj   = x_to - half_w_from / 4,
      yend_adj   = y_to + half_h_to
    )

  # recombine
  out <- bind_rows(rest_straight_lines, horizontal, curved)
  # View(out)
  return(out)
}

plot_dag <- function(
    nodes,
    edges,
    label_size = 16.9,
    label_size_unit = "pt",
    label_border_size = 1,
    text_size = 7,
    xlim = c(0, 14),
    ylim = c(0, 8),
    plot_width_cm = 20,
    plot_height_cm = 12,
    footnote = NULL,
    footnote_size = 12) {
  nodes_boxes <- compute_node_boxes(
    nodes = nodes,
    label_col = "label",
    text_size = label_size,
    size.unit = label_size_unit,
    fontface = "bold",
    padding_cm = 0.3,
    xlim = xlim,
    ylim = ylim,
    plot_width_cm = plot_width_cm,
    plot_height_cm = plot_height_cm
  )
  edges_adj <- adjust_edges_by_box(edges, nodes_boxes)

  to_return <- ggplot() +
    geom_label(
      data = nodes_boxes,
      aes(x = x, y = y, label = label),
      size = label_size,
      label.size = label_border_size,
      size.unit = label_size_unit,
      fontface = "bold"
    ) +
    geomtextpath::geom_textsegment(
      data = edges_adj %>%
        filter(curvature != 1, pvalue <= 0.05),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj,
        label = paste0(
          est, # "β=",
          "\n(", ci.lower, " — ", ci.upper, ")"
        ),
        # Adjust it dynamically, not fixed for every line
        hjust = hjust,
        vjust = vjust
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 1,
      size = text_size
    ) +
    geomtextpath::geom_textsegment(
      data = edges_adj %>%
        filter(curvature != 1, pvalue > 0.05, !is_vertical),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj,
        label = paste0(
          est, # "β=",
          "\n(", ci.lower, " — ", ci.upper, ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1.25,
      linetype = 2,
      size = text_size
    ) +
    # Vertical lines are drawn separately
    geom_segment(
      data = edges_adj %>% filter(curvature != 1, pvalue > 0.05, is_vertical),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj,
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 1
    ) +
    geom_label(
      data = edges_adj %>%
        filter(curvature != 1, pvalue > 0.05, is_vertical),
      aes(
        x = xstart_adj + (xend_adj - xstart_adj) * vertical_label_position,
        y = ystart_adj + (yend_adj - ystart_adj) * vertical_label_position,
        label = paste0(
          est,
          "\n(", ci.lower, " — ", ci.upper, ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      angle = 0,
      label.size = 0, # removes border line
      fill = "white", # background color
      size = text_size
    ) +
    geomtextpath::geom_textcurve(
      data = edges_adj %>% filter(curvature == 1, pvalue > 0.05),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj,
        label = paste0(
          est, # "β=",
          "\n(", ci.lower, " — ", ci.upper, ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      curvature = -0.15,
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1.5,
      linetype = 2,
      size = text_size
    ) +
    geomtextpath::geom_textcurve(
      data = edges_adj %>% filter(curvature == 1, pvalue <= 0.05),
      aes(
        x = xstart_adj, y = ystart_adj,
        xend = xend_adj, yend = yend_adj,
        label = paste0(
          est, # "β=",
          "\n(", ci.lower, " — ", ci.upper, ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      curvature = -0.1,
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 1,
      size = text_size
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_void()

  if (is.null(footnote) == F) {
    to_return <- to_return +
      labs(caption = footnote) +
      theme(plot.caption = element_text(hjust = 0.5, size = footnote_size))
  }
  return(to_return)
}
