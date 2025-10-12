library(lavaan)
library(dagitty)
library(ggdag)


s1 <- lavaan::lavaanify(
  lavaan::lavParseModelString(twin_diff_model_scaled_modified)
)

s1 <- s1 %>% filter(
  user == 1
)

dag <- lavaanToGraph(s1, labels = var_labels)

coordinates(dag) <- list(
  x = c(
    sex_1_fct = 3,
    mpvs_total_12_1_scaled_32 = -2,
    mpvs_total_child_14_1_scaled_32 = -1,
    mpvs_total_16_1_scaled_32 = 0,
    mpvs_total_phase_2_21_1_scaled_32 = 1,
    dcq_total_26_1 = 0
  ),
  y = c(
    sex_1_fct = -3,
    mpvs_total_12_1_scaled_32 = -4,
    mpvs_total_child_14_1_scaled_32 = -3,
    mpvs_total_16_1_scaled_32 = -2,
    mpvs_total_phase_2_21_1_scaled_32 = -1,
    dcq_total_26_1 = 1
  )
)


# It's easier to view the plots separately
lapply(equivalentDAGs(dag), plot)

dag <- dag %>%
  tidy_dagitty() %>%
  dag_label(labels = var_labels)

dag$data$names <- unlist(dag$data$label)


plots <- dag %>% ggdag_equivalent_dags(
  text = FALSE,
  node_size = 22,
  # use_nodes   =F,
  edge_cap = 2,
  # If you want labels, uncomment
  # use_labels = "names"
) +
  # Overwrite the black point nodes
  geom_dag_point(color = "white", size = 22) +
  geom_dag_text(
    aes(
      x = .data$x, y = .data$y,
      label = .data$names
    ),
    size = 3,
    color = "black"
  ) + theme_dag()
