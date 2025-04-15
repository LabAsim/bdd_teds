library(ggplot2)


longitudinal_means <- ggplot(
    data=summary_df, 
    aes(
      x=age, y=mean, color = informant,shape = informant
    )
  ) + geom_point() + geom_line() +
  labs(
    y = "Mean",
    x = "Age",
    title = "MPVS (original scale)"
  ) +
  theme_minimal() +
  theme(
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = c(seq(0, 8, 1)), limits = c(0, 8))+
  scale_x_continuous(breaks = c(seq(12, 22, 2)), limits = c(12, 22))


plot_mpvs <- function(df, var_to_plot, title, x_axis_title){
  inner_plot <- ggplot(
    data=df, 
    # See: https://stackoverflow.com/a/76568015
    # aes(x=!!ensym(var_to_plot))#.data[[!!var_to_plot]])
    
    # https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/#create-a-plotting-function
    aes(x=.data[[var_to_plot]])#.data[[!!var_to_plot]])
  ) + geom_density() +
    labs(
      y = "Density",
      x = x_axis_title,
      title = title #glue::glue("{var_to_plot}")
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(hjust = 0.5),
      plot.title = element_text(hjust = 0.5)
    )
  return(inner_plot)
}
var_vec <- colnames(df_1)[grepl(pattern="mpvs_total",x=colnames(df_1))]

plots <- purrr::map(
  var_vec, ~plot_mpvs(df=df_1,var_to_plot = .x, x_axis_title="", title=.x)
)


training_comb <- cowplot::plot_grid(
  plotlist=plots,
  ncol = 2
)


density_dcq <- ggplot(
  data=df_1, 
  # See: https://stackoverflow.com/a/76568015
  # aes(x=!!ensym(var_to_plot))#.data[[!!var_to_plot]])
  
  # https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/#create-a-plotting-function
  aes(x=dcq_total_26_1)#.data[[!!var_to_plot]])
) + geom_density() +
  labs(
    y = "Density",
    x = "",
    title = "DCQ" #glue::glue("{var_to_plot}")
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) 
