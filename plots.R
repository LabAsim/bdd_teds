library(ggplot2)


density_mpvs_scaled_32 <- ggplot(
  data=df_all_diffs_long,
  aes(x=mpvs)
) + 
  geom_density(aes(color=mpvs_age,group = mpvs_age)) + 
  labs(
    y = "Density",
    x = "ΜΖ difference in MPVS score",
    title = "ΜΖ difference in MPVS (scaled at 32 items)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) 
# density_mpvs_scaled_32

density_dcq_diff <- ggplot(
  data=df_all_diffs_long,
  aes(x=dcq_total_26_1)
) + 
  geom_density() + 
  labs(
    y = "Density",
    x = "ΜΖ difference in DCQ score",
    title = "DCQ diff"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) 
# density_dcq

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


longitudinal_means_scaled_without_NA_21 <- ggplot(
  data=summary_df_without_NA_21, 
  aes(
    x=age, y=mean, color = informant,shape = informant
  )
) + geom_point() + geom_line() +
  labs(
    y = "Mean",
    x = "Age",
    title = "MPVS (scaled at maximum of 32-points)"
  ) +
  theme_minimal() +
  theme(
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) 


longitudinal_means_scaled_without_NA <- ggplot(
  data=summary_df_without_NA, 
  aes(
    x=age, y=mean, color = informant,shape = informant
  )
) + geom_point() + geom_line() +
  labs(
    y = "Mean",
    x = "Age",
    title = "MPVS (scaled at maximum of 32-points)"
  ) +
  theme_minimal() +
  theme(
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_text(hjust = 0.5),
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = c(seq(0, 10, 1)), limits = c(0, 10))+
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


var_vec <- colnames(df_essential_vars)[grepl(pattern="mpvs_total",x=colnames(df_essential_vars))]


plots2 <- purrr::map(
  var_vec[grepl(pattern="scaled_32",x=var_vec)], 
  ~plot_mpvs(df=df_essential_vars,var_to_plot = .x, x_axis_title="", title=.x)
)


training_comb2 <- cowplot::plot_grid(
  plotlist=plots2,
  ncol = 2
)

##############
# MPVS items #
##############

df_21_phase_2 <- df_1 %>% 
  mutate(
    punched_me = mpvs_item_1_phase_2_21_1,
    get_into_trouble = mpvs_item_2_phase_2_21_1,
    called_me_names = mpvs_item_3_phase_2_21_1,
    nasty_texts = mpvs_item_4_phase_2_21_1,
    kicked_me = mpvs_item_5_phase_2_21_1,
    turn_friend_against_me = mpvs_item_6_phase_2_21_1,
    fun_of_me_appearance = mpvs_item_7_phase_2_21_1,
    mean_on_social_media = mpvs_item_8_phase_2_21_1,
    hurt_me_physically = mpvs_item_9_phase_2_21_1,
    resufed_talk =  mpvs_item_10_phase_2_21_1,
    fun_of_me = mpvs_item_11_phase_2_21_1,
    spiteful_in_chatroom = mpvs_item_12_phase_2_21_1,
    beaten_me_up = mpvs_item_13_phase_2_21_1,
    made_people_not_talk = mpvs_item_14_phase_2_21_1,
    sworn_at_me = mpvs_item_15_phase_2_21_1,
    nasty_things_instant_messenger = mpvs_item_16_phase_2_21_1
  )

df_21_phase_2$wave <- "phase_2"

df_21_cov1 <- df_1 %>% 
  mutate(
    punched_me = mpvs_item_1_cov1_21_1,
    get_into_trouble = NA,
    called_me_names = mpvs_item_2_cov1_21_1,
    nasty_texts = mpvs_item_3_cov1_21_1,
    kicked_me = mpvs_item_4_cov1_21_1,
    turn_friend_against_me = NA,
    fun_of_me_appearance = mpvs_item_5_cov1_21_1,
    mean_on_social_media = mpvs_item_6_cov1_21_1,
    hurt_me_physically = mpvs_item_7_cov1_21_1,
    resufed_talk =  NA,
    fun_of_me = mpvs_item_8_cov1_21_1,
    spiteful_in_chatroom = mpvs_item_9_cov1_21_1,
    beaten_me_up = mpvs_item_10_cov1_21_1,
    made_people_not_talk = NA,
    sworn_at_me = mpvs_item_11_cov1_21_1,
    nasty_things_instant_messenger = mpvs_item_12_cov1_21_1
  )

df_21_cov1$wave <- "cov1"

df_21_cov2 <- df_1 %>% 
  mutate(
    punched_me = mpvs_item_1_cov2_21_1,
    get_into_trouble = NA,
    called_me_names = mpvs_item_2_cov2_21_1,
    nasty_texts = mpvs_item_3_cov2_21_1,
    kicked_me = mpvs_item_4_cov2_21_1,
    turn_friend_against_me = NA,
    fun_of_me_appearance = mpvs_item_5_cov2_21_1,
    mean_on_social_media = mpvs_item_6_cov2_21_1,
    hurt_me_physically = mpvs_item_7_cov2_21_1,
    resufed_talk =  NA,
    fun_of_me = mpvs_item_8_cov2_21_1,
    spiteful_in_chatroom = mpvs_item_9_cov2_21_1,
    beaten_me_up = mpvs_item_10_cov2_21_1,
    made_people_not_talk = NA,
    sworn_at_me = mpvs_item_11_cov2_21_1,
    nasty_things_instant_messenger = mpvs_item_12_cov2_21_1
  )
df_21_cov2$wave <- "cov2"

df_21_cov3 <- df_1 %>% 
  mutate(
    punched_me = mpvs_item_1_cov3_21_1,
    get_into_trouble = NA,
    called_me_names = mpvs_item_2_cov3_21_1,
    nasty_texts = mpvs_item_3_cov3_21_1,
    kicked_me = mpvs_item_4_cov3_21_1,
    turn_friend_against_me = NA,
    fun_of_me_appearance = mpvs_item_5_cov3_21_1,
    mean_on_social_media = mpvs_item_6_cov3_21_1,
    hurt_me_physically = mpvs_item_7_cov3_21_1,
    resufed_talk =  NA,
    fun_of_me = mpvs_item_8_cov3_21_1,
    spiteful_in_chatroom = mpvs_item_9_cov3_21_1,
    beaten_me_up = mpvs_item_10_cov3_21_1,
    made_people_not_talk = NA,
    sworn_at_me = mpvs_item_11_cov3_21_1,
    nasty_things_instant_messenger = mpvs_item_12_cov3_21_1
  )

df_21_cov3$wave <- "cov3"
df_21_cov4 <- df_1 %>% 
  mutate(
    punched_me = mpvs_item_1_cov4_21_1,
    get_into_trouble = NA,
    called_me_names = mpvs_item_2_cov4_21_1,
    nasty_texts = mpvs_item_3_cov4_21_1,
    kicked_me = mpvs_item_4_cov4_21_1,
    turn_friend_against_me = NA,
    fun_of_me_appearance = mpvs_item_5_cov4_21_1,
    mean_on_social_media = mpvs_item_6_cov4_21_1,
    hurt_me_physically = mpvs_item_7_cov4_21_1,
    resufed_talk =  NA,
    fun_of_me = mpvs_item_8_cov4_21_1,
    spiteful_in_chatroom = mpvs_item_9_cov4_21_1,
    beaten_me_up = mpvs_item_10_cov4_21_1,
    made_people_not_talk = NA,
    sworn_at_me = mpvs_item_11_cov4_21_1,
    nasty_things_instant_messenger = mpvs_item_12_cov4_21_1
  )

df_21_cov4$wave <- "cov4"
df_items <- rbind(
  df_21_phase_2,
  df_21_cov1,
  df_21_cov2,
  df_21_cov3,
  df_21_cov4
)

vars <- c(
  "punched_me",
  "get_into_trouble",
  "called_me_names",
  "nasty_texts" ,
  "kicked_me" ,
  "turn_friend_against_me" ,
  "fun_of_me_appearance" ,
  "mean_on_social_media",
  "hurt_me_physically",
  "resufed_talk",
  "fun_of_me",
  "spiteful_in_chatroom",
  "beaten_me_up",
  "made_people_not_talk" ,
  "sworn_at_me" ,
  "nasty_things_instant_messenger" 
)

create_summary_df <- function(df, var_vec){
  inner_df <- data.frame(
    mean = NA,
    sd = NA,
    median = NA,
    var = NA,
    wave = NA,
    N = NA
  )
  for (num in 1:length(var_vec)){
    mean_ <- mean(df[,var_vec[num]], na.rm = T)
    median_ <- median(df[,var_vec[num]], na.rm = T)
    sd_ <- sd(df[,var_vec[num]],na.rm=T)
    inner_df[num,] <- c(
      mean_,median_,sd_, var_vec[num],df[1,"wave"], 
      sum(!is.na(df[,var_vec[num]]))
    )
  }
  inner_df[,c("mean", "median", "sd","N")] <- lapply(
    X=inner_df[,c("mean", "median", "sd","N")], 
    FUN=as.numeric
  )
  inner_df[,c("var", "wave")] <- lapply(
    X=inner_df[,c("var", "wave")],
    FUN=as.factor
  )
  return(inner_df)
}

summary_items <- rbind(
  create_summary_df(df=df_21_phase_2, var_vec=vars),
  create_summary_df(df=df_21_cov1, var_vec=vars),
  create_summary_df(df=df_21_cov2, var_vec=vars),
  create_summary_df(df=df_21_cov3, var_vec=vars),
  create_summary_df(df=df_21_cov4, var_vec=vars)
)


longitudinal_means_items <- ggplot(
  data=summary_items, 
  aes(
    x=wave, y=mean, color = var, group=var
  )
) + 
  geom_point() + 
  geom_line(alpha=0.3) + 
  ggrepel::geom_text_repel(
    aes(label = N),
    seed = 123
  )

create_summary_pct_change_df2 <- function(summary_df){
  waves <- levels(summary_df$wave)
  df_to_return <-- data.frame(
      mean_diff = NA,
      mean_diff_pct = NA,
      median_diff = NA,
      median_diff_pct = NA,
      var = NA,
      wave = NA,
      N_diff = NA
    )
  for (level in waves[-1]){
    for (item in summary_df$var){
      # Add 1 row
      df_to_return[nrow(df_to_return)+1,"mean_diff"] <- summary_df[summary_df$wave==level & summary_df$var == item,"mean"] - summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"mean"]
      df_to_return[nrow(df_to_return),"mean_diff_pct"] <- (summary_df[summary_df$wave==level & summary_df$var == item,"mean"] - summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"mean"])*100/summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"mean"]
      df_to_return[nrow(df_to_return),"median_diff"] <- summary_df[summary_df$wave==level & summary_df$var == item,"median"] - summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"median"]
      df_to_return[nrow(df_to_return),"median_diff_pct"] <- (summary_df[summary_df$wave==level & summary_df$var == item,"median"] - summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"median"])*100/summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"median"]
      df_to_return[nrow(df_to_return),"var"] <- item
      df_to_return[nrow(df_to_return),"wave"] <- glue::glue("phase_2_{level}")
      df_to_return[nrow(df_to_return),"N_diff"] <- summary_df[summary_df$wave==level & summary_df$var == item,"N"] - summary_df[summary_df$wave=="phase_2" & summary_df$var == item,"N"]
      
    }
  }
  return(df_to_return)
}

create_summary_pct_change_df <- function(summary_df){
  waves <- levels(summary_df$wave)
  df_to_return <- data.frame(
    mean_diff = NA,
    mean_diff_pct = NA,
    median_diff = NA,
    median_diff_pct = NA,
    var = NA,
    wave = NA,
    N_diff = NA
  )
  for (num in 1:length(waves[1:4])){
    for (item in levels(summary_df$var)){
      # Add 1 row
      df_to_return[nrow(df_to_return)+1,"mean_diff"] <- summary_df[summary_df$wave==waves[num+1] & summary_df$var == item,"mean"] - summary_df[summary_df$wave==waves[num] & summary_df$var == item,"mean"]
      df_to_return[nrow(df_to_return),"mean_diff_pct"] <- (summary_df[summary_df$wave==waves[num+1] & summary_df$var == item,"mean"] - summary_df[summary_df$wave==waves[num] & summary_df$var == item,"mean"])*100/summary_df[summary_df$wave==waves[num] & summary_df$var == item,"mean"]
      df_to_return[nrow(df_to_return),"median_diff"] <- summary_df[summary_df$wave==waves[num+1] & summary_df$var == item,"median"] - summary_df[summary_df$wave==waves[num] & summary_df$var == item,"median"]
      df_to_return[nrow(df_to_return),"median_diff_pct"] <- (summary_df[summary_df$wave==waves[num+1] & summary_df$var == item,"median"] - summary_df[summary_df$wave==waves[num] & summary_df$var == item,"median"])*100/summary_df[summary_df$wave==waves[num] & summary_df$var == item,"median"]
      df_to_return[nrow(df_to_return),"var"] <- item
      df_to_return[nrow(df_to_return),"wave"] <- glue::glue("{waves[num+1]}_{waves[num]}")
      df_to_return[nrow(df_to_return),"N_diff"] <- summary_df[summary_df$wave==waves[num+1] & summary_df$var == item,"N"] - summary_df[summary_df$wave==waves[num] & summary_df$var == item,"N"]
      
    }
  }
  df_to_return <- df_to_return %>% drop_na()
  df_to_return[,c("mean_diff", "mean_diff_pct", "median_diff","median_diff_pct", "N_diff")] <- lapply(
    X=df_to_return[,c("mean_diff", "mean_diff_pct", "median_diff","median_diff_pct", "N_diff")], 
    FUN=as.numeric
  )
  df_to_return[,c("var", "wave")] <- lapply(
    X=df_to_return[,c("var", "wave")],
    FUN=as.factor
  )
  return(df_to_return)
}

summary_items_change <- create_summary_pct_change_df(summary_df = summary_items)
summary_items_change <- summary_items_change %>% drop_na()

longitudinal_means_pct_items_facet <- ggplot(
  data=summary_items_change, 
  aes(
    x=wave, y=mean_diff_pct, color = var, group=var
  )
) + 
  geom_point() + 
  geom_line(alpha=0.3) +
  labs(
    y = "Percentage change in mean (%)",
    x = "Pair of waves",
    title = "MPVS individual items"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) + facet_wrap(vars(var))
longitudinal_means_pct_items_facet

filt <- summary_items_change %>% filter(wave=="cov4_cov3")
longitudinal_means_pct_items + 
  ggrepel::geom_text_repel(
    data=summary_items_change %>% filter(wave=="cov2_cov1"),
    aes(label = var),
    seed = 123
  )

longitudinal_means_pct_items1 <- ggplot(
  data=summary_items_change[summary_items_change$wave=="cov1_phase_2",], 
  aes(
    x=wave, y=mean_diff_pct, color = var, group=var
  )
) + 
  geom_point() + 
  geom_line(alpha=0.3) +
  labs(
    y = "Percentage change in mean (%)",
    x = "Pair of waves",
    title = "MPVS individual items"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  ) + 
  ggrepel::geom_text_repel(
    aes(label = var),
    seed = 123
  )
longitudinal_means_pct_items1

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


