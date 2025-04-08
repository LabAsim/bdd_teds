library(mice)
library(miceadds)
library(micemd)
library(beepr)

SEED <- 123
set.seed(seed = SEED)

impute_items <- function(df,parallel=T, maxit=1, m=1,n.core=1){
  ######################
  # Impute scale items #
  ######################
  df_imp <- df_1 %>% 
    dplyr::select(
      twin_id,
      fam_id,
      sex_1,
      # age_12_1,
      age_parent_12,
      age_teach_12_1,
      age_child_12_1,
      # age_14_1,
      age_parent_14,
      age_teach_14_1,
      age_child_14_1,
      # age_16_1,
      age_web_16_1,
      age_child_16_1,
      age_parent_16,
      age_leap_study_parent_16,
      # age_21_1,
      age_phase1_parent_21,
      age_phase1_child_21_1,
      age_phase2_child_21_1,
      age_cov1_child_21_1,
      age_cov2_child_21_1,
      age_cov3_child_21_1,
      age_cov4_child_21_1,
      age_26_1,
      colnames(df)[grepl(pattern="item",x=colnames(df))]
    )
  df_imp$fam_id <- as.integer(df_imp$fam_id)
  predMatrix <- make.predictorMatrix(data = df_imp)
  impMethod <- make.method(data = df_imp)
  ####################################
  # method for lower-level variables #
  ####################################
  # mice.impute.poisson <- countimp::mice.impute.poisson
  for (
    var in colnames(df_imp)[grepl(pattern="mpvs_item",x=colnames(df_imp))]
  ){
    if (is.na(var)==F){
      impMethod[var] <- "pmm" 
      # Alternatively, use "poisson". However, note that it's memory intensive
      # and takes a lot of time
    }
  }
  
  #####################################
  # method for variables at top level #
  #####################################
  
  # remove cluster indicator variables from predictor matrix
  predMatrix[, c("fam_id")] <- 0 # Should not predict anything
  predMatrix [c("fam_id"),] <- 0 # Should not be predicted by anything
  
  for (
    var in colnames(df_imp)[grepl(pattern="age",x=colnames(df_imp))]
  ){
    print(var)
    if (is.na(var)==F){
      impMethod[var] <- "2lonly.norm"
      # specify cluster indicator (2lonly.norm)
      predMatrix[var, "fam_id"] <- -2
    }
  }
  
  predMatrix[c("sex_1"),] <- 0
  
  predMatrix[, c("twin_id")] <- 0
  predMatrix[c("twin_id"),] <- 0
  
  
  level <- character(ncol(df_imp))
  names(level) <- colnames(df_imp)
  
  # specify cluster indicators (as list)
  scale_items <- c(colnames(df_1)[grepl(pattern="item",x=colnames(df_1))])
  cluster <- vector(mode="list",length=length(scale_items))
  names(cluster) <- scale_items
  cluster <- lapply(cluster, function(x){"fam_id"}) # Specify the level two
  # cluster[["dcq_26_1"]] <- c("fam_id") # Specify the level two
    
  # Define post processing funcs
  post <- make.post(df_imp)
  post["mpvs_item_2_12_1"] <- "imp[[j]][data$mpvs_item_2_12_1[!r[, j]] > 2, i] <- as.numeric(2)"
  for (
    var in colnames(df_imp)[grepl(pattern="mpvs_item",x=colnames(df_imp))]
  ){
    if (is.na(var)==F){
      post[var] <- glue::glue("imp[[j]][data${var}[!r[, j]] > 2, i] <- as.numeric(2)")
    }
  }
  for (
    var in colnames(df_imp)[grepl(pattern="dcq_item",x=colnames(df_imp))]
  ){
    if (is.na(var)==F){
      post[var] <- glue::glue("imp[[j]][data${var}[!r[, j]] > 7, i] <- as.numeric(7)")
    }
  }
  # https://stackoverflow.com/a/7219371
  # If mice throws error for the labels, uncomment the follow line
  df_imp <- labelled::remove_val_labels(df_imp)
  

  # print(predMatrix)
  # View(predMatrix)
  # print(impMethod)
  # print(cluster)
  
  #########################
  # Remove collinear vars #
  #########################
  predMatrix["age_parent_12",c("age_teach_14_1", "age_child_14_1","age_parent_14","age_web_16_1")] <- 0
  predMatrix["age_child_12_1",c("age_teach_14_1", "age_child_14_1","age_parent_14","age_web_16_1")] <- 0
  
  for (var in 1:16){
    predMatrix[paste0("mpvs_item_",var,"_14_teacher_1"),colnames(df_imp)[grepl(pattern="21",x=colnames(df_imp))]] <- 0
  }
  predMatrix["age_teach_14_1",] <- 0
  predMatrix["age_teach_14_1","fam_id"] <- -2
  predMatrix["age_teach_14_1", c("sex_1", colnames(df_imp)[grepl(pattern="age",x=colnames(df_imp))])] <- 1
  predMatrix["age_teach_14_1","age_teach_14_1"] <- 0
  predMatrix["age_parent_16", c(colnames(df_imp)[grepl(pattern="16", x=colnames(df_imp))])] <- 0
  View(predMatrix)
  #############
  # Call mice #
  #############
  gc()
  startTime <- Sys.time()
  if (parallel == F){
    imp <- mice(
      df_imp, method = impMethod, predictorMatrix = predMatrix, maxit = maxit,
      m = m, levels_id = cluster, variables_levels = level, seed=SEED,
      post=post
    )
  }else if(parallel == T){
    # Run in parallel
    print(parallelly::availableCores(logical = TRUE))
    imp <- futuremice(
      df_imp, method = impMethod, predictorMatrix = predMatrix, maxit = maxit,
      m = m, levels_id = cluster, variables_levels = level, parallelseed=SEED,
      post=post, n.core = n.core,
      print=T
    )
    beep("mario")
    Sys.sleep(0.5)
  }
  print(Sys.time() - startTime)
  return(imp)
}


# imp <- impute_items(df=df_1, parallel=T, maxit=5, m=14, n.core=14)

imp <- impute_items(df=df_1,parallel=F, maxit=1, m=1)
data_imp <- complete(imp)
summary(data_imp)

summary(data_imp$mpvs_item_2_12_1)
data_imp[data_imp$mpvs_item_2_12_1>2,"mpvs_item_2_12_1"]

# Recalculate all total items
data_imp_calc <- data_imp %>%
  calculate_items(target_phrase = "_12_1", output_var = "mpvs_total_12_1") %>%
  calculate_items(target_phrase = "14_parent_1", output_var = "mpvs_total_14_parent_1") %>%
  calculate_items(target_phrase = "14_child_1", output_var = "mpvs_total_14_child_1") %>%
  calculate_items(target_phrase = "14_teacher_1", output_var = "mpvs_total_14_teacher_1") %>%
  calculate_items(target_phrase = "16_1", output_var = "mpvs_total_16_1")  %>%
  calculate_items(target_phrase = "_21_phase_2_1", output_var = "mpvs_total_21_phase_2_1")%>%
  calculate_items(target_phrase = "_21_cov1_2_1", output_var = "mpvs_total_21_cov1_1")%>%
  calculate_items(target_phrase = "_21_cov2_2_1", output_var = "mpvs_total_21_cov2_1")%>%
  calculate_items(target_phrase = "_21_cov3_2_1", output_var = "mpvs_total_21_cov3_1") %>%
  calculate_items(target_phrase = "_21_cov4_2_1", output_var = "mpvs_total_21_cov4_1")%>%
  calculate_items(target_phrase = "_26_1", output_var = "dcq_26_1")
