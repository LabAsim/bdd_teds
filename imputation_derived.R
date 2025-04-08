library(mice)
library(miceadds)
library(micemd)

SEED <- 123
set.seed(seed = SEED)


impute_total_scaled_df <- function(){
  ######################
  # Impute variables   #
  ######################
  df_imp <- df_1 %>% 
    dplyr::select(
      #twin_id,
      fam_id,
      sex_1,
      age_12_1,
      age_14_1,
      age_16_1,
      age_21_1,
      age_26_1,
      mpvs_total_12_1_scaled_32,
      mpvs_total_14_1_scaled_32,
      mpvs_total_16_1_scaled_32,
      mpvs_total_21_scaled_32,
      dcq_26_1
    )
  
  predMatrix <- make.predictorMatrix(data = df_imp)
  impMethod <- make.method(data = df_imp)
  post <- make.post(data=df_imp)
  #####################################
  # method for variables at top level #
  #####################################
  impMethod["age_12_1"] <- "2lonly.norm"
  impMethod["age_14_1"] <- "2lonly.norm"
  impMethod["age_16_1"] <- "2lonly.norm"
  impMethod["age_21_1"] <- "2lonly.norm"
  impMethod["age_26_1"] <- "2lonly.norm"
  
  predMatrix[c("sex_1"),] <- 0
  
  # remove cluster indicator variables from predictor matrix
  predMatrix[, c("fam_id")] <- 0
  
  # ... specify cluster indicator (2lonly.norm)
  predMatrix["age_12_1", "fam_id"] <- -2
  predMatrix["age_14_1", "fam_id"] <- -2
  predMatrix["age_16_1", "fam_id"] <- -2
  predMatrix["age_21_1", "fam_id"] <- -2
  predMatrix["age_26_1", "fam_id"] <- -2
  
  level <- character(ncol(df_imp))
  names(level) <- colnames(df_imp)
  
  # specify cluster indicators (as list)
  scale_items <- c(colnames(df_imp)[grepl(pattern="mpvs",x=colnames(df_imp))])
  cluster <- vector(mode="list",length=length(scale_items))
  names(cluster) <- scale_items
  cluster <- lapply(cluster, function(x){"fam_id"}) # Specify the level two
  # cluster[["dcq_26_1"]] <- c("fam_id") # Specify the level two
  
  # https://stackoverflow.com/a/7219371
  # If mice throws error for the labels, uncomment the follow line
  df_imp <- labelled::remove_val_labels(df_imp)
  
  # Run in parallel
  print(parallelly::availableCores(logical = TRUE))
  startTime <- Sys.time() 
  imp <- futuremice(
    df_imp, method = impMethod, predictorMatrix = predMatrix, maxit = 30,
    m = 28, levels_id = cluster, variables_levels = level, parallelseed=SEED,
    post=post, n.core = (parallelly::availableCores(logical = TRUE) - 2)
  )
  data <- complete(imp)
  print(Sys.time() - startTime)
  summary(data)
  # Plots look good!
  plot(imp)
  
  return(data)
}



######################
# Impute variables   #
######################
df_imp <- df_1 %>% 
  dplyr::select(
    #twin_id,
    fam_id,
    sex_1,
    age_12_1,
    age_14_1,
    age_16_1,
    age_21_1,
    age_26_1,
    mpvs_total_12_1_scaled_32,
    mpvs_total_14_1_scaled_32,
    mpvs_total_16_1_scaled_32,
    mpvs_total_21_scaled_32,
    dcq_26_1
  )

predMatrix <- make.predictorMatrix(data = df_imp)
impMethod <- make.method(data = df_imp)
post <- make.post(data=df_imp)
#####################################
# method for variables at top level #
#####################################
impMethod["age_12_1"] <- "2lonly.norm"
impMethod["age_14_1"] <- "2lonly.norm"
impMethod["age_16_1"] <- "2lonly.norm"
impMethod["age_21_1"] <- "2lonly.norm"
impMethod["age_26_1"] <- "2lonly.norm"

predMatrix[c("sex_1"),] <- 0

# remove cluster indicator variables from predictor matrix
predMatrix[, c("fam_id")] <- 0

# ... specify cluster indicator (2lonly.norm)
predMatrix["age_12_1", "fam_id"] <- -2
predMatrix["age_14_1", "fam_id"] <- -2
predMatrix["age_16_1", "fam_id"] <- -2
predMatrix["age_21_1", "fam_id"] <- -2
predMatrix["age_26_1", "fam_id"] <- -2

level <- character(ncol(df_imp))
names(level) <- colnames(df_imp)

# specify cluster indicators (as list)
scale_items <- c(colnames(df_imp)[grepl(pattern="mpvs",x=colnames(df_imp))])
cluster <- vector(mode="list",length=length(scale_items))
names(cluster) <- scale_items
cluster <- lapply(cluster, function(x){"fam_id"}) # Specify the level two
# cluster[["dcq_26_1"]] <- c("fam_id") # Specify the level two

# https://stackoverflow.com/a/7219371
# If mice throws error for the labels, uncomment the follow line
df_imp <- labelled::remove_val_labels(df_imp)

# Run in parallel
print(parallelly::availableCores(logical = TRUE))
startTime <- Sys.time() 
imp <- futuremice(
  df_imp, method = impMethod, predictorMatrix = predMatrix, maxit = 30,
  m = 28, levels_id = cluster, variables_levels = level, parallelseed=SEED,
  post=post, n.core = (parallelly::availableCores(logical = TRUE) - 2)
)
data <- complete(imp)
print(Sys.time() - startTime)
summary(data)
# Plots look good!
plot(imp)
