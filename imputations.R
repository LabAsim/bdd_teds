library(mice)
library(miceadds)
library(micemd)

SEED <- 123
set.seed(seed = SEED)


######################
# Impute scale items #
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
    colnames(df_1)[grepl(pattern="item",x=colnames(df_1))]
  )

predMatrix <- make.predictorMatrix(data = df_imp)
impMethod <- make.method(data = df_imp)
####################################
# method for lower-level variables #
####################################
mice.impute.poisson <- countimp::mice.impute.poisson
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

startTime <- Sys.time() 
imp <- mice(
  df_imp, method = impMethod, predictorMatrix = predMatrix, maxit = 1,
  m = 1, levels_id = cluster, variables_levels = level, seed=SEED,
  post=post
)
data <- complete(imp)
print(Sys.time() - startTime)
summary(data)

# Run in parallel
print(parallelly::availableCores(logical = TRUE))
startTime <- Sys.time() 
imp <- futuremice(
  df_imp, method = impMethod, predictorMatrix = predMatrix, maxit = 2,
  m = 14, levels_id = cluster, variables_levels = level, parallelseed=SEED,
  post=post, n.core = (parallelly::availableCores(logical = TRUE) - 2)
)
data <- complete(imp)
print(Sys.time() - startTime)
summary(data)

summary(data$mpvs_item_2_12_1)
data[data$mpvs_item_2_12_1>2,"mpvs_item_2_12_1"]
