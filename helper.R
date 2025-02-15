library(tidyverse)

test <- df

fill_single_var_twin_from_cotwin2 <- function(df, var){
  calculate <- data.frame(f_id=1)
  for (f_id in unique(getElement(df, "fam_id"))){
    twin_df <- df[getElement(df, "fam_id")==f_id,]
    if (
      # is.numeric returns T even though it's NA
      # See: https://statisticaloddsandends.wordpress.com/2019/04/09/testing-numeric-variables-for-na-nan-inf/
      (is.na(twin_df[1,var]) == T & !is.na(twin_df[2,var]) == T) | 
      (!is.na(twin_df[1,var])== T & is.na(twin_df[2,var])== T)
    ){
      if (is.na(twin_df[1,var]) == T){
        twin_df[1,var] <- twin_df[2,var]
      }else{
        twin_df[2,var] <- twin_df[1,var]
      }
      df[getElement(df, "fam_id")== f_id &  getElement(df, "twin_id")==twin_df[1,"twin_id"],var] <- twin_df[1,var]
      df[getElement(df, "fam_id")== f_id &  getElement(df, "twin_id")==twin_df[2,"twin_id"],var] <- twin_df[2,var]
      
      calculate <- rbind(
        calculate, data.frame(f_id=f_id)
      )
    }
  }
  if (dim(calculate)[1]>1){
    calculate <- data.frame(f_id=calculate[2:nrow(calculate),])
  } else{
    calculate <- data.frame() 
  }
  print(glue::glue(paste("Found {dim(calculate)[1]} pair(s) for '{var}'!")))
  return(df)
}

s <- fill_single_var_twin_from_cotwin2(df=test[1:150,], var="mpvs_total_12_1")
stopifnot(all.equal(s[80,"mpvs_total_12_1"], s[79,"mpvs_total_12_1"]))


fill_multiple_vars_twin_from_cotwin2 <- function(df,vars){
  
  for (var in vars){
    df <- fill_single_var_twin_from_cotwin2(df=df, var=var)
  }
  return(df)
}


fill_single_var_twin_from_cotwin <- function(df, var){
  df <- df %>%
    dplyr::group_by(fam_id) %>%
    fill({{var}}, .direction = "downup") %>%
    dplyr::ungroup()
  return(df)
}

s <- fill_single_var_twin_from_cotwin(
  df=df, var="mpvs_total_12_1"
)

stopifnot(all.equal(s[80,"mpvs_total_12_1"], s[79,"mpvs_total_12_1"]))


s <- df %>% group_by(fam_id) %>% count() %>% arrange(desc(n))

# Test it
# A co-twin of this pair had NA in MPVS
# However, note that
# we need this function only for AGE, which is the same for the twins,
# but not for the rest vars, in which they may differ naturally

s <- fill_single_var_twin_from_cotwin(df=test[70:150,], var="mpvs_total_12_1")
stopifnot(all.equal(s[80,"mpvs_total_12_1"], s[79,"mpvs_total_12_1"]))


fill_multiple_vars_twin_from_cotwin <- function(df,vars){
  
  for (var in vars){
    df <- fill_single_var_twin_from_cotwin(df=df, var=var)
  }
  return(df)
}

# Test it
s <- fill_multiple_vars_twin_from_cotwin(
  df=test[70:150,], 
  vars=c("mpvs_total_12_1", "mpvs_physical_12_1", "mpvs_physical_12_2")
)

stopifnot(all.equal(s[80,"mpvs_total_12_1"], s[79,"mpvs_total_12_1"]))
stopifnot(all.equal(s[80,"mpvs_physical_12_1"], s[79,"mpvs_physical_12_1"]))
stopifnot(all.equal(s[80,"mpvs_physical_12_2"], s[79,"mpvs_physical_12_2"]))

s <- fill_multiple_vars_twin_from_cotwin(
  df=test[1:150,], 
  vars=c(colnames(df)[grepl(pattern="age", x=colnames(df))])
)
stopifnot(all.equal(s[1,"age_26_1"], s[2,"age_26_1"]))
stopifnot(all.equal(s[1,"age_26_2"], s[2,"age_26_2"]))





t1 <- test[1:150,] %>% fill_multiple_vars_twin_from_cotwin(
  vars=c(
    colnames(
      test[1:150,]
    )[grepl(pattern="age", x=colnames(test[1:150,]))] %>% purrr::discard(is.na)
  ) 
)

t2 <- test[1:150,] %>% fill_multiple_vars_twin_from_cotwin2(
  vars=c(
    colnames(
      test[1:150,]
    )[grepl(pattern="age", x=colnames(test[1:150,]))] %>% purrr::discard(is.na)
  ) 
)

comp <- janitor::compare_df_cols(t1,t2, return = "mismatch")
stopifnot(dim(comp)[1]==0)

# See: https://cran.r-project.org/web/packages/arsenal/vignettes/comparedf.html#example-1
s <- arsenal::diffs(arsenal::comparedf(t1,t2))
stopifnot(dim(s)[1]==0)



# Remove test objects
rm(list=c("test", "s", "t1", "t2", "comp"))





