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

test <- df_1

test1 <- df_1
test1$age_12_1 <- NA
test1$age_12_1 <- ifelse(
  test = is.na(test1$age_parent_12), 
  yes = ifelse(
    test=is.na(test1$age_child_12_1),
    yes = test1$age_teach_12_1,
    no = test1$age_child_12_1
  ),
  no=test1$age_parent_12
)

colSums(is.na(test1[,c(colnames(test1)[grepl(pattern="age", x=colnames(test1))])]))

fill_var <- function(df, primary, secondary, tertiary, new_column){
  df[,c(new_column)] <- rep(NA, times=dim(df)[1])
  print(rlang::as_name(new_column))
  df <- df %>% 
    mutate(
      "{new_column}" := case_when(
        is.na(.data[[!!new_column]]) ~ .data[[!!primary]]
      )
    )
  df <- df %>% 
    mutate(
      "{new_column}" := case_when(
        is.na(.data[[!!new_column]]) ~ .data[[!!secondary]],
        .default = .data[[!!new_column]]
      )
    )
  df <- df %>% 
    mutate(
      "{new_column}" := case_when(
        is.na(.data[[!!new_column]]) ~ .data[[!!tertiary]],
        .default = .data[[!!new_column]]
      )
    )
  return(df)
}

test <- fill_var(
  df=df_1, 
  primary = "age_parent_12",
  secondary = "age_child_12_1",
  tertiary = "age_teach_12_1",
  new_column = "age_12_1"
)
t1 <- colSums(is.na(test1[,c(colnames(test1)[grepl(pattern="age", x=colnames(test1))])]))
t2 <- colSums(is.na(test[,c(colnames(test)[grepl(pattern="age", x=colnames(test))])]))
stopifnot(t1 == t2)
stopifnot(t1["age_12_1"] == 3269)


test <- df_1 %>% 
  select(
    twin_id,
    fam_id,
    mpvs_total_21_phase_2_1 ,
    mpvs_total_21_cov1_1       ,
    mpvs_total_21_cov2_1,
    mpvs_total_21_cov3_1       ,
    mpvs_total_21_cov4_1 ,
    age_phase2_child_21_1, age_cov1_child_21_1 , age_cov2_child_21_1,
    age_cov3_child_21_1 ,age_cov4_child_21_1
  ) 

colSums(is.na(test[,c(colnames(test)[grepl(pattern="age", x=colnames(test))])]))

test <- fill_multiple_vars_twin_from_cotwin(
  df=test, 
  vars = c(
    "age_phase2_child_21_1", "age_cov1_child_21_1" , "age_cov2_child_21_1",
    "age_cov3_child_21_1" ,"age_cov4_child_21_1"
  )
)
colSums(is.na(test[,c(colnames(test)[grepl(pattern="age", x=colnames(test))])]))


# See ?sample
resample <- function(x, ...) x[sample.int(length(x), ...)]

fill_age_cov2 <- function(df,order="ascending"){
  set.seed(123)
  # Cov2 and Cov1 phases were done from 1 to 2 months apart
  # See the table: https://datadictionary.teds.ac.uk/studies/21yr.htm
  
  if (order=="ascending"){
    #############################
    # Fill NA of cov2 with cov1 #
    #############################
    df$diffcov21 <- df$age_cov2_child_21_1 - df$age_cov1_child_21_1
    
    # Calculate the relative frequency
    # We will use it to add it to cov2 
    sum1 <- sum(df[round(df$diffcov21,1)==0.1, "diffcov21"], na.rm = T)
    sum2 <- sum(df[round(df$diffcov21,1)==0.2, "diffcov21"], na.rm = T)
    sum3 <- sum(df[round(df$diffcov21,1)==0.3, "diffcov21"], na.rm = T)
    total <- sum1+sum2+sum3
    prob1 <- sum1/total
    prob2 <- sum2/total
    prob3 <- sum3/total
    probs <- c(prob1,prob2,prob3)
    sorted_nums <- sort(unique(round(df$diffcov21,1)))
    df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov2_child_21_1"] <- df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov1_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov2_child_21_1")]),])[1])
    return(df)
  }
  #######################
  # Fill cov1 from cov2 #
  #######################
  
  df$diffcov12 <- df$age_cov1_child_21_1 - df$age_cov2_child_21_1
  
  # Calculate the relative frequency
  # We will use it to add it to cov1 
  sum1 <- sum(df[round(df$diffcov12,1)==-0.1, "diffcov12"], na.rm = T)
  sum2 <- sum(df[round(df$diffcov12,1)==-0.2, "diffcov12"], na.rm = T)
  sum3 <- sum(df[round(df$diffcov12,1)==-0.3, "diffcov12"], na.rm = T)
  total <- sum1+sum2+sum3
  prob1 <- sum1/total
  prob2 <- sum2/total
  prob3 <- sum3/total
  probs <- c(prob1,prob2,prob3)
  sorted_nums <- sort(unique(round(df$diffcov12,1)))
  df[is.na(df[,c("age_cov1_child_21_1")]), "age_cov1_child_21_1"] <- df[is.na(df[,c("age_cov1_child_21_1")]), "age_cov2_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov1_child_21_1")]),])[1])
  return(df)
}

# Test
test <- fill_age_cov2(df=test)
t1 <- colSums(
  is.na(
    test[,c(colnames(test)[grepl(pattern="age_cov", x=colnames(test))])]
  )
)
colSums(
  is.na(
    test[,c(colnames(test)[grepl(pattern="age", x=colnames(test))])]
  )
)
stopifnot(t1["age_cov1_child_21_1"] == 9078)
stopifnot(t1["age_cov2_child_21_1"] == 8404)
stopifnot(t1["age_cov3_child_21_1"] == 10816)
stopifnot(t1["age_cov4_child_21_1"] ==10394)


extract_unique_nums_probs <- function(df,var){
  uniq <- deframe(unique(round(df[,c(var)],1))[,1])
  sorted_nums <- sort(uniq)
  sums <- c()
  for (i in sorted_nums){
    sums <- cbind(sums,sum(df[round(df[,var],1)==i, var], na.rm = T))
  }
  total <- sum(sums)
  probs <- c()
  for (i in sums){
    probs <- c(probs, i/total)
  }
  to_return <- data.frame(
    sorted_nums = sorted_nums,
    probs = probs
  )
  print(to_return)
  return(to_return)
}

# Test
test$diffcov32 <- test$age_cov3_child_21_1 - test$age_cov2_child_21_1
extracted <- extract_unique_nums_probs(df=test,var="diffcov32")
stopifnot(deframe(round(extracted["sorted_nums"],1)) == round(seq(from=0.1,to=0.5,by=0.1),1))
stopifnot(round(extracted["probs"],2) == c(0,0.03,0.75,0.22, 0))
test <- test %>% select(-diffcov32)


fill_age_cov3 <- function(df,order="ascending"){
  set.seed(123)
  # Cov2 and Cov1 phases were done from 1 to 2 months apart
  # See the table: https://datadictionary.teds.ac.uk/studies/21yr.htm
  
  if (order=="ascending"){
    ##############################
    # Fill NA of cov3 with cov2 #
    ##############################
    df$diffcov32 <- df$age_cov3_child_21_1 - df$age_cov2_child_21_1
    
    # Calculate the relative frequency
    # We will use it to add it to cov3 
    extracted <- extract_unique_nums_probs(df=df, var="diffcov32")
    probs <- extracted$probs
    sorted_nums <- extracted$sorted_nums
    df[is.na(df[,c("age_cov3_child_21_1")]), "age_cov3_child_21_1"] <- df[is.na(df[,c("age_cov3_child_21_1")]), "age_cov2_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov3_child_21_1")]),])[1])
    return(df)
  }
  #######################
  # Fill cov2 from cov3 #
  #######################
  
  df$diffcov23 <- df$age_cov2_child_21_1 - df$age_cov3_child_21_1
  
  # Calculate the relative frequency
  # We will use it to add it to cov2 
  
  # sum1 <- sum(df[round(df$diffcov23,1)==-0.1, "diffcov23"], na.rm = T)
  # sum2 <- sum(df[round(df$diffcov23,1)==-0.2, "diffcov23"], na.rm = T)
  # sum3 <- sum(df[round(df$diffcov23,1)==-0.3, "diffcov23"], na.rm = T)
  # sum4 <- sum(df[round(df$diffcov23,1)==-0.4, "diffcov23"], na.rm = T)
  # sum5 <- sum(df[round(df$diffcov23,1)==-0.5, "diffcov23"], na.rm = T)
  # total <- sum1+sum2+sum3+sum4+sum5
  # 
  # prob1 <- sum1/total
  # prob2 <- sum2/total
  # prob3 <- sum3/total
  # prob4 <- sum4/total
  # prob5 <- sum5/total
  # probs <- c(prob1,prob2,prob3,prob4,prob5)
  # sorted_nums <- sort(unique(round(df$diffcov23,1)))
  
  extracted <- extract_unique_nums_probs(df=df, var="diffcov23")
  probs <- extracted$probs
  sorted_nums <- extracted$sorted_nums
  df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov2_child_21_1"] <- df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov3_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov2_child_21_1")]),])[1])
  return(df)
}

# Test
test <- fill_age_cov3(df=test)
t1 <- colSums(is.na(test[,c(colnames(test)[grepl(pattern="age_cov", x=colnames(test))])]))
stopifnot(t1["age_cov1_child_21_1"] == 9078)
stopifnot(t1["age_cov2_child_21_1"] == 8404)
stopifnot(t1["age_cov3_child_21_1"] == 8166)
stopifnot(t1["age_cov4_child_21_1"] ==10394)


fill_age_cov4 <- function(df,order="ascending"){
  set.seed(123)
  # Cov2 and Cov1 phases were done from 1 to 2 months apart
  # See the table: https://datadictionary.teds.ac.uk/studies/21yr.htm
  
  if (order=="ascending"){
    ##############################
    # Fill NA of cov4 with cov3 #
    ##############################
    df$diffcov43 <- df$age_cov4_child_21_1 - df$age_cov3_child_21_1
    
    # Calculate the relative frequency
    # We will use it to add it to cov4 
    # sum2 <- sum(df[round(df$diffcov43,1)==0.2, "diffcov43"], na.rm = T)
    # sum3 <- sum(df[round(df$diffcov43,1)==0.3, "diffcov43"], na.rm = T)
    # sum4 <- sum(df[round(df$diffcov43,1)==0.4, "diffcov43"], na.rm = T)
    # sum5 <- sum(df[round(df$diffcov43,1)==0.5, "diffcov43"], na.rm = T)
    # sum6 <- sum(df[round(df$diffcov43,1)==0.6, "diffcov43"], na.rm = T)
    # total <- sum1+sum2+sum3+sum4+sum5+sum6
    # prob1 <- sum1/total
    # prob2 <- sum2/total
    # prob3 <- sum3/total
    # prob4 <- sum4/total
    # prob5 <- sum5/total
    # prob6 <- sum6/total
    # 
    # probs <- c(prob1,prob2,prob3,prob4,prob5,prob6)
    # sorted_nums <- sort(unique(round(df$diffcov43,1)))
    
    extracted <- extract_unique_nums_probs(df=df, var="diffcov43")
    probs <- extracted$probs
    sorted_nums <- extracted$sorted_nums
    df[is.na(df[,c("age_cov4_child_21_1")]), "age_cov4_child_21_1"] <- df[is.na(df[,c("age_cov4_child_21_1")]), "age_cov3_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov4_child_21_1")]),])[1])
    return(df)
  }
  #######################
  # Fill cov2 from cov3 #
  #######################
  
  df$diffcov34 <- df$age_cov3_child_21_1 - df$age_cov4_child_21_1
  
  # Calculate the relative frequency
  # We will use it to add it to cov3 
  # sum21 <- sum(df[round(df$diffcov34,1)==0.21, "diffcov34"], na.rm = T)
  # sum2 <- sum(df[round(df$diffcov34,1)==0.2, "diffcov34"], na.rm = T)
  # sum3 <- sum(df[round(df$diffcov34,1)==0.3, "diffcov34"], na.rm = T)
  # sum4 <- sum(df[round(df$diffcov34,1)==0.4, "diffcov34"], na.rm = T)
  # sum5 <- sum(df[round(df$diffcov34,1)==0.5, "diffcov34"], na.rm = T)
  # sum6 <- sum(df[round(df$diffcov34,1)==0.6, "diffcov34"], na.rm = T)
  # total <- sum1+sum2+sum3+sum4+sum5+sum6
  # prob1 <- sum1/total
  # prob2 <- sum2/total
  # prob3 <- sum3/total
  # prob4 <- sum4/total
  # prob5 <- sum5/total
  # prob6 <- sum6/total
  # 
  # probs <- c(prob1,prob2,prob3,prob4,prob5,prob6)
  # sorted_nums <- sort(unique(round(df$diffcov34,1)))
  
  extracted <- extract_unique_nums_probs(df=df, var="diffcov34")
  probs <- extracted$probs
  sorted_nums <- extracted$sorted_nums
  df[is.na(df[,c("age_cov3_child_21_1")]), "age_cov3_child_21_1"] <- df[is.na(df[,c("age_cov3_child_21_1")]), "age_cov4_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov3_child_21_1")]),])[1])
  return(df)
}

# Test
test <- fill_age_cov4(df=test)

t1 <- colSums(is.na(test[,c(colnames(test)[grepl(pattern="age_cov", x=colnames(test))])]))
stopifnot(t1["age_cov1_child_21_1"] == 9078)
stopifnot(t1["age_cov2_child_21_1"] == 8404)
stopifnot(t1["age_cov3_child_21_1"] == 8166)
stopifnot(t1["age_cov4_child_21_1"] ==7996)


fill_age_covid_21 <- function(df,order="ascending"){
  set.seed(123)
  if (order=="ascending"){
    df <- fill_age_cov2(df=df)
    df <- fill_age_cov3(df=df)
    df <- fill_age_cov4(df=df)
  }else{
    df <- fill_age_cov4(df=df, order=order)
    df <- fill_age_cov3(df=df, order=order)
    df <- fill_age_cov2(df=df, order=order)
  }
  return(df)
}

# Test
test <- fill_age_covid_21(df=test, order="descending")

t1 <- colSums(is.na(test[,c(colnames(test)[grepl(pattern="age_cov", x=colnames(test))])]))
stopifnot(t1["age_cov1_child_21_1"] == 7996)
stopifnot(t1["age_cov2_child_21_1"] == 7996)
stopifnot(t1["age_cov3_child_21_1"] == 7996)
stopifnot(t1["age_cov4_child_21_1"] ==7996)




# Remove test objects
rm(list=c("test", "s", "t1", "t2", "comp", "test1", "extracted"))





