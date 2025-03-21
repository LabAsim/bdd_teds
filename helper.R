library(tidyverse)

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  twin_id = c(11,12,21,22,31,32),
  test_var = c(1,NA,NA,2,NA,NA),
  test_var2 = c(1,NA,NA,2,3,NA)
)

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

s <- fill_single_var_twin_from_cotwin2(
  df=test,
  var="test_var"
)

stopifnot(
  all.equal(
    s,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      twin_id = c(11,12,21,22,31,32),
      test_var = c(1,1,2,2,NA,NA),
      test_var2 = c(1,NA,NA,2,3,NA)
    )
    
  )
)
fill_multiple_vars_twin_from_cotwin2 <- function(df,vars){
  
  for (var in vars){
    df <- fill_single_var_twin_from_cotwin2(df=df, var=var)
  }
  return(df)
}

s <- fill_multiple_vars_twin_from_cotwin2(
  df=test,
  vars=c("test_var", "test_var2")
)

stopifnot(
  all.equal(
    s,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      twin_id = c(11,12,21,22,31,32),
      test_var = c(1,1,2,2,NA,NA),
      test_var2 = c(1,1,2,2,3,3)
    )
    
  )
)

fill_single_var_twin_from_cotwin <- function(df, var){
  df <- df %>%
    dplyr::group_by(fam_id) %>%
    fill({{var}}, .direction = "downup") %>%
    dplyr::ungroup()
  return(as.data.frame(df))
}

s <- fill_single_var_twin_from_cotwin(
  df=test, var="test_var"
)

stopifnot(
  all.equal(
    s,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      twin_id = c(11,12,21,22,31,32),
      test_var = c(1,1,2,2,NA,NA),
      test_var2 = c(1,NA,NA,2,3,NA)
    )
    
  )
)

# A co-twin of this pair had NA in MPVS
# However, note that
# we need this function only for AGE, which is the same for the twins,
# but not for the rest vars, in which they may differ naturally

fill_multiple_vars_twin_from_cotwin <- function(df,vars){
  
  for (var in vars){
    df <- fill_single_var_twin_from_cotwin(df=df, var=var)
  }
  return(df)
}

s <- fill_multiple_vars_twin_from_cotwin(
  df=test,
  vars=c("test_var", "test_var2")
)

stopifnot(
  all.equal(
    s,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      twin_id = c(11,12,21,22,31,32),
      test_var = c(1,1,2,2,NA,NA),
      test_var2 = c(1,1,2,2,3,3)
    )
    
  )
)


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
  return(as.data.frame(df))
}

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  twin_id = c(11,12,21,22,31,32),
  test_var = c(1,NA,NA,2,NA,NA),
  test_var2 = c(3,NA,3,3,3,3),
  test_var3 = c(4,4,4,4,4,4)
)

s <- fill_var(
  df=test, 
  primary = "test_var",
  secondary = "test_var2",
  tertiary = "test_var3",
  new_column = "new_var"
)

stopifnot(
  all.equal(
    s,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      twin_id = c(11,12,21,22,31,32),
      test_var = c(1,NA,NA,2,NA,NA),
      test_var2 = c(3,NA,3,3,3,3),
      test_var3 = c(4,4,4,4,4,4),
      new_var = c(1,4,3,2,3,3)
    )
  )
)


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

extract_unique_nums_probs <- function(df,var){
  unique_values <- unique(round(df[,c(var)],1))
  if (is.null(dim(unique_values))){
    # It's already a vector
    uniq <- unique_values
  }else{
    # Convert df to vector
    uniq <- deframe(unique_values[,1])
  }
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

###############
# COVID waves #
###############

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
    extracted <- extract_unique_nums_probs(df=df, var="diffcov21")
    probs <- extracted$probs
    sorted_nums <- extracted$sorted_nums
    df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov2_child_21_1"] <- df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov1_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov2_child_21_1")]),])[1])
    return(df)
  }
  #######################
  # Fill cov1 from cov2 #
  #######################
  
  df$diffcov12 <- df$age_cov1_child_21_1 - df$age_cov2_child_21_1
  
  # Calculate the relative frequency
  # We will use it to add it to cov1 
  extracted <- extract_unique_nums_probs(df=df, var="diffcov12")
  probs <- extracted$probs
  sorted_nums <- extracted$sorted_nums
  df[is.na(df[,c("age_cov1_child_21_1")]), "age_cov1_child_21_1"] <- df[is.na(df[,c("age_cov1_child_21_1")]), "age_cov2_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov1_child_21_1")]),])[1])
  return(df)
}

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
  extracted <- extract_unique_nums_probs(df=df, var="diffcov23")
  probs <- extracted$probs
  sorted_nums <- extracted$sorted_nums
  df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov2_child_21_1"] <- df[is.na(df[,c("age_cov2_child_21_1")]), "age_cov3_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov2_child_21_1")]),])[1])
  return(df)
}

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
  extracted <- extract_unique_nums_probs(df=df, var="diffcov34")
  probs <- extracted$probs
  sorted_nums <- extracted$sorted_nums
  df[is.na(df[,c("age_cov3_child_21_1")]), "age_cov3_child_21_1"] <- df[is.na(df[,c("age_cov3_child_21_1")]), "age_cov4_child_21_1"] + resample(x=sorted_nums, prob=probs, replace = T, size=dim(df[is.na(df[,c("age_cov3_child_21_1")]),])[1])
  return(df)
}

create_test_df <- function(){
  testit <- data.frame(
    age_cov1_child_21_1 = c(NA,18.5,19,NA,NA,NA,NA),
    age_cov2_child_21_1 = c(20,20.5,NA,20.5,23,NA,NA),
    age_cov3_child_21_1 = c(21,21,NA,21,NA,23,NA),
    age_cov4_child_21_1 = c(22,NA,22,NA,24,24,25)
  )
  return(testit)
} 

testit <- create_test_df()
testit <- fill_age_cov2(df=testit)
stopifnot(all.equal(testit$age_cov1_child_21_1, c(NA,18.5,19.0,NA,NA,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,21,20.5,23,NA,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,NA,21,NA,23,NA)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,NA,22,NA,24,24,25)))


testit <- create_test_df()
testit <- fill_age_cov2(df=testit, order = "desc")
stopifnot(all.equal(testit$age_cov1_child_21_1, c(18,18.5,19.0,18.5,21,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,NA,20.5,23,NA,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,NA,21,NA,23,NA)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,NA,22,NA,24,24,25)))

testit <- create_test_df()
testit <- fill_age_cov3(df=testit)
stopifnot(all.equal(testit$age_cov1_child_21_1, c(NA,18.5,19,NA,NA,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,NA,20.5,23,NA,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,NA,21,23.5,23,NA)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,NA,22,NA,24,24,25)))

testit <- create_test_df()
testit <- fill_age_cov3(df=testit, order="desc")
stopifnot(all.equal(testit$age_cov1_child_21_1, c(NA,18.5,19,NA,NA,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,NA,20.5,23,22,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,NA,21,NA,23,NA)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,NA,22,NA,24,24,25)))

testit <- create_test_df()
testit <- fill_age_cov4(df=testit)
stopifnot(all.equal(testit$age_cov1_child_21_1, c(NA,18.5,19,NA,NA,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,NA,20.5,23,NA,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,NA,21,NA,23,NA)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,22,22,22,24,24,25)))

testit <- create_test_df()
testit <- fill_age_cov4(df=testit,order="desc")
stopifnot(all.equal(testit$age_cov1_child_21_1, c(NA,18.5,19,NA,NA,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,NA,20.5,23,NA,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,21,21,23,23,24)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,NA,22,NA,24,24,25)))


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

testit <- create_test_df()
testit <- fill_age_covid_21(df=testit)
stopifnot(all.equal(testit$age_cov1_child_21_1, c(NA,18.5,19.0,NA,NA,NA,NA)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,21.0,20.5,23,NA,NA)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,22,21,23.5,23,NA)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,22,22,22,24,24,25)))

testit <- create_test_df()
testit <- fill_age_covid_21(df=testit, order="descending")
stopifnot(all.equal(testit$age_cov1_child_21_1, c(18,18.5,19.0,19,21,20.5,22.0)))
stopifnot(all.equal(testit$age_cov2_child_21_1, c(20,20.5,20.5,20.5,23,22,23.5)))
stopifnot(all.equal(testit$age_cov3_child_21_1, c(21,21,21,21,23,23,24)))
stopifnot(all.equal(testit$age_cov4_child_21_1, c(22,NA,22,NA,24,24,25)))

rm(list=c("testit","create_test_df"))
##############################################################################



#####################################################################
# Some twin pairs have the same values, 
# so there is no variance in these families.
# We could drop these rows, they don't contribute to the MSEM model
######################################################################

test <- data.frame(
  fam_id = c(1,1,2,2,3,3,4,4),
  test_var = c(12,12,13,13,14,NA,NA,NA)
)

drop_identical_values <- function(
    df,
    group_var="fam_id",
    var,
    drop_same_value, 
    drop_na
){
  # Some twins have the SAME value in few vars.
  # For this reason, there is no variance within these clusters
  # and lavaan throws an error in MSEM.
  
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  dflist <- split(df, f = list(df[,c(group_var)]), drop = TRUE)
  to_return <- lapply(
    X=dflist, FUN=function(df){
      # If either of the twins has NA, drop both
      if (drop_na==T){
        if (is.na(df[1,var]) ==T | is.na(df[2,var])==T){
          return(NULL)
        }
      }
      # Manipulate twins with the same value in `var`
      if ((is.na(df[1,var]) ==F) & (is.na(df[2,var])==F)){
        if (drop_same_value==F){
          # Return df in all cases 
          # (the same or different values between the twins)
          return(df)
        }else if (drop_same_value==T){
          if (round(df[1,var]) == round(df[2,var])){
            return(NULL)
          }else{
            # The values are different, we should retain them
            return(df)
          }
        }
      }
      else{
        # One of twins or both have NA 
        return(df)
      }
    }
  )
  # https://stackoverflow.com/a/35951683
  to_return <- unname(to_return)
  to_return <- do.call(rbind, to_return)
  # Reset index
  row.names(to_return) <- NULL
  return(to_return)
}


testit <- drop_identical_values(
  df=test,
  var="test_var",
  drop_same_value = F,
  drop_na = F
)
stopifnot(all.equal(testit, test))

testit <- drop_identical_values(
  df=test,
  var="test_var",
  drop_same_value = T,
  drop_na = F
)
row.names(testit) <- NULL
stopifnot(class(testit) == "data.frame")
stopifnot(dim(testit) == c(4,dim(test)[2]))
stopifnot(
  all.equal(
    testit, data.frame(
      fam_id=c(3,3,4,4),
      test_var = c(14,NA,NA,NA)
    )
  )
)

testit <- drop_identical_values(
  df=test,
  var="test_var",
  drop_same_value = F,
  drop_na = T
)

stopifnot(dim(testit) == c(4,dim(test)[2]))
stopifnot(
  testit == data.frame(
    fam_id=c(1,1,2,2),
    test_var = c(12,12,13,13)
  )
)

testit <- drop_identical_values(
  df=test,
  var="test_var",
  drop_same_value = T,
  drop_na = T
)

stopifnot(is.null(testit)==T)

test <- data.frame(
  fam_id = c(1,1,2,2,3,3,4,4,5,5),
  test_var = c(12,12,13,13,14,NA,NA,NA,12,13)
)

testit <- drop_identical_values(
  df=test,
  var="test_var",
  drop_same_value = T,
  drop_na = T
)

stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(5,5),
      test_var = c(12,13)
    )
  )
)

rm(list=c("test", "testit"))

############################################################################
# Few variables contain twins that have different values within each family.
# For example, ~1000 twins have different age values, which is not possible
# In this function, we just simply replace the second twin value with the
# first one.
#############################################################################

test <- data.frame(
  fam_id = c(1,1,2,2,3,3,4,4),
  age_var = c(12,12.1,13,13,14,NA,NA,NA)
)


fix_different_twins_values <- function(
    df,
    group_var="fam_id",
    var,
    replace_na = T
){
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  dflist <- split(df, f = list(df[,c(group_var)]), drop = TRUE)
  to_return <- lapply(
    X=dflist, FUN=function(df){
      # Check for NAs
      if (is.na(df[1,var]) ==T | is.na(df[2,var])==T){
        if (replace_na == F){
          # Just return the df
          return(df)
        }else if (replace_na == T){
          # If just one of the twin has a NA, 
          # replace it with their co-twin value
          # If both have NA, return df
          if (is.na(df[1,var]) ==T){
            # Two possibilities; twin1 has NA and twin2 has NA or numeric
            # In both cases, we are ok
            df[1,var] <- df[2,var]
            return(df)
          } else if (is.na(df[2,var]) == T){
            # 1 possibility; twin1 has a value and twin2 has NA
            df[2,var] <- df[1,var]
            return(df)
          }
        }
      # Twins' values are numeric  
      }else if(round(df[2,var],4) != round(df[1,var],4)){
        # The values are not equal, replace twin2's value with their co-twin's
        df[2,var] <- df[1,var]
        return(df)
      }else{
        return(df)
      }
    }
  )
  # https://stackoverflow.com/a/35951683
  to_return <- unname(to_return)
  to_return <- do.call(rbind, to_return)
  return(to_return)
}


testit <- fix_different_twins_values(df=test, var="age_var", replace_na=F)
stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(1,1,2,2,3,3,4,4),
      age_var = c(12,12,13,13,14,NA,NA,NA)
    )
  )
)
stopifnot(dim(testit) == c(8,2))
testit <- fix_different_twins_values(df=test, var="age_var", replace_na=T)
stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(1,1,2,2,3,3,4,4),
      age_var = c(12,12,13,13,14,14,NA,NA)
    )
  )
)
stopifnot(dim(testit) == c(8,2))

# Remove test objects
rm(list=c("test", "s"))
rm(testit)


subtract_twins_values <- function(
    df,
    group_var="fam_id",
    var
){
  
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  dflist <- split(df, f = list(df[,c(group_var)]), drop = TRUE)
  to_return <- lapply(
    X=dflist, FUN=function(df_){
      diff <- df_[1,var] - df_[2,var]
      return(c(df_[1,"fam_id"], diff))
    }
  )
  # https://stackoverflow.com/a/35951683
  to_return <- unname(to_return)
  to_return <- do.call(rbind, to_return)
  to_return1 <- tibble::tibble(fam_id = to_return[,1])
  to_return2 <- tibble::tibble("{var}" := to_return[,2])
  to_return <- as.data.frame(cbind(to_return1,to_return2))
  return(to_return)
}

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  test_var = c(1:6)
)
test_diff <-  subtract_twins_values(df=test, var="test_var")
stopifnot(class(test_diff) == "data.frame")
stopifnot(dim(test_diff) == c(3,2))
stopifnot(test_diff$variable == c(-1,-1,-1))
stopifnot(test_diff$fam_id == c(1:3))
rm(list=c("test", "test_diff"))

left_join_df_diff_twin_values <- function(left_df,right_df, join_by_var="fam_id"){
  df <- left_join(
    x=left_df,
    y=right_df,
    join_by({{join_by_var}}=={{join_by_var}})
  )
  return(df)
}

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  test_var = c(1:6)
)
test_diff <-  subtract_twins_values(df=test, var="test_var")
test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  test_var2 = c(6:1)
)
test_diff2 <-  subtract_twins_values(df=test, var="test_var2")

test <- left_join_df_diff_twin_values(
  left_df = test_diff,
  right_df = test_diff2,
  join_by_var = "fam_id"
)
stopifnot(class(test) == "data.frame")
stopifnot(dim(test) == c(3,3))
stopifnot(test$test_var == c(-1,-1,-1))
stopifnot(test$test_var2 == c(1,1,1))
stopifnot(test$fam_id == c(1:3))


left_join_multiple_df_diff_twin_values <- function(
    left_df, right_dfs, join_by_var="fam_id"
){
  
  stopifnot(class(right_dfs) == "list")
  if (class(right_dfs[[1]]) != "data.frame"){
    message(paste0("object:",right_dfs[[1]], "\n"))
    stop(
      glue::glue(
        "{class(right_dfs[[1]])} is not a data.frame! 
        `right_dfs` need to be a list(), not c()!"
      )
    )
  }
  
  for (df in right_dfs){
    left_df <- left_join_df_diff_twin_values(
      left_df = left_df,
      right_df = df,
      join_by_var = "fam_id"
    )
  }
  return(left_df)
}


test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  test_var = c(1:6)
)
test_diff <-  subtract_twins_values(df=test, var="test_var")
test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  test_var2 = c(6:1)
)
test_diff2 <-  subtract_twins_values(df=test, var="test_var2")

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  test_var3 = c(1:6)
)
test_diff3 <-  subtract_twins_values(df=test, var="test_var3")

test <- left_join_multiple_df_diff_twin_values(
  left_df = test_diff,
  right_dfs = list(test_diff2, test_diff3),
  join_by_var = "fam_id"
)

stopifnot(class(test) == "data.frame")
stopifnot(dim(test) == c(3,4))
stopifnot(test$test_var == c(-1,-1,-1))
stopifnot(test$test_var2 == c(1,1,1))
stopifnot(test$test_var3 == c(-1,-1,-1))
stopifnot(test$fam_id == c(1:3))

rm(list=c("test", "test_diff", "test_diff2", "test_diff3"))




scale_mpvs <- function(df, scale_size=32){
  # Age 12
  # 16 items
  df$mpvs_total_12_1_scaled <- df$mpvs_total_12_1/32
  df[,paste0("mpvs_total_12_1_scaled","_",scale_size)] <- df$mpvs_total_12_1_scaled * scale_size
  # Age 14
  # 12 items
  df$mpvs_total_14_1_scaled <- df$mpvs_total_14_1/24
  df[,paste0("mpvs_total_14_1_scaled","_",scale_size)] <- df$mpvs_total_14_1_scaled * scale_size
  
  # Age 16
  # 6 items
  df$mpvs_total_16_1_scaled <- df$mpvs_total_16_1/12
  df[,paste0("mpvs_total_16_1_scaled","_",scale_size)] <- df$mpvs_total_16_1_scaled * scale_size
  
  ##########
  # Age 21 #
  ##########
  # 16 items
  df$mpvs_total_21_phase_2_1_scaled <- df$mpvs_total_21_phase_2_1/32
  df[,paste0("mpvs_total_21_phase_2_1_scaled","_",scale_size)] <- df$mpvs_total_21_phase_2_1_scaled * scale_size
  # 12 items
  df$mpvs_total_21_cov1_1_scaled <- df$mpvs_total_21_cov1_1/24
  df[,paste0("mpvs_total_21_cov1_1_scaled","_",scale_size)] <- df$mpvs_total_21_cov1_1_scaled * scale_size
  df$mpvs_total_21_cov2_1_scaled <- df$mpvs_total_21_cov2_1/24
  df[,paste0("mpvs_total_21_cov2_1_scaled","_",scale_size)] <- df$mpvs_total_21_cov2_1_scaled * scale_size
  
  df$mpvs_total_21_cov3_1_scaled <- df$mpvs_total_21_cov3_1/24
  df[,paste0("mpvs_total_21_cov3_1_scaled","_",scale_size)] <- df$mpvs_total_21_cov3_1_scaled * scale_size
  
  df$mpvs_total_21_cov4_1_scaled <- df$mpvs_total_21_cov4_1/24
  df[,paste0("mpvs_total_21_cov4_1_scaled","_",scale_size)] <- df$mpvs_total_21_cov4_1_scaled * scale_size
  
  return(df)
}
test <- data.frame(
  mpvs_total_12_1 = 32,
  mpvs_total_14_1 = 12,
  mpvs_total_16_1 = 6,
  mpvs_total_21_phase_2_1 = 16,
  mpvs_total_21_cov1_1 = 12,
  mpvs_total_21_cov2_1 = 12,
  mpvs_total_21_cov3_1 = 12,
  mpvs_total_21_cov4_1 = 12
)

testit <- scale_mpvs(df=test, scale_size = 32)
stopifnot(testit$mpvs_total_12_1_scaled == test$mpvs_total_12_1/32)
stopifnot(testit$mpvs_total_14_1_scaled == test$mpvs_total_14_1/24)
stopifnot(testit$mpvs_total_16_1_scaled == test$mpvs_total_16_1/12)
stopifnot(testit$mpvs_total_21_phase_2_1_scaled == test$mpvs_total_21_phase_2_1/32)
stopifnot(testit$mpvs_total_21_cov1_1_scaled == test$mpvs_total_21_cov1_1/24)
stopifnot(testit$mpvs_total_21_cov2_1_scaled == test$mpvs_total_21_cov2_1/24)
stopifnot(testit$mpvs_total_21_cov3_1_scaled == test$mpvs_total_21_cov3_1/24)
stopifnot(testit$mpvs_total_21_cov4_1_scaled == test$mpvs_total_21_cov4_1/24)

scale_size <- 32
stopifnot(testit$mpvs_total_12_1_scaled_32 == test$mpvs_total_12_1_scaled*scale_size)
stopifnot(testit$mpvs_total_14_1_scaled_32 == test$mpvs_total_14_1_scaled*scale_size)
stopifnot(testit$mpvs_total_16_1_scaled_32 == test$mpvs_total_16_1_scaled*scale_size)
stopifnot(testit$mpvs_total_21_phase_2_1_scaled_32 == test$mpvs_total_21_phase_2_1_scaled*scale_size)
stopifnot(testit$mpvs_total_21_cov1_1_scaled_32 == test$mpvs_total_21_cov1_1_scaled*scale_size)
stopifnot(testit$mpvs_total_21_cov2_1_scaled_32 == test$mpvs_total_21_cov2_1_scaled*scale_size)
stopifnot(testit$mpvs_total_21_cov3_1_scaled_32 == test$mpvs_total_21_cov3_1_scaled*scale_size)
stopifnot(testit$mpvs_total_21_cov4_1_scaled_32 == test$mpvs_total_21_cov4_1_scaled*scale_size)
stopifnot(dim(testit) == c(1,24))
stopifnot(
  c(
    "mpvs_total_12_1_scaled","mpvs_total_14_1_scaled",
    "mpvs_total_16_1_scaled", "mpvs_total_21_phase_2_1_scaled",
    "mpvs_total_21_cov1_1_scaled", "mpvs_total_21_cov2_1_scaled",
    "mpvs_total_21_cov3_1_scaled", "mpvs_total_21_cov4_1_scaled",
    "mpvs_total_12_1_scaled_32","mpvs_total_14_1_scaled_32",
    "mpvs_total_16_1_scaled_32", "mpvs_total_21_phase_2_1_scaled_32",
    "mpvs_total_21_cov1_1_scaled_32", "mpvs_total_21_cov2_1_scaled_32",
    "mpvs_total_21_cov3_1_scaled_32", "mpvs_total_21_cov4_1_scaled_32"
  ) %in% colnames(testit)
)

rm(list=c("test", "testit", "scale_size"))


