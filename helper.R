library(tidyverse)


time_and_beep <- function(f, sound=1) {
  function(...) {
    start_time <- Sys.time()
    to_return <- f(...)
    on.exit(beepr::beep(sound))
    end_time <- Sys.time()
    print(Sys.time()-start_time)
    rm(start_time)
    return(to_return)
  }
}

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
  df <- df |>
    dplyr::group_by(fam_id) |>
    fill({{var}}, .direction = "downup")|>
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
  if (!(var %in% colnames(df))){
    stop(paste("`",var,"`", "does not exist in column names!"))
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


subtract_mz_twins_values <- function(
    df,
    group_var="fam_id",
    var,
    sex_var = "sex_1"
){
  if ((sex_var %in% colnames(df)) == F){
    stop(
      glue::glue(
        "`{sex_var}` is not a column name of the df"
      )
    )
  }
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  dflist <- split(df, f = list(df[,c(group_var)]), drop = TRUE)
  to_return <- lapply(
    X=dflist, FUN=function(df_){
      if (df_[1, sex_var] == df_[2, sex_var]){
        diff <- df_[1,var] - df_[2,var]
        return(c(df_[1,"fam_id"], diff))
      }else{
        return(NULL)
      }
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
  sex = c(0,0,1,1,0,1),
  test_var = c(1:6)
)
test_diff <-  subtract_mz_twins_values(df=test, var="test_var", sex_var = "sex")
stopifnot(class(test_diff) == "data.frame")
stopifnot(dim(test_diff) == c(2,2))
stopifnot(test_diff$variable == c(-1,-1))
stopifnot(test_diff$fam_id == c(1:2))
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


create_df_subtract_twins_values_multiple_vars <- function(
    df,
    group_var="fam_id",
    vars
){
  for (var in vars){
    # https://www.r-bloggers.com/2024/05/how-to-check-if-a-column-exists-in-a-data-frame-in-r/
    if (!(var %in% colnames(df))){
      stop(paste0(var,"does not exist in column names!"))
    }
  }
  temp_list <- list(subtract_mz_twins_values(df=df, var=vars[1]))
  for (var in vars[2:length(vars)]){
    loop_df <- subtract_mz_twins_values(df=df, var=var)
    temp_list <- c(temp_list, list(loop_df))
  }
  
  temp_df <- left_join_multiple_df_diff_twin_values(
    left_df = temp_list[[1]],
    right_dfs = temp_list[-1],
    join_by_var = "fam_id"
  )
  return(temp_df)
}

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  sex_1 = c(1,1,1,1,0,0),
  test_var = c(1:6),
  test_var2 = seq(from=10, to=20, by=2)
)
test <- create_df_subtract_twins_values_multiple_vars(
  df=test, vars=c("test_var","test_var2")
)

stopifnot(class(test) == "data.frame")
stopifnot(dim(test) == c(3,3))
stopifnot(test$test_var == c(-1,-1,-1))
stopifnot(test$test_var2 == c(-2,-2,-2))
stopifnot(test$fam_id == c(1:3))

rm(list=c("test"))


# Prints execution time and beeps
create_df_subtract_twins_values_multiple_vars_decorated <- time_and_beep(
  create_df_subtract_twins_values_multiple_vars
)

test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  sex_1 = c(1,1,1,1,0,0),
  test_var = c(1:6),
  test_var2 = seq(from=10, to=20, by=2)
)
test <- create_df_subtract_twins_values_multiple_vars_decorated(
  df=test, vars=c("test_var","test_var2")
)

stopifnot(class(test) == "data.frame")
stopifnot(dim(test) == c(3,3))
stopifnot(test$test_var == c(-1,-1,-1))
stopifnot(test$test_var2 == c(-2,-2,-2))
stopifnot(test$fam_id == c(1:3))

rm(list=c("test"))

create_scaled_var <- function(df,from_var,to_var, scale_size=32){
  if (grepl(pattern="16", x=from_var)){
    df[,to_var] <- df[,from_var]/12
  } else if (grepl(pattern="cov", x=from_var)){
    df[,to_var] <- df[,from_var]/24
  } else{
    df[,to_var] <- df[,from_var]/32
  }
  df[,paste0(to_var,"_",scale_size)] <- df[,to_var] * scale_size
  return(df)
}

scale_mpvs <- function(df,from_vars, scale_size=32){
  to_vars <- paste0(from_vars,"_scaled")
  for (num in 1:length(from_vars)){
    df <- create_scaled_var(
      df=df, from_var = from_vars[num],
      to_var = to_vars[num], scale_size=scale_size
    )
  }
  return(df)
}

test <- data.frame(
  # Max values for these vars
  mpvs_total_12_1 = 32,
  mpvs_total_14_1 = 32,
  mpvs_total_16_1 = 12,
  mpvs_total_phase_2_21_1 = 32,
  mpvs_total_cov1_21_1 = 24,
  mpvs_total_cov2_21_1 = 24,
  mpvs_total_cov3_21_1 = 24,
  mpvs_total_cov4_21_1 = 24
)

testit <- scale_mpvs(
  df=test, scale_size = 32,
  from_vars = c(
    "mpvs_total_12_1","mpvs_total_14_1","mpvs_total_16_1",
    "mpvs_total_phase_2_21_1", "mpvs_total_cov1_21_1",
    "mpvs_total_cov2_21_1", "mpvs_total_cov3_21_1","mpvs_total_cov4_21_1"
  )
)
stopifnot(testit$mpvs_total_12_1_scaled == test$mpvs_total_12_1/32)
stopifnot(testit$mpvs_total_14_1_scaled == test$mpvs_total_14_1/32)
stopifnot(testit$mpvs_total_16_1_scaled == test$mpvs_total_16_1/12)
stopifnot(testit$mpvs_total_phase_2_21_1_scaled == test$mpvs_total_phase_2_21_1/32)
stopifnot(testit$mpvs_total_cov1_21_1_scaled == test$mpvs_total_cov1_21_1/24)
stopifnot(testit$mpvs_total_cov2_21_1_scaled == test$mpvs_total_cov2_21_1/24)
stopifnot(testit$mpvs_total_cov3_21_1_scaled == test$mpvs_total_cov3_21_1/24)
stopifnot(testit$mpvs_total_cov4_21_1_scaled == test$mpvs_total_cov4_21_1/24)

scale_size <- 32
stopifnot(testit$mpvs_total_12_1_scaled_32 == test$mpvs_total_12_1_scaled*scale_size)
stopifnot(testit$mpvs_total_14_1_scaled_32 == test$mpvs_total_14_1_scaled*scale_size)
stopifnot(testit$mpvs_total_16_1_scaled_32 == test$mpvs_total_16_1_scaled*scale_size)
stopifnot(testit$mpvs_total_phase_2_21_1_scaled_32 == test$mpvs_total_phase_2_21_1_scaled*scale_size)
stopifnot(testit$mpvs_total_cov1_21_1_scaled_32 == test$mpvs_total_cov1_21_1_scaled*scale_size)
stopifnot(testit$mpvs_total_cov2_21_1_scaled_32 == test$mpvs_total_cov2_21_1_scaled*scale_size)
stopifnot(testit$mpvs_total_cov3_21_1_scaled_32 == test$mpvs_total_cov3_21_1_scaled*scale_size)
stopifnot(testit$mpvs_total_cov4_21_1_scaled_32 == test$mpvs_total_cov4_21_1_scaled*scale_size)
stopifnot(dim(testit) == c(1,24))
stopifnot(
  c(
    "mpvs_total_12_1_scaled","mpvs_total_14_1_scaled",
    "mpvs_total_16_1_scaled", "mpvs_total_phase_2_21_1_scaled",
    "mpvs_total_cov1_21_1_scaled", "mpvs_total_cov2_21_1_scaled",
    "mpvs_total_cov3_21_1_scaled", "mpvs_total_cov4_21_1_scaled",
    "mpvs_total_12_1_scaled_32","mpvs_total_14_1_scaled_32",
    "mpvs_total_16_1_scaled_32", "mpvs_total_phase_2_21_1_scaled_32",
    "mpvs_total_cov1_21_1_scaled_32", "mpvs_total_cov2_21_1_scaled_32",
    "mpvs_total_cov3_21_1_scaled_32", "mpvs_total_cov4_21_1_scaled_32"
  ) %in% colnames(testit)
)

rm(list=c("test", "testit", "scale_size"))


drop_identical_fix_different_values <- function(df,fix_vec, drop_identical_vec){
  for (var in fix_vec){
    df <- df %>%
      fix_different_twins_values(var=var)
  }
  for (var in drop_identical_vec){
    df <- df %>%
      drop_identical_values(var=var, drop_same_value=T, drop_na = T)
  }
  return(df)
}


test <- data.frame(
  fam_id = c(1,1,2,2,3,3),
  twin_id = c(11,12,21,22,31,32),
  test_item_var = c(1,1,1,2,2,2),
  test_item_2var = c(1,1,2,2,3,3),
  test_var = c(1,1,2,2,3,3)
)

calculate_items <- function(
    df, target_phrase,target_phrase2="item", output_var
){
  x <- df[,colnames(df)[grepl(
    pattern=(paste0(target_phrase,"$")),x=colnames(df)
  )]]
  x <- x[,colnames(x)[grepl(pattern=(target_phrase2),x=colnames(x))]]
  print(glue::glue("Found {dim(x)[2]} columns;"))
  print(colnames(x))
  df <- df %>% 
    mutate(
      "{output_var}" := rowSums(
        x=x
      )
    )
  return(df)
}

test <- calculate_items(
  df=test, 
  target_phrase = "var",
  target_phrase2 = "item",
  output_var = "testit"
)

stopifnot(
  all.equal(
    test,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      twin_id = c(11,12,21,22,31,32),
      test_item_var = c(1,1,1,2,2,2),
      test_item_2var = c(1,1,2,2,3,3),
      test_var = c(1,1,2,2,3,3),
      testit = c(2,2,3,4,5,5)
    )
  )
)

rm(test)




# Exclude collinear vars

exclude_collinear_vars <- function(
    pred_matrix,
    corr_mat, 
    lower_threshold=0.1,
    upper_threshold=0.99
){
  # That's the function of mice for removing collinear vars
  # https://github.com/amices/mice/blob/7df3487a56cd49dffc9192a8ebe9697bfa7258ca/R/internal.R#L84-L91
  # Inclusion of low correlated varvs is not useful
  # See: Auxiliary_variables_in_multiple_imputation_in_regr
  # See also a practical guide
  # https://pmc.ncbi.nlm.nih.gov/articles/PMC8017730/
  # It suggests keeping auxiliary vars with cors >=0.5 
  for (colvar in colnames(corr_mat)){
    for (rowvar in rownames(corr_mat)){
      if (!is.na(corr_mat[rowvar, colvar])){
        if ((abs(corr_mat[rowvar, colvar]) > upper_threshold) | abs(corr_mat[rowvar, colvar]) < lower_threshold){
          print(
            glue::glue("Removing; `{rowvar} - {colvar}` with corr={corr_mat[rowvar, colvar]}")
          )
          pred_matrix[rowvar, colvar] <- 0
          pred_matrix[colvar, rowvar] <- 0
        }
      }
    }
  }
  return(pred_matrix)
}

# apply_exclude_collinear_vars <- function(
#     pred_matrix,
#     corr_mat, 
#     lower_threshold=0.1,
#     upper_threshold=0.99
# ){
#   # That's the function of mice for removing collinear vars
#   # https://github.com/amices/mice/blob/7df3487a56cd49dffc9192a8ebe9697bfa7258ca/R/internal.R#L84-L91
#   # Inclusion of low correlated varvs is not useful
#   # See: Auxiliary_variables_in_multiple_imputation_in_regr
#   # See also a practical guide
#   # https://pmc.ncbi.nlm.nih.gov/articles/PMC8017730/
#   # It suggests keeping auxiliary vars with cors >=0.5 
#   s <- lapply(
#     X=split.data.frame(corr_mat, as.factor(rownames(corr_mat))),
#     FUN = function(x_df){
#       for (colvar in colnames(x_df)){
#         
#         if (!is.na(corr_mat[rownames(x_df), colvar])){
#           if ((abs(corr_mat[rownames(x_df), colvar]) > upper_threshold) | abs(corr_mat[rownames(x_df), colvar]) < lower_threshold){
#             print(
#               glue::glue("Removing; `{rownames(x_df)} - {colvar}` with corr={corr_mat[rownames(x_df), colvar]}")
#             )
#             pred_matrix[rownames(x_df), colvar] <- 0
#             pred_matrix[colvar, rownames(x_df)] <- 0
#           }
#         }
#       }
#     }
#   )
#   return(pred_matrix)
# }

test_df <- data.frame(
  age_child_12_1 = c(0,1,1,1,1,1),
  age_parent_14  = c(1,0,1,1,1,1),
  age_teach_14_1 = c(1,1,0,1,1,1),
  age_child_14_1 = c(1,1,1,0,1,1),
  # age_child_16_1 & age_parent_16 are highly correlated
  age_child_16_1 = c(1,1,1,1,0,0),
  age_parent_16 = c(1,1,1,1,0,0)
)

predMatrix <- mice::make.predictorMatrix(data = test_df)

corr_mat <- cor(test_df, method="spearman", use="pairwise.complete.obs")
# ΝΑ are produced because one var does not vary when their pair does, so
# their SD ==0 and the correlation is NA

corr_mat <- as.data.frame(corr_mat)
s <- exclude_collinear_vars(
  pred_matrix = predMatrix, 
  corr_mat=cor(test_df, use = "pairwise.complete.obs")
)


test_s <- data.frame(
  age_child_12_1 = c(0,1,1,1,1,1),
  age_parent_14  = c(1,0,1,1,1,1),
  age_teach_14_1 = c(1,1,0,1,1,1),
  age_child_14_1 = c(1,1,1,0,1,1),
  # age_child_16_1 & age_parent_16 are highly correlated
  age_child_16_1 = c(1,1,1,1,0,0),
  age_parent_16 = c(1,1,1,1,0,0)
  
)
names(test_s) <- rownames(predMatrix)
rownames(test_s) <- rownames(predMatrix)
test_s <- as.matrix(test_s)
stopifnot(
  all.equal(
    s,
    test_s
  )
)
rm(list=c("test_s","s","corr_mat", "predMatrix", "test_df"))



test <- data.frame(
  mpvs_total_14_1 = c(19,19,19,10),
  mpvs_item_1_14_1=c(0,1,2,3),
  mpvs_item_2_14_1=c(-1,2,-4,0),
  mpvs_total_16_1 = c(19,19,19,10),
  mpvs_item_1_16_1=c(0,1,2,3),
  mpvs_item_2_16_1=c(-1,2,-4,0),
  age = c(1,2,3,4),
  sex = c(1,0,1,1)
)
predMatrix <- mice::make.predictorMatrix(test)
modify_pred_matrix_items_predict_nothing <- function(
    df,target_phrase,target_phrase2
){
  
  all_items <- df[,colnames(df)[grepl(
    pattern=target_phrase,x=colnames(df)
  )]]
  all_items <- all_items[,colnames(all_items)[grepl(
    pattern=target_phrase2,x=colnames(all_items)
  )]]
  # Individual items can predict ONLY items from the same wave, nothing else
  df[colnames(all_items),colnames(all_items)] <- 0
  # They should not predict other vars, too (their totals should)
  df[,colnames(all_items)] <- 0
  return(df)
}

modify_pred_matrix <- function(
    df, target_phrase,target_phrase2="item",
    target_phrase_total
){

  x <- df[,colnames(df)[grepl(
    pattern=(paste0(target_phrase,"$")),x=colnames(df)
  )]]
  total_score <- colnames(x)[grepl(pattern=(target_phrase_total),x=colnames(x))]
  print(
    glue::glue("Found {length(total_score)} `{target_phrase_total}` columns;")
  )
  print(total_score)
  x <- x[,colnames(x)[grepl(pattern=(target_phrase2),x=colnames(x))]]
  print(glue::glue("Found {dim(x)[2]} `{target_phrase2}` columns;"))
  print(colnames(x))
  
  # Sum scores should not be predicted by other vars, 
  # but they can predict others, including items from other waves.
  # Individual items can predict ONLY items from the same wave, nothing else
  # Individual items can be predicted by other waves' total and other vars
  
  
  # Items from the same wave can predict other items from the same wave
  # But not themselves
  df[colnames(x),colnames(x)] <- 1
  for (item in colnames(x)){
    df[item,item] <- 0
  }
  # Items should not be predicted by the total score of the same wave
  df[colnames(x), total_score] <- 0
  # Total score should not be predicted by other vars
  df[total_score,] <- 0
  # But total scores can predict other vars and other waves' items
  
  return(df)
}  

to_test <- modify_pred_matrix_items_predict_nothing(
  df=predMatrix, target_phrase = "mpvs",
  target_phrase2="item"
)

to_test <- modify_pred_matrix(
  df=to_test, target_phrase = "14_1",
  target_phrase_total="total"
)
to_test <- modify_pred_matrix(
  df=to_test, target_phrase = "16_1",
  target_phrase_total="total"
)

s <- data.frame(
  mpvs_total_14_1=c(0,0,0,0,1,1,1,1),
  mpvs_item_1_14_1=c(0,0,1,0,0,0,0,0),
  mpvs_item_2_14_1=c(0,1,0,0,0,0,0,0),
  mpvs_total_16_1=c(0,1,1,0,0,0,1,1),
  mpvs_item_1_16_1=c(0,0,0,0,0,1,0,0),
  mpvs_item_2_16_1=c(0,0,0,0,1,0,0,0),
  age = c(0,1,1,0,1,1,0,1),
  sex = c(0,1,1,0,1,1,1,0)
)

rownames(s) <- rownames(to_test)

stopifnot(
  all.equal(
    as.matrix(s),
    to_test
  )
)
rm(list=c("s", "to_test", "test", "predMatrix"))



remove_twins_without_var <- function(
    df,
    group_var="fam_id",
    sex_var = "sex_1",
    pattern = "dcq_item",
    keep_empty_cotwin = T,
    NA_threshold = 7
){
  for (var in c(sex_var, group_var)){
    if ((var %in% colnames(df)) == F){
      stop(
        glue::glue(
          "`{var}` is not a column name of the df"
        )
      )
    }
  }
  
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  dflist <- split(df, f = list(df[,c(group_var)]), drop = TRUE)
  if(keep_empty_cotwin==F){
    df <- df %>%
      filter(
        !if_all(
          # Get the column names containing `pattern`
          c(colnames(df)[grepl(pattern=pattern, x=colnames(df))]),
          is.na
        )
      )
    return(df)
  }
  
  to_return <- lapply(
    X=dflist, FUN=function(inner_df){
      # df_twin_1 <- inner_df[1,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]
      # df_twin_2 <- inner_df[2,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]
      N_NA_twin_1 <- sum(is.na(inner_df[1,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]))
      N_NA_twin_2 <- sum(is.na(inner_df[2,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]))
      
      if ((N_NA_twin_1 == NA_threshold) & (N_NA_twin_2 == NA_threshold)){
        return(NULL)
      }
      # if (keep_empty_cotwin == F){
      #   if(
      #     (N_NA_twin_1 == NA_threshold) | (N_NA_twin_2 == NA_threshold)
      #   ){
      #     if (N_NA_twin_1 == NA_threshold){
      #       return(inner_df[2,])
      #     }
      #     if (N_NA_twin_2 == NA_threshold){
      #       return(inner_df[1,])
      #     }
      #   }else{
      #     return(inner_df)
      #   }
      # }
      if(keep_empty_cotwin == T){
        # Essentially, we keep everything
        return(inner_df)
      }
    }
  )
  # # https://stackoverflow.com/a/35951683
  to_return <- unname(to_return)
  to_return <- do.call(rbind, to_return)
  return(to_return)
}

remove_twins_without_var_parallel <- function(
    df,
    group_var="fam_id",
    sex_var = "sex_1",
    pattern = "dcq_item",
    keep_empty_cotwin = T,
    NA_threshold = 7,
    cl 
){
  for (var in c(sex_var, group_var)){
    if ((var %in% colnames(df)) == F){
      stop(
        glue::glue(
          "`{var}` is not a column name of the df"
        )
      )
    }
  }
  
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  dflist <- split(df, f = list(df[,c(group_var)]), drop = TRUE)
  if(keep_empty_cotwin==F){
    df <- df %>%
      #filter(!if_all(colnames(df), is.na))
      filter(
        !if_all(
          # Get the column names containing "mpvs"
          c(colnames(df)[grepl(pattern=pattern, x=colnames(df))]),
          is.na
        )
      )
    return(df)
  }
  
  parallel::clusterExport(
    cl=cl, 
    varlist=list(
      "cl"
    )
  )
  
  to_return <- parallel::parLapplyLB(
    cl = cl, 
    X=dflist, 
    fun = function(inner_df, NA_threshold, pattern, keep_empty_cotwin){
      N_NA_twin_1 <- sum(is.na(inner_df[1,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]))
      N_NA_twin_2 <- sum(is.na(inner_df[2,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]))
      
      if ((N_NA_twin_1 == NA_threshold) & (N_NA_twin_2 == NA_threshold)){
        return(NULL)
      }
      if(keep_empty_cotwin == T){
        # Essentially, we keep everything
        return(inner_df)
      }
    },
    NA_threshold,
    pattern,
    keep_empty_cotwin
  )
  
  # # https://stackoverflow.com/a/35951683
  to_return <- unname(to_return)
  to_return <- do.call(rbind, to_return)
  return(to_return)
}

inner <- function(inner_df,pattern,NA_threshold,keep_empty_cotwin){
  N_NA_twin_1 <- sum(is.na(inner_df[1,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]))
  N_NA_twin_2 <- sum(is.na(inner_df[2,colnames(inner_df)[grepl(pattern=pattern, x=colnames(inner_df))]]))
  if ((N_NA_twin_1 == NA_threshold) & (N_NA_twin_2 == NA_threshold)){
    return(data.frame(NULL))
  }
  if(keep_empty_cotwin == T){
    # Essentially, we keep everything
    return(inner_df)
  }
  return(inner_df)
}

remove_twins_without_var2 <- function(
    df,
    group_var="fam_id",
    sex_var = "sex_1",
    pattern = "dcq_item",
    keep_empty_cotwin = T,
    NA_threshold = 7
){
  for (var in c(sex_var, group_var)){
    if ((var %in% colnames(df)) == F){
      stop(
        glue::glue(
          "`{var}` is not a column name of the df"
        )
      )
    }
  }
  
  if ("tbl_df" %in% class(df)){
    df <- as.data.frame(df)
  }
  if(keep_empty_cotwin==F){
    df <- df %>%
      #filter(!if_all(colnames(df), is.na))
      filter(
        !if_all(
          # Get the column names containing "mpvs"
          c(colnames(df)[grepl(pattern=pattern, x=colnames(df))]),
          is.na
        )
      )
    return(df)
  }
  
  if(keep_empty_cotwin==T){
    df <- df %>% 
      group_by(.data[[!!group_var]]) %>% 
      group_modify(
        ~ inner(.x,pattern,NA_threshold,keep_empty_cotwin)
      ) %>% ungroup()
  }
  
  # # https://stackoverflow.com/a/35951683
  # to_return <- unname(to_return)
  # to_return <- do.call(rbind, to_return)
  return(df %>% as.data.frame())
}
test <- data.frame(
  fam_id = c(1,1,2,2,3,3,4,4),
  sex = c(0,0,1,1,0,1,1,1),
  test_var = c(1:5,NA,NA,NA),
  test_var2 = c(1,1,1,NA,NA,NA,NA,NA),
  test_var3 = c(1,NA,1,NA,1,NA,NA,NA)
)
testit <- remove_twins_without_var(
  df=test, keep_empty_cotwin=T,
  sex_var="sex",NA_threshold=3, pattern="test"
)
stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      sex = c(0,0,1,1,0,1),
      test_var = c(1:5,NA),
      test_var2 = c(1,1,1,NA,NA,NA),
      test_var3 = c(1,NA,1,NA,1,NA)
    )
  )
)

testit <- remove_twins_without_var(
  df=test, keep_empty_cotwin=F,
  sex_var="sex",NA_threshold=3, pattern="test"
)

stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(1,1,2,2,3),
      sex = c(0,0,1,1,0),
      test_var = c(1:5),
      test_var2 = c(1,1,1,NA,NA),
      test_var3 = c(1,NA,1,NA,1)
    )
  )
)

rm(list=c("test", "testit"))


# Test parallel fun
cl <- parallel::makeCluster(parallel::detectCores() - 1)
test <- data.frame(
  fam_id = c(1,1,2,2,3,3,4,4),
  sex = c(0,0,1,1,0,1,1,1),
  test_var = c(1:5,NA,NA,NA),
  test_var2 = c(1,1,1,NA,NA,NA,NA,NA),
  test_var3 = c(1,NA,1,NA,1,NA,NA,NA)
)
testit <- remove_twins_without_var_parallel(
  df=test, keep_empty_cotwin=T,
  sex_var="sex",NA_threshold=3, pattern="test",cl=cl
)
stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(1,1,2,2,3,3),
      sex = c(0,0,1,1,0,1),
      test_var = c(1:5,NA),
      test_var2 = c(1,1,1,NA,NA,NA),
      test_var3 = c(1,NA,1,NA,1,NA)
    )
  )
)

testit <- remove_twins_without_var_parallel(
  df=test, keep_empty_cotwin=F,
  sex_var="sex",NA_threshold=3, pattern="test",cl=cl
)
testit

stopifnot(
  all.equal(
    testit,
    data.frame(
      fam_id = c(1,1,2,2,3),
      sex = c(0,0,1,1,0),
      test_var = c(1:5),
      test_var2 = c(1,1,1,NA,NA),
      test_var3 = c(1,NA,1,NA,1)
    )
  )
)

parallel::stopCluster(cl=cl)
rm(list=c("test", "testit", "cl"))
