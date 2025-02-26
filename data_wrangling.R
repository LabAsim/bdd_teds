library(haven)
library(tidyverse)

# The data is saved in an encrypted file
if(exists("df_raw")==F){
  # Load it just once
  df_raw <- read_sav("G:\\693 Georgina Krebs BDD and victimisation Jan2025.sav")
}

#############
# IMPORTANT #
#############

# See: https://datadictionary.teds.ac.uk/studies/data_processing/data_processing.htm#Double_entering_parent
# The variables having names ending 
# in '1' contain data for this twin (whether elder or younger). 
# The variables having names ending in '2' contain data for this twin's co-twin.


# Create a new df to store the variables that we will need
df <- data.frame(to_remove=rep(NA, nrow(df_raw)))

#################
# Miscellaneous #
#################

df$twin_id <- df_raw$randomtwinid
df$fam_id <- df_raw$randomfamid
df$twin_order <- df_raw$twin
df$random_twin_from_pair <- df_raw$random
df$school_cohort <- df_raw$cohort
df$sex_1 <- df_raw$sex1
df$sex_2 <- df_raw$sex2
df$zygosity_binary <- df_raw$zygos
df$zygosity_ternary <- df_raw$x3zygos
df$zygosity_quinary <- df_raw$sexzyg

##########
# AGE 12 #
##########

# MPVS at the age of 12
# See https://datadictionary.teds.ac.uk/studies/measures/12yr_measures.htm
# lcvicph1/2, lcvicpr1/2, lcvicso1/2, lcvicve1/2
df$mpvs_physical_12_1 <- df_raw$lcvicph1
df$mpvs_physical_12_2 <- df_raw$lcvicph2
df$mpvs_verbal_12_1 <- df_raw$lcvicve1
df$mpvs_verbal_12_2 <- df_raw$lcvicve2
df$mpvs_social_12_1 <- df_raw$lcvicso1
df$mpvs_social_12_2 <- df_raw$lcvicso2
df$mpvs_property_12_1 <- df_raw$lcvicpr1 
df$mpvs_property_12_2 <- df_raw$lcvicpr2

table(df$mpvs_physical_12_1, df$mpvs_verbal_12_1, useNA = "always")

df$mpvs_total_12_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_12_1",
    "mpvs_verbal_12_1",
    "mpvs_social_12_1",
    "mpvs_property_12_1"
  )]
)

# Age
df$age_parent_12 <- df_raw$lpqage
df$age_teach_12_1 <- df_raw$ltqage1
df$age_teach_12_2 <- df_raw$ltqage2
df$age_child_12_1 <- df_raw$lcqage1
df$age_child_12_2 <- df_raw$lcqage2


##########
# AGE 14 #
##########

# MPVS at the age of 14
# See: https://datadictionary.teds.ac.uk/studies/derived_variables/14yr_derived_variables.htm#vic
df$mpvs_physical_14_parent_1 <- df_raw$npvicph1
df$mpvs_physical_14_parent_2 <- df_raw$npvicph2
df$mpvs_verbal_14_parent_1 <- df_raw$npvicve1
df$mpvs_verbal_14_parent_2 <- df_raw$npvicve2
df$mpvs_social_14_parent_1 <- df_raw$npvicso1
df$mpvs_social_14_parent_2 <- df_raw$npvicso2
df$mpvs_property_14_parent_1 <- df_raw$npvicpr1 
df$mpvs_property_14_parent_2 <- df_raw$npvicpr2

df$mpvs_total_14_parent_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_14_parent_1",
    "mpvs_verbal_14_parent_1",
    "mpvs_social_14_parent_1",
    "mpvs_property_14_parent_1"
  )]
)

df$mpvs_physical_14_child_1 <- df_raw$ncvicph1
df$mpvs_physical_14_child_2 <- df_raw$ncvicph2
df$mpvs_verbal_14_child_1 <- df_raw$ncvicve1
df$mpvs_verbal_14_child_2 <- df_raw$ncvicve2
df$mpvs_social_14_child_1 <- df_raw$ncvicso1
df$mpvs_social_14_child_2 <- df_raw$ncvicso2
df$mpvs_property_14_child_1 <- df_raw$ncvicpr1 
df$mpvs_property_14_child_2 <- df_raw$ncvicpr2

df$mpvs_total_14_child_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_14_child_1",
    "mpvs_verbal_14_child_1",
    "mpvs_social_14_child_1",
    "mpvs_property_14_child_1"
  )]
)

df$mpvs_physical_14_teacher_1 <- df_raw$ntvicph1
df$mpvs_physical_14_teacher_2 <- df_raw$ntvicph2
df$mpvs_verbal_14_teacher_1 <- df_raw$ntvicve1
df$mpvs_verbal_14_teacher_2 <- df_raw$ntvicve2
df$mpvs_social_14_teacher_1 <- df_raw$ntvicso1
df$mpvs_social_14_teacher_2 <- df_raw$ntvicso2
df$mpvs_property_14_teacher_1 <- df_raw$ntvicpr1 
df$mpvs_property_14_teacher_2 <- df_raw$ntvicpr2

df$mpvs_total_14_teacher_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_14_teacher_1",
    "mpvs_verbal_14_teacher_1",
    "mpvs_social_14_teacher_1",
    "mpvs_property_14_teacher_1"
  )]
)



# Age

df$age_parent_14 <- df_raw$npqage
df$age_teach_14_1 <- df_raw$ntqage1
df$age_teach_14_2 <- df_raw$ntqage2
df$age_child_14_1 <- df_raw$ncqage1
df$age_child_14_2 <- df_raw$ncqage2



##########
# AGE 16 #
##########

# MPVS at 16 years
# See: https://datadictionary.teds.ac.uk/studies/derived_variables/16yr_derived_variables.htm#pcpevit
df$mpvs_total_16_1 <- df_raw$pcpevit1
df$mpvs_total_16_2 <- df_raw$pcpevit2

# Eating Disorders Diagnostic Scale
df$eat_dis_scale_16_1 <- df_raw$pcbheddsm1
df$eat_dis_scale_16_2 <- df_raw$pcbheddsm2

# Age
df$age_web_16_1 <- df_raw$pcwebage1
df$age_web_16_2 <- df_raw$pcwebage2
df$age_child_16_1 <- df_raw$pcbhage1
df$age_child_16_2 <- df_raw$pcbhage2
df$age_parent_16 <- df_raw$ppbhage
df$age_leap_study_parent_16 <- df_raw$ppl2age

##########
# AGE 21 #
##########

# MPVS at 21 years
# See: https://datadictionary.teds.ac.uk/studies/derived_variables/21yr_derived_variables.htm#u2cvict
df$cyber_bullying_phase1_1 <- df_raw$u1cobult1
df$cyber_bullying_phase1_2 <- df_raw$u1cobult2
df$mpvs_total_21_phase_2_1 <- df_raw$u2cvictt1
df$mpvs_total_21_phase_2_2 <- df_raw$u2cvictt2
df$mpvs_total_21_cov1_1<- df_raw$ucv1victt1
df$mpvs_total_21_cov1_2<- df_raw$ucv1victt2
df$mpvs_total_21_cov2_1<- df_raw$ucv2victt1
df$mpvs_total_21_cov2_2<- df_raw$ucv2victt2
df$mpvs_total_21_cov3_1<- df_raw$ucv3victt1
df$mpvs_total_21_cov3_2<- df_raw$ucv3victt2
df$mpvs_total_21_cov4_1<- df_raw$ucv4victt1
df$mpvs_total_21_cov4_2<- df_raw$ucv4victt2


# Anorexia nervosa diagnosis
df$anorexia_diag_21_phase1_1 <- df_raw$u1ceatd11
df$anorexia_diag_21_phase1_2 <- df_raw$u1ceatd12
# Bulimia
df$bulimia_diag_21_phase1_1 <- df_raw$u1ceatd21
df$bulimia_diag_21_phase1_2 <- df_raw$u1ceatd22
# Binge eating disorder
df$binge_eat_diag_21_phase1_1 <- df_raw$u1ceatd31
df$binge_eat_diag_21_phase1_2 <- df_raw$u1ceatd32

# Eating disorders symptoms scale
# https://datadictionary.teds.ac.uk/studies/derived_variables/21yr_derived_variables.htm#eats

# Binge-eating total score (TEDS21 phase 1 twin qnr), 0-15
df$bing_eat_scale_phase1_1 <- df_raw$u1ceatsbint1
df$bing_eat_scale_phase1_2 <- df_raw$u1ceatsbint2

# Body preoccupation total score (0-40)
df$body_preoccup_phase1_1 <- df_raw$u1ceatsbodt1
df$body_preoccup_phase1_2 <- df_raw$u1ceatsbodt2

# Age
df$age_phase1_parent_21 <- df_raw$u1page
df$age_phase1_child_21_1 <- df_raw$u1cage1
df$age_phase1_child_21_2 <- df_raw$u1cage2
df$age_phase2_child_21_1 <- df_raw$u2cage1
df$age_phase2_child_21_2 <- df_raw$u2cage2
df$age_cov1_child_21_1 <- df_raw$ucv1age1
df$age_cov1_child_21_2 <- df_raw$ucv1age2
df$age_cov2_child_21_1 <- df_raw$ucv2age1
df$age_cov2_child_21_2 <- df_raw$ucv2age2
df$age_cov3_child_21_1 <- df_raw$ucv3age1
df$age_cov3_child_21_2 <- df_raw$ucv3age2
df$age_cov4_child_21_1 <- df_raw$ucv4age1
df$age_cov4_child_21_2 <- df_raw$ucv4age2



##########
# AGE 26 #
##########

# DCQ total score at age 26 
# Total scale, from all 7 items of the DCQ-BDD measure in the twin MHQ. 
# Each item has values 0/1/2/3, hence the scale values have range 0 to 21.
# See: (https://datadictionary.teds.ac.uk/studies/derived_variables/26yr_derived_variables.htm#zmhbddt)
df$dcq_26_1 <- df_raw$zmhbddt1
df$dcq_26_2 <- df_raw$zmhbddt2

df$bdd_diagnosis_26_1 <- df_raw$zmhmhddx1m1
df$bdd_diagnosis_26_2 <- df_raw$zmhmhddx1m2
table(df$bdd_diagnosis_26_1, df$bdd_diagnosis_26_2)

# Age
df$age_26_1 <- df_raw$zmhage1
df$age_26_2 <- df_raw$zmhage2




# Last but not least, drop only the rows that contain NA in MPVS columns!
# We can impute the remaining NA!
# See: https://stackoverflow.com/a/70325350
# https://www.geeksforgeeks.org/how-to-check-if-characters-are-present-in-a-string-in-r/

df <- df %>%
  #filter(!if_all(colnames(df), is.na))
  filter(
    !if_all(
      # Get the column names containing "mpvs"
      colnames(df)[grepl(pattern="mpvs", x=colnames(df))],
      is.na
    )
  )

df <- df %>% dplyr::select(-all_of("to_remove"))


# Drop the cotwin variables
df_12 <- df
df_1 <- df %>% select(!matches("_2$"))

source("helper.R")


df_1 <- df_1 %>% fill_multiple_vars_twin_from_cotwin(
  vars=c(
    colnames(
      df_1
    )[grepl(pattern="age", x=colnames(df))] %>% purrr::discard(is.na)
  ) 
)

df_1 <- fill_var(
  df=df_1,
  primary = "age_parent_12",
  secondary = "age_child_12_1",
  tertiary = "age_teach_12_1",
  new_column = "age_12_1"
)

df_1 <- fill_var(
  df=df_1,
  primary = "age_parent_14",
  secondary = "age_child_14_1",
  tertiary = "age_teach_14_1",
  new_column = "age_14_1"
)

# At age 16, MPVS questionnaire  was answered only by the twins
# (cohort 1 & 2) during the web study.
# From TEDS' website: https://datadictionary.teds.ac.uk/studies/16yr.htm
# The initial booklet study was administered in two waves: 
# wave 1 (cohort 1), called the Behaviour study, 
# started immediately after the end of cohort 1's web study; 
# wave 2 (cohorts 2, 3 and 4), called the LEAP study, 
# started after the end of cohort 2's web study. 
# Twin ages ranged from roughly 15 (cohort 4) up to 17.5 (cohorts 1 and 2) 
# when booklets were returned. 
# Thus, first, we need age_web_16_1. 
# If NA exists, we could pull age from parent and then, from child.


df_1 <- fill_var(
  df=df_1,
  primary = "age_web_16_1",
  secondary = "age_parent_16",
  tertiary = "age_child_16_1",
  new_column = "age_16_1"
)


# At age 21, MPVS questionnaire was answered twin phase1,
# Covid phase 1,2,3 & 4.

df_1 <- fill_age_covid_21(df=df_1)
df_1 <- fill_age_covid_21(df=df_1, order="desceding")


# Fill age from co-twin 
df_1 <- df_1 %>% fill_multiple_vars_twin_from_cotwin(
  vars=c(
    colnames(
      df_1
    )[grepl(pattern="age_cov", x=colnames(df_1))] %>% purrr::discard(is.na)
  ) 
)


df_1 <- df_1 %>% fill_multiple_vars_twin_from_cotwin(
  vars=c(
    colnames(
      df_1
    )[grepl(pattern="age_phase", x=colnames(df_1))] %>% purrr::discard(is.na)
  ) 
)
