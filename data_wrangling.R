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
df$ses_1st_contact <- df_raw$ases
df$ethnic <- df_raw$aethnic
df$exclude1 <- df_raw$exclude1 
df$exclude2 <- df_raw$exclude2

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

df$mpvs_item_1_12_1 <- df_raw$lcvic011
df$mpvs_item_2_12_1 <- df_raw$lcvic021
df$mpvs_item_3_12_1 <- df_raw$lcvic031
df$mpvs_item_4_12_1 <- df_raw$lcvic041
df$mpvs_item_5_12_1 <- df_raw$lcvic051
df$mpvs_item_6_12_1 <- df_raw$lcvic061
df$mpvs_item_7_12_1 <- df_raw$lcvic071
df$mpvs_item_8_12_1 <- df_raw$lcvic081
df$mpvs_item_9_12_1 <- df_raw$lcvic091
df$mpvs_item_10_12_1 <- df_raw$lcvic101
df$mpvs_item_11_12_1 <- df_raw$lcvic111
df$mpvs_item_12_12_1 <- df_raw$lcvic121
df$mpvs_item_13_12_1 <- df_raw$lcvic131
df$mpvs_item_14_12_1 <- df_raw$lcvic141
df$mpvs_item_15_12_1 <- df_raw$lcvic151
df$mpvs_item_16_12_1 <- df_raw$lcvic161

table(df$mpvs_physical_12_1, df$mpvs_verbal_12_1, useNA = "always")

df$mpvs_total_12_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_12_1",
    "mpvs_verbal_12_1",
    "mpvs_social_12_1",
    "mpvs_property_12_1"
  )],
  na.rm = F
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
df$mpvs_physical_parent_14_1 <- df_raw$npvicph1
df$mpvs_physical_parent_14_2 <- df_raw$npvicph2
df$mpvs_verbal_parent_14_1 <- df_raw$npvicve1
df$mpvs_verbal_parent_14_2 <- df_raw$npvicve2
df$mpvs_social_parent_14_1 <- df_raw$npvicso1
df$mpvs_social_parent_14_2 <- df_raw$npvicso2
df$mpvs_property_parent_14_1 <- df_raw$npvicpr1 
df$mpvs_property_parent_14_2 <- df_raw$npvicpr2

df$mpvs_item_1_parent_14_1 <- df_raw$npvic011
df$mpvs_item_2_parent_14_1 <- df_raw$npvic021
df$mpvs_item_3_parent_14_1 <- df_raw$npvic031
df$mpvs_item_4_parent_14_1 <- df_raw$npvic041
df$mpvs_item_5_parent_14_1 <- df_raw$npvic051
df$mpvs_item_6_parent_14_1 <- df_raw$npvic061
df$mpvs_item_7_parent_14_1 <- df_raw$npvic071
df$mpvs_item_8_parent_14_1 <- df_raw$npvic081
df$mpvs_item_9_parent_14_1 <- df_raw$npvic091
df$mpvs_item_10_parent_14_1 <- df_raw$npvic101
df$mpvs_item_11_parent_14_1 <- df_raw$npvic111
df$mpvs_item_12_parent_14_1 <- df_raw$npvic121
df$mpvs_item_13_parent_14_1 <- df_raw$npvic131
df$mpvs_item_14_parent_14_1 <- df_raw$npvic141
df$mpvs_item_15_parent_14_1 <- df_raw$npvic151
df$mpvs_item_16_parent_14_1 <- df_raw$npvic161


df$mpvs_total_parent_14_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_parent_14_1",
    "mpvs_verbal_parent_14_1",
    "mpvs_social_parent_14_1",
    "mpvs_property_parent_14_1"
  )]
)

df$mpvs_physical_child_14_1 <- df_raw$ncvicph1
df$mpvs_physical_child_14_2 <- df_raw$ncvicph2
df$mpvs_verbal_child_14_1 <- df_raw$ncvicve1
df$mpvs_verbal_child_14_2 <- df_raw$ncvicve2
df$mpvs_social_child_14_1 <- df_raw$ncvicso1
df$mpvs_social_child_14_2 <- df_raw$ncvicso2
df$mpvs_property_child_14_1 <- df_raw$ncvicpr1 
df$mpvs_property_child_14_2 <- df_raw$ncvicpr2

df$mpvs_item_1_child_14_1 <-df_raw$ncvic011
df$mpvs_item_2_child_14_1 <-df_raw$ncvic021
df$mpvs_item_3_child_14_1 <-df_raw$ncvic031
df$mpvs_item_4_child_14_1 <-df_raw$ncvic041
df$mpvs_item_5_child_14_1 <-df_raw$ncvic051
df$mpvs_item_6_child_14_1 <-df_raw$ncvic061
df$mpvs_item_7_child_14_1 <-df_raw$ncvic071
df$mpvs_item_8_child_14_1 <-df_raw$ncvic081
df$mpvs_item_9_child_14_1 <-df_raw$ncvic091
df$mpvs_item_10_child_14_1  <-df_raw$ncvic101
df$mpvs_item_11_child_14_1  <-df_raw$ncvic111
df$mpvs_item_12_child_14_1  <-df_raw$ncvic121
df$mpvs_item_13_child_14_1  <-df_raw$ncvic131
df$mpvs_item_14_child_14_1  <-df_raw$ncvic141
df$mpvs_item_15_child_14_1  <-df_raw$ncvic151
df$mpvs_item_16_child_14_1  <-df_raw$ncvic161


df$mpvs_total_child_14_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_child_14_1",
    "mpvs_verbal_child_14_1",
    "mpvs_social_child_14_1",
    "mpvs_property_child_14_1"
  )]
)

df$mpvs_physical_teacher_14_1 <- df_raw$ntvicph1
df$mpvs_physical_teacher_14_2 <- df_raw$ntvicph2
df$mpvs_verbal_teacher_14_1 <- df_raw$ntvicve1
df$mpvs_verbal_teacher_14_2 <- df_raw$ntvicve2
df$mpvs_social_teacher_14_1 <- df_raw$ntvicso1
df$mpvs_social_teacher_14_2 <- df_raw$ntvicso2
df$mpvs_property_teacher_14_1 <- df_raw$ntvicpr1 
df$mpvs_property_teacher_14_2 <- df_raw$ntvicpr2

df$mpvs_item_1_teacher_14_1 <-df_raw$ntvic011
df$mpvs_item_2_teacher_14_1 <-df_raw$ntvic021
df$mpvs_item_3_teacher_14_1 <-df_raw$ntvic031
df$mpvs_item_4_teacher_14_1 <-df_raw$ntvic041
df$mpvs_item_5_teacher_14_1 <-df_raw$ntvic051
df$mpvs_item_6_teacher_14_1 <-df_raw$ntvic061
df$mpvs_item_7_teacher_14_1 <-df_raw$ntvic071
df$mpvs_item_8_teacher_14_1 <-df_raw$ntvic081
df$mpvs_item_9_teacher_14_1 <-df_raw$ntvic091
df$mpvs_item_10_teacher_14_1  <-df_raw$ntvic101
df$mpvs_item_11_teacher_14_1  <-df_raw$ntvic111
df$mpvs_item_12_teacher_14_1  <-df_raw$ntvic121
df$mpvs_item_13_teacher_14_1  <-df_raw$ntvic131
df$mpvs_item_14_teacher_14_1  <-df_raw$ntvic141
df$mpvs_item_15_teacher_14_1  <-df_raw$ntvic151
df$mpvs_item_16_teacher_14_1  <-df_raw$ntvic161


df$mpvs_total_teacher_14_1 <- rowSums(
  x=df[,c(
    "mpvs_physical_teacher_14_1",
    "mpvs_verbal_teacher_14_1",
    "mpvs_social_teacher_14_1",
    "mpvs_property_teacher_14_1"
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
df$mpvs_item_1_16_1 <-df_raw$pcqbpevi11
df$mpvs_item_2_16_1 <-df_raw$pcqbpevi21
df$mpvs_item_3_16_1 <-df_raw$pcqbpevi31
df$mpvs_item_4_16_1 <-df_raw$pcqbpevi41
df$mpvs_item_5_16_1 <-df_raw$pcqbpevi51
df$mpvs_item_6_16_1 <-df_raw$pcqbpevi61

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
df$mpvs_total_phase_2_21_1 <- df_raw$u2cvictt1
df$mpvs_total_phase_2_21_2 <- df_raw$u2cvictt2


df$mpvs_item_1_phase_2_21_1 <-df_raw$u2cvict011 
df$mpvs_item_2_phase_2_21_1 <-df_raw$u2cvict021 
df$mpvs_item_3_phase_2_21_1 <-df_raw$u2cvict031 
df$mpvs_item_4_phase_2_21_1 <-df_raw$u2cvict041 
df$mpvs_item_5_phase_2_21_1 <-df_raw$u2cvict051 
df$mpvs_item_6_phase_2_21_1 <-df_raw$u2cvict061 
df$mpvs_item_7_phase_2_21_1 <-df_raw$u2cvict071 
df$mpvs_item_8_phase_2_21_1 <-df_raw$u2cvict081 
df$mpvs_item_9_phase_2_21_1 <-df_raw$u2cvict091 
df$mpvs_item_10_phase_2_21_1  <-df_raw$u2cvict101
df$mpvs_item_11_phase_2_21_1  <-df_raw$u2cvict111
df$mpvs_item_12_phase_2_21_1  <-df_raw$u2cvict121
df$mpvs_item_13_phase_2_21_1  <-df_raw$u2cvict131
df$mpvs_item_14_phase_2_21_1  <-df_raw$u2cvict141
df$mpvs_item_15_phase_2_21_1  <-df_raw$u2cvict151
df$mpvs_item_16_phase_2_21_1  <-df_raw$u2cvict161

df$mpvs_item_1_cov1_21_1 <-df_raw$ucv1vict011
df$mpvs_item_2_cov1_21_1 <-df_raw$ucv1vict021
df$mpvs_item_3_cov1_21_1 <-df_raw$ucv1vict031
df$mpvs_item_4_cov1_21_1 <-df_raw$ucv1vict041
df$mpvs_item_5_cov1_21_1 <-df_raw$ucv1vict051
df$mpvs_item_6_cov1_21_1 <-df_raw$ucv1vict061
df$mpvs_item_7_cov1_21_1 <-df_raw$ucv1vict071
df$mpvs_item_8_cov1_21_1 <-df_raw$ucv1vict081
df$mpvs_item_9_cov1_21_1 <-df_raw$ucv1vict091
df$mpvs_item_10_cov1_21_1 <-df_raw$ucv1vict101
df$mpvs_item_11_cov1_21_1 <-df_raw$ucv1vict111
df$mpvs_item_12_cov1_21_1 <-df_raw$ucv1vict121
df$mpvs_item_1_cov2_21_1 <-df_raw$ucv2vict011
df$mpvs_item_2_cov2_21_1 <-df_raw$ucv2vict021
df$mpvs_item_3_cov2_21_1 <-df_raw$ucv2vict031
df$mpvs_item_4_cov2_21_1 <-df_raw$ucv2vict041
df$mpvs_item_5_cov2_21_1 <-df_raw$ucv2vict051
df$mpvs_item_6_cov2_21_1 <-df_raw$ucv2vict061
df$mpvs_item_7_cov2_21_1 <-df_raw$ucv2vict071
df$mpvs_item_8_cov2_21_1 <-df_raw$ucv2vict081
df$mpvs_item_9_cov2_21_1 <-df_raw$ucv2vict091
df$mpvs_item_10_cov2_21_1 <-df_raw$ucv2vict101
df$mpvs_item_11_cov2_21_1 <-df_raw$ucv2vict111
df$mpvs_item_12_cov2_21_1 <-df_raw$ucv2vict121
df$mpvs_item_1_cov3_21_1 <-df_raw$ucv3vict011
df$mpvs_item_2_cov3_21_1 <-df_raw$ucv3vict021
df$mpvs_item_3_cov3_21_1 <-df_raw$ucv3vict031
df$mpvs_item_4_cov3_21_1 <-df_raw$ucv3vict041
df$mpvs_item_5_cov3_21_1 <-df_raw$ucv3vict051
df$mpvs_item_6_cov3_21_1 <-df_raw$ucv3vict061
df$mpvs_item_7_cov3_21_1 <-df_raw$ucv3vict071
df$mpvs_item_8_cov3_21_1 <-df_raw$ucv3vict081
df$mpvs_item_9_cov3_21_1 <-df_raw$ucv3vict091
df$mpvs_item_10_cov3_21_1 <-df_raw$ucv3vict101
df$mpvs_item_11_cov3_21_1 <-df_raw$ucv3vict111
df$mpvs_item_12_cov3_21_1 <-df_raw$ucv3vict121
df$mpvs_item_1_cov4_21_1 <-df_raw$ucv4vict011
df$mpvs_item_2_cov4_21_1 <-df_raw$ucv4vict021
df$mpvs_item_3_cov4_21_1 <-df_raw$ucv4vict031
df$mpvs_item_4_cov4_21_1 <-df_raw$ucv4vict041
df$mpvs_item_5_cov4_21_1 <-df_raw$ucv4vict051
df$mpvs_item_6_cov4_21_1 <-df_raw$ucv4vict061
df$mpvs_item_7_cov4_21_1 <-df_raw$ucv4vict071
df$mpvs_item_8_cov4_21_1 <-df_raw$ucv4vict081
df$mpvs_item_9_cov4_21_1 <-df_raw$ucv4vict091
df$mpvs_item_10_cov4_21_1 <-df_raw$ucv4vict101
df$mpvs_item_11_cov4_21_1 <-df_raw$ucv4vict111
df$mpvs_item_12_cov4_21_1 <-df_raw$ucv4vict121

df$mpvs_total_cov1_21_1<- df_raw$ucv1victt1
df$mpvs_total_cov1_21_2<- df_raw$ucv1victt2
df$mpvs_total_cov2_21_1<- df_raw$ucv2victt1
df$mpvs_total_cov2_21_2<- df_raw$ucv2victt2
df$mpvs_total_cov3_21_1<- df_raw$ucv3victt1
df$mpvs_total_cov3_21_2<- df_raw$ucv3victt2
df$mpvs_total_cov4_21_1<- df_raw$ucv4victt1
df$mpvs_total_cov4_21_2<- df_raw$ucv4victt2

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
df$dcq_total_26_1 <- df_raw$zmhbddt1
df$dcq_total_26_2 <- df_raw$zmhbddt2

df$dcq_item_1_26_1 <- df_raw$zmhbdd11
df$dcq_item_2_26_1 <- df_raw$zmhbdd21
df$dcq_item_3_26_1 <- df_raw$zmhbdd31
df$dcq_item_4_26_1 <- df_raw$zmhbdd41
df$dcq_item_5_26_1 <- df_raw$zmhbdd51
df$dcq_item_6_26_1 <- df_raw$zmhbdd61
df$dcq_item_7_26_1 <- df_raw$zmhbdd71

df$bdd_diagnosis_26_1 <- df_raw$zmhmhddx1m1
df$bdd_diagnosis_26_2 <- df_raw$zmhmhddx1m2
table(df$bdd_diagnosis_26_1, df$bdd_diagnosis_26_2)

# Age
df$age_26_1 <- df_raw$zmhage1
df$age_26_2 <- df_raw$zmhage2



#######################
# Drop excluded twins #
#######################
df <- df[df$exclude1 == 0,]
df <- df[df$exclude2 == 0,]

############################################################################
# Last but not least, drop only the rows that contain NA in MPVS columns!
# We can impute the remaining NA!
# See: https://stackoverflow.com/a/70325350
# https://www.geeksforgeeks.org/how-to-check-if-characters-are-present-in-a-string-in-r/
############################################################################

df_raw_named <- df %>% dplyr::select(-all_of("to_remove"))

df <- df %>%
  #filter(!if_all(colnames(df), is.na))
  filter(
    !if_all(
      # Get the column names containing "mpvs"
      colnames(df)[grepl(pattern="mpvs", x=colnames(df))],
      is.na
    )
  )

source("helper.R")


df <- df %>% dplyr::select(-all_of("to_remove"))

# Drop the cotwin variables
df_12 <- df
df_1 <- df %>% select(!matches("_2$"))

rm(df)



start_time <- Sys.time()
#cl <- parallel::makeCluster(parallel::detectCores()-2)
df_1 <- remove_twins_without_var( #
  df=df_1, #
  group_var = "fam_id",
  sex_var = "sex_1",
  pattern = "dcq_item",
  keep_empty_cotwin = T,
  NA_threshold = 7#,
  #cl=cl
) 
#parallel::stopCluster(cl)
print(Sys.time()-start_time)


df_1 <- df_1 %>% fill_multiple_vars_twin_from_cotwin(
  vars=c(
    colnames(
      df_1
    )[grepl(pattern="age", x=colnames(df_1))] %>% purrr::discard(is.na)
  ) 
)

# df_1 <- fill_var(
#   df=df_1,
#   primary = "age_parent_12",
#   secondary = "age_child_12_1",
#   tertiary = "age_teach_12_1",
#   new_column = "age_12_1"
# )
# 
# df_1 <- fill_var(
#   df=df_1,
#   primary = "age_parent_14",
#   secondary = "age_child_14_1",
#   tertiary = "age_teach_14_1",
#   new_column = "age_14_1"
# )
# 
# df_1 <-fill_var(
#   df=df_1, 
#   primary = "mpvs_total_child_14_1",
#   secondary = "mpvs_total_parent_14_1",
#   tertiary = "mpvs_total_teacher_14_1",
#   new_column = "mpvs_total_14_1"
# )

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

# df_1 <- fill_var(
#   df=df_1,
#   primary = "age_web_16_1",
#   secondary = "age_parent_16",
#   tertiary = "age_child_16_1",
#   new_column = "age_16_1"
# )


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

# df_1 <- scale_mpvs(df=df_1)

# Create a variable representing mean MPVS across the waves 
# at age 21

# df_1 <- df_1 %>% 
#   mutate(
#     mpvs_total_21_1 = rowMeans(
#       select(
#         df_1,
#         mpvs_total_phase_2_21_1,
#         mpvs_total_cov1_21_1, 
#         mpvs_total_cov2_21_1, 
#         mpvs_total_cov3_21_1,
#         mpvs_total_cov4_21_1
#       ),
#       na.rm = T
#     )
#   )

# df_1 <- df_1 %>% 
#   mutate(
#     mpvs_total_21_scaled_32 = rowMeans(
#       select(
#         df_1,
#         mpvs_total_21_phase_2_1_scaled_32,
#         mpvs_total_21_cov1_1_scaled_32, 
#         mpvs_total_21_cov2_1_scaled_32, 
#         mpvs_total_21_cov3_1_scaled_32,
#         mpvs_total_21_cov4_1_scaled_32
#       ),
#       na.rm = T
#     )
#   )

# df_1 <- df_1 %>% 
#   mutate(
#     age_21_1 = rowMeans(
#       select(
#         .,
#         age_cov1_child_21_1,
#         age_cov2_child_21_1, 
#         age_cov3_child_21_1, 
#         age_cov4_child_21_1,
#         age_phase2_child_21_1
#       ),
#       na.rm = T
#     )
#   )
