library(haven)
library(tidyverse)

# The data is saved in an encrypted file
df_raw <- read_sav("G:\\693 Georgina Krebs BDD and victimisation Jan2025.sav")

# Create a new df to store the variables that we will need
df <- data.frame(dcq_26_1=rep(NA, nrow(df_raw)))


# DCQ total score at age 26 
# Total scale, from all 7 items of the DCQ-BDD measure in the twin MHQ. 
# Each item has values 0/1/2/3, hence the scale values have range 0 to 21.
# See: (https://datadictionary.teds.ac.uk/studies/derived_variables/26yr_derived_variables.htm#zmhbddt)
df$dcq_26_1 <- df_raw$zmhbddt1
df$dcq_26_2 <- df_raw$zmhbddt2

df$bdd_diagnosis_26_1 <- df_raw$zmhmhddx1m1
df$bdd_diagnosis_26_2 <- df_raw$zmhmhddx1m2
table(df$bdd_diagnosis_26_1, df$bdd_diagnosis_26_2)



# MPVS at 16 years
# See: https://datadictionary.teds.ac.uk/studies/derived_variables/16yr_derived_variables.htm#pcpevit
df$mpvs_total_16_1 <- df_raw$pcpevit1
df$mpvs_total_16_2 <- df_raw$pcpevit2



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












# Last but not least, drop the rows that contain NA across all columns only!
# See: https://stackoverflow.com/a/70325350

df <- df %>%
  filter(!if_all(colnames(df), is.na))



