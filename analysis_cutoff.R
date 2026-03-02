library("sandwich")
library("lmtest")

####################
# Use of mean mpvs #
####################

df_essential_vars %>% explore::describe(dcq_total_26_1_cutoff17)
df_essential_vars %>% explore::describe(mpvs_mean_scaled_32)

# 11p cut-off
df_1 %>% explore::describe(dcq_total_26_1_cutoff11)

fit_mean_mpvs_scaled_32_cutoff11 <- glm(
  dcq_total_26_1_cutoff11 ~ mpvs_mean_scaled_32 + sex_1_fct,
  data = df_essential_vars,
  family = binomial()
)
summary(fit_mean_mpvs_scaled_32_cutoff11)


# See https://stackoverflow.com/a/61052072
# get results with clustered standard errors (of type HC0)
coeftest_results_mean_mpvs_scaled_32 <- coeftest(
  fit_mean_mpvs_scaled_32_cutoff11,
  vcov. = vcovCL(
    fit_mean_mpvs_scaled_32_cutoff11,
    cluster = ~twin_id, type = "HC0"
  )
)
# See https://cran.r-project.org/web/packages/mets/vignettes/glm-utility.html
summary_fit_mean_mpvs_scaled_32_cutoff11 <- mets::summaryGLM(
  fit_mean_mpvs_scaled_32_cutoff11,
  id = df_essential_vars$twin_id
)
summary_fit_mean_mpvs_scaled_32_cutoff11

# 17p cutoff
df_1 %>% explore::describe(dcq_total_26_1_cutoff17)

fit_mean_mpvs_scaled_32_cutoff17 <- glm(
  dcq_total_26_1_cutoff17 ~ mpvs_mean_scaled_32 + sex_1_fct,
  data = df_essential_vars,
  family = binomial()
)
summary(fit_mean_mpvs_scaled_32_cutoff17)


# See https://stackoverflow.com/a/61052072
# get results with clustered standard errors (of type HC0)
coeftest_results_mean_mpvs_scaled_32 <- coeftest(
  fit_mean_mpvs_scaled_32_cutoff17,
  vcov. = vcovCL(
    fit_mean_mpvs_scaled_32_cutoff17,
    cluster = ~twin_id, type = "HC0"
  )
)
# See https://cran.r-project.org/web/packages/mets/vignettes/glm-utility.html
summary_fit_mean_mpvs_scaled_32_cutoff17 <- mets::summaryGLM(
  fit_mean_mpvs_scaled_32_cutoff17,
  id = df_essential_vars$twin_id
)


##########################
# Use of latest mpvs var #
##########################
fit_mpvs_total_21_scaled_32 <- glm(
  dcq_total_26_1_cutoff17 ~ mpvs_total_phase_2_21_1_scaled_32 + sex_1_fct,
  data = df_essential_vars,
  family = binomial()
)
summary(fit_mpvs_total_21_scaled_32)


# See https://stackoverflow.com/a/61052072
# get results with clustered standard errors (of type HC0)
coeftest_results_mean_total_21_scaled_32 <- coeftest(
  fit_mpvs_total_21_scaled_32,
  vcov. = vcovCL(
    fit_mpvs_total_21_scaled_32,
    cluster = ~twin_id, type = "HC0"
  )
)
# See https://cran.r-project.org/web/packages/mets/vignettes/glm-utility.html
mets::summaryGLM(fit_mpvs_total_21_scaled_32, id = df_essential_vars$twin_id)
