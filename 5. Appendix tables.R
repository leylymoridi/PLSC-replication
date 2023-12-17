#Appendix tables - Leyly Moridi and Isabel Linzer


library(tidyverse)
library(gt)
library(haven)
library(glue)
library(fixest)
library(modelsummary)
library(dplyr)
library(broom)
library(tibble)

#Table A - Quartile means ----

##load and setup data----
data <- read_dta("data/Final_Main.dta")

data <- data |>
  filter(year >= 1987) 

##find education means by country and divide countries into quartiles ----
data[data == -99.00] <- NA

#find means by quartile
means_data <- data %>%
  filter(!is.na(covihme_ayem)) %>%  # Exclude NA values in covihme_ayem before computing quartiles
  mutate(covihme_ayem_quartile = ntile(covihme_ayem, 4)) %>%
  group_by(covihme_ayem_quartile) %>%
  summarize(mean_education = mean(covihme_ayem, na.rm = TRUE))

#create gt table
means_table <- means_data |>
  gt() |>
  fmt_number(columns = "mean_education",
             decimals = 2) |>
  cols_label(covihme_ayem_quartile = "Quartile",
             mean_education = "Years") |>
  tab_header("Table A: Mean Years of Education (male) by Quartile")

means_table |> gtsave("quartile_means.tex")


#Table B, 1990 benchmark----
data <- read_dta("data/Final_Main.dta")

#1990 ----
data <- data |>
  filter(year >= 1990) 

#find education means by country and divide countries into quartiles 
data[data == -99.00] <- NA

##find means by quartile ----
means_data <- data |>
  filter(!is.na(covihme_ayem)) |> 
  mutate(covihme_ayem_quartile = ntile(covihme_ayem, 4)) |>
  group_by(covihme_ayem_quartile) |>
  summarize(mean_education = mean(covihme_ayem, na.rm = TRUE))

means_table <- means_data |>
  gt() |>
  fmt_number(columns = "mean_education",
             decimals = 2) |>
  cols_label(covihme_ayem_quartile = "Quartile",
             mean_education = "Average Education Attainment (males)")


data <- data |>
  group_by(ccode) |>
  mutate(edu_mean_ccode = mean(covihme_ayem, na.rm = TRUE)) |>
  ungroup()

data <- data |>
  filter(!is.na(edu_mean_ccode)) |>
  mutate(quartile = ntile(edu_mean_ccode, 4))



##re-run original regressions, by quartile, 1990 ----
#add -99s back in to set up for regression for all ^cov variables
data[, grepl("^cov", names(data))] <- replace(data[, grepl("^cov", names(data))], is.na(data[, grepl("^cov", names(data))]), -99)

#CIRI by quartile-----
##CIRI simple 
###quartile 1
ciri_short_1 <- data |>
  filter(quartile == 1) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

###quartile 2
ciri_short_2 <- data |>
  filter(quartile == 2) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

###quartile 3
ciri_short_3 <- data |>
  filter(quartile == 3) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

###quartile 4
ciri_short_4 <- data |>
  filter(quartile == 4) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

##CIRI covariate-adjusted, by quartile
covars <- str_subset(colnames(data), "(^cov)")
form_ciri_long <- as.formula(glue("new_empinxavg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")) 

ciri_long_1 <- data |>
  filter(quartile ==1) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_long_2 <- data |>
  filter(quartile ==2) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_long_3 <- data |>
  filter(quartile ==3) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_long_4 <- data |>
  filter(quartile ==4) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

##create table of CIRI by quartile, 1990 ----
modelsummary(list(ciri_short_1, ciri_long_1, ciri_short_2, ciri_long_2, ciri_short_3, ciri_long_3, ciri_short_4, ciri_long_4),
             coef_omit = "^cov",
             coef_rename = ("fit_EV" = "Aid"),
             gof_map = c("nobs", "FE: ccode", "FE: year"),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))


#Get info for CIRI table
fixef_ccode <- c(ciri_short_1$fixef_sizes["ccode"], ciri_long_1$fixef_sizes["ccode"],
                 ciri_short_2$fixef_sizes["ccode"], ciri_long_2$fixef_sizes["ccode"],
                 ciri_short_3$fixef_sizes["ccode"], ciri_long_3$fixef_sizes["ccode"],
                 ciri_short_4$fixef_sizes["ccode"], ciri_long_4$fixef_sizes["ccode"])

fixef_year <- c(ciri_short_1$fixef_sizes["year"], ciri_long_1$fixef_sizes["year"],
                ciri_short_2$fixef_sizes["year"], ciri_long_2$fixef_sizes["year"],
                ciri_short_3$fixef_sizes["year"], ciri_long_3$fixef_sizes["year"],
                ciri_short_4$fixef_sizes["year"], ciri_long_4$fixef_sizes["year"])

gof_ciri_90 <- data.frame(
  Metric = c("Effect of Aid", "Standard Error", "Country Fixed Effect", "Year Fixed Effect", "Number of Countries", "Number of Years", "N"),
  Model_1 = c(round(coef(ciri_short_1)["fit_EV"], 3), round(summary(ciri_short_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_1)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short_1)$nobs),
  Model_2 = c(round(coef(ciri_short_2)["fit_EV"], 3), round(summary(ciri_short_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_2)), "Yes", "No"),
              fixef_ccode[3], fixef_year[3], summary(ciri_short_2)$nobs),
  Model_3 = c(round(coef(ciri_short_3)["fit_EV"], 3), round(summary(ciri_short_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short_3)$nobs),
  Model_4 = c(round(coef(ciri_short_4)["fit_EV"], 3), round(summary(ciri_short_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short_4)$nobs),
  Model_5 = c(round(coef(ciri_long_1)["fit_EV"], 3), round(summary(ciri_long_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_1)), "Yes", "No"),
              fixef_ccode[2], fixef_year[2], summary(ciri_long_1)$nobs),
  Model_6 = c(round(coef(ciri_long_2)["fit_EV"], 3), round(summary(ciri_long_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_2)), "Yes", "No"),
              fixef_ccode[4], fixef_year[4], summary(ciri_long_2)$nobs),
  Model_7 = c(round(coef(ciri_long_3)["fit_EV"], 3), round(summary(ciri_long_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_long_3)$nobs),
  Model_8 = c(round(coef(ciri_long_4)["fit_EV"], 3), round(summary(ciri_long_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_long_4)$nobs))

##Create a CIRI 1990 gt table
ciri_table_90 <- gt(gof_ciri_90)

#Update titles and labels
ciri_table_90 <- ciri_table_90 %>%
  cols_label(
    Metric = "CIRI",
    Model_1 = "Q1",
    Model_2 = "Q2",
    Model_3 = "Q3",
    Model_4 = "Q4",
    Model_5 = "Q1",
    Model_6 = "Q2",
    Model_7 = "Q3",
    Model_8 = "Q4",
  ) |>
  tab_spanner(
    label = "Simple",
    columns = c(Model_1, Model_2, Model_3, Model_4)
  ) |>
  tab_spanner(
    label = "Covariate-Adjusted",
    columns = c(Model_5, Model_6, Model_7, Model_8)) |>
  tab_header("Table B: Instrumental Variable Estimation of the Effect of EU Foreign Aid (t-1) on CIRI Human Empowerment Index and Polity IV Combined Scores (t - t+3) by Education Quartile, baseline year 1990")

print(ciri_table_90)

#save table
ciri_table_90 |> gtsave("ciri_table_90.tex")


#Polity by quartile----
### Quartile 1 Short
polity_short_1 <- data %>%
  filter(quartile == 1) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

### Quartile 2 Short
polity_short_2 <- data %>%
  filter(quartile == 2) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

### Quartile 3 Short
polity_short_3 <- data %>%
  filter(quartile == 3) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

### Quartile 4 Short
polity_short_4 <- data %>%
  filter(quartile == 4) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

##Polity long, by quartile
form_polity_long <- as.formula(glue("polity2avg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")) 

polity_long_1 <- data %>%
  filter(quartile == 1) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity_long_2 <- data %>%
  filter(quartile == 2) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity_long_3 <- data %>%
  filter(quartile == 3) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity_long_4 <- data %>%
  filter(quartile == 4) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))


##create table of Polity regressions by quartile ----
pvalue_polity <- modelsummary(list(polity_short_1, polity_long_1, polity_short_2, polity_long_2, polity_short_3, polity_long_3, polity_short_4, polity_long_4),
                              coef_omit = "^cov",
                              coef_rename = ("fit_EV" = "Aid"),
                              gof_map = c("nobs", "FE: ccode", "FE: year"),
                              stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))

gof_polity_90 <- data.frame(
  Metric = c("Effect of Aid", "Standard Error", "Country Fixed Effect", "Year Fixed Effect", "Number of Countries", "Number of Years", "N"),
  Model_1 = c(round(coef(polity_short_1)["fit_EV"], 3), round(summary(polity_short_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_1)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_short_1)$nobs),
  Model_2 = c(round(coef(polity_short_2)["fit_EV"], 3), round(summary(polity_short_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_2)), "Yes", "No"),
              fixef_ccode[3], fixef_year[3], summary(polity_short_2)$nobs),
  Model_3 = c(round(coef(polity_short_3)["fit_EV"], 3), round(summary(polity_short_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_short_3)$nobs),
  Model_4 = c(round(coef(polity_short_4)["fit_EV"], 3), round(summary(polity_short_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_short_4)$nobs),
  Model_5 = c(round(coef(polity_long_1)["fit_EV"], 3), round(summary(polity_long_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_1)), "Yes", "No"),
              fixef_ccode[2], fixef_year[2], summary(polity_long_1)$nobs),
  Model_6 = c(round(coef(polity_long_2)["fit_EV"], 3), round(summary(polity_long_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_2)), "Yes", "No"),
              fixef_ccode[4], fixef_year[4], summary(polity_long_2)$nobs),
  Model_7 = c(round(coef(polity_long_3)["fit_EV"], 3), round(summary(polity_long_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_long_3)$nobs),
  Model_8 = c(round(coef(polity_long_4)["fit_EV"], 3), round(summary(polity_long_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_long_4)$nobs))

##Create a gt table
polity_table_90 <- gt(gof_polity_90)

#Update titles and labels
polity_table_90 <- polity_table_90 %>%
  cols_label(
    Metric = "Polity IV",
    Model_1 = "Q1",
    Model_2 = "Q2",
    Model_3 = "Q3",
    Model_4 = "Q4",
    Model_5 = "Q1",
    Model_6 = "Q2",
    Model_7 = "Q3",
    Model_8 = "Q4",
  ) |>
  tab_spanner(
    label = "Simple",
    columns = c(Model_1, Model_2, Model_3, Model_4)
  ) |>
  tab_spanner(
    label = "Covariate-Adjusted",
    columns = c(Model_5, Model_6, Model_7, Model_8)) |>
  tab_source_note(
    source_note = "Q1 is the first quartile, which are countries with the the lowest mean male education. 
    The nine covariates and the nine dummy variables that control for missing covariate values are not shown 
    in the covariate-adjusted model. All models include robust standard errors clustered by country and year.")

#save table
polity_table_90 |> gtsave("polity_table_90.tex")

#1995 -----
data <- read_dta("data/Final_Main.dta")

data <- data |>
  filter(year >= 1995) 

#find education means by country and divide countries into quartiles ----
data[data == -99.00] <- NA

#find means by quartile
means_data <- data %>%
  filter(!is.na(covihme_ayem)) %>%  # Exclude NA values in covihme_ayem before computing quartiles
  mutate(covihme_ayem_quartile = ntile(covihme_ayem, 4)) %>%
  group_by(covihme_ayem_quartile) %>%
  summarize(mean_education = mean(covihme_ayem, na.rm = TRUE))

means_table <- means_data |>
  gt() |>
  fmt_number(columns = "mean_education",
             decimals = 2) |>
  cols_label(covihme_ayem_quartile = "Quartile",
             mean_education = "Average Education Attainment (males)")

means_table |> gtsave("quartile_means.tex")

data <- data |>
  group_by(ccode) |>
  mutate(edu_mean_ccode = mean(covihme_ayem, na.rm = TRUE)) |>
  ungroup()

data <- data |>
  filter(!is.na(edu_mean_ccode)) %>%
  mutate(quartile = ntile(edu_mean_ccode, 4))


#re-run original regressions, by quartile 1995 ----
#add -99s back in to set up for regression for all ^cov variables
data[, grepl("^cov", names(data))] <- replace(data[, grepl("^cov", names(data))], is.na(data[, grepl("^cov", names(data))]), -99)

#CIRI by quartile, 1995-----
##CIRI short, by quartile
###quartile 1
ciri_short_1 <- data |>
  filter(quartile == 1) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

###quartile 2
ciri_short_2 <- data |>
  filter(quartile == 2) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))


###quartile 3
ciri_short_3 <- data |>
  filter(quartile == 3) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

###quartile 4
ciri_short_4 <- data |>
  filter(quartile == 4) |>
  feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

##CIRI long, by quartile
covars <- str_subset(colnames(data), "(^cov)")
form_ciri_long <- as.formula(glue("new_empinxavg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")) 

ciri_long_1 <- data |>
  filter(quartile ==1) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_long_2 <- data |>
  filter(quartile ==2) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_long_3 <- data |>
  filter(quartile ==3) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

ciri_long_4 <- data |>
  filter(quartile ==4) |>
  feols(form_ciri_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))


#create table of CIRI regressions by quartile ----
modelsummary(list(ciri_short_1, ciri_long_1, ciri_short_2, ciri_long_2, ciri_short_3, ciri_long_3, ciri_short_4, ciri_long_4),
             coef_omit = "^cov",
             coef_rename = ("fit_EV" = "Aid"),
             gof_map = c("nobs", "FE: ccode", "FE: year"),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))

gof_ciri_master_95 <- data.frame(
  Metric = c("Effect of Aid", "Standard Error", "Country Fixed Effect", "Year Fixed Effect", "Number of Countries", "Number of Years", "N"),
  Model_1 = c(round(coef(ciri_short_1)["fit_EV"], 3), round(summary(ciri_short_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_1)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short_1)$nobs),
  Model_2 = c(round(coef(ciri_short_2)["fit_EV"], 3), round(summary(ciri_short_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_2)), "Yes", "No"),
              fixef_ccode[3], fixef_year[3], summary(ciri_short_2)$nobs),
  Model_3 = c(round(coef(ciri_short_3)["fit_EV"], 3), round(summary(ciri_short_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short_3)$nobs),
  Model_4 = c(round(coef(ciri_short_4)["fit_EV"], 3), round(summary(ciri_short_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short_4)$nobs),
  Model_5 = c(round(coef(ciri_long_1)["fit_EV"], 3), round(summary(ciri_long_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_1)), "Yes", "No"),
              fixef_ccode[2], fixef_year[2], summary(ciri_long_1)$nobs),
  Model_6 = c(round(coef(ciri_long_2)["fit_EV"], 3), round(summary(ciri_long_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_2)), "Yes", "No"),
              fixef_ccode[4], fixef_year[4], summary(ciri_long_2)$nobs),
  Model_7 = c(round(coef(ciri_long_3)["fit_EV"], 3), round(summary(ciri_long_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_long_3)$nobs),
  Model_8 = c(round(coef(ciri_long_4)["fit_EV"], 3), round(summary(ciri_long_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_long_4)$nobs))

##Create a gt table
ciri_table_95 <- gt(gof_ciri_master_95)

#Update titles and labels
ciri_table_95 <- ciri_table_95 |>
  cols_label(
    Metric = "CIRI",
    Model_1 = "Q1",
    Model_2 = "Q2",
    Model_3 = "Q3",
    Model_4 = "Q4",
    Model_5 = "Q1",
    Model_6 = "Q2",
    Model_7 = "Q3",
    Model_8 = "Q4",
  ) |>
  tab_spanner(
    label = "Simple",
    columns = c(Model_1, Model_2, Model_3, Model_4)
  ) |>
  tab_spanner(
    label = "Covariate-Adjusted",
    columns = c(Model_5, Model_6, Model_7, Model_8)) |>
  tab_header("Table C: Instrumental Variable Estimation of the Effect of EU Foreign Aid (t-1) on CIRI Human Empowerment Index and Polity IV Combined Scores (t - t+3) by Education Quartile, baseline year 1995")

print(ciri_table_95)

#save table for paper
ciri_table_95 |> gtsave("ciri_table_95.tex")


#Polity by quartile, 1995 ----
### Quartile 1 Short
polity_short_1 <- data %>%
  filter(quartile == 1) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

### Quartile 2 Short
polity_short_2 <- data %>%
  filter(quartile == 2) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

### Quartile 3 Short
polity_short_3 <- data %>%
  filter(quartile == 3) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

### Quartile 4 Short
polity_short_4 <- data %>%
  filter(quartile == 4) %>%
  feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, 
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

##Polity long, by quartile
form_polity_long <- as.formula(glue("polity2avg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")) 

polity_long_1 <- data %>%
  filter(quartile == 1) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity_long_2 <- data %>%
  filter(quartile == 2) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity_long_3 <- data %>%
  filter(quartile == 3) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

polity_long_4 <- data %>%
  filter(quartile == 4) %>%
  feols(form_polity_long,
        cluster = ~ ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))


# Create a table of Polity regressions by quartile ----
pvalue_polity <- modelsummary(list(polity_short_1, polity_long_1, polity_short_2, polity_long_2, polity_short_3, polity_long_3, polity_short_4, polity_long_4),
                              coef_omit = "^cov",
                              coef_rename = ("fit_EV" = "Aid"),
                              gof_map = c("nobs", "FE: ccode", "FE: year"),
                              stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))


gof_polity_95 <- data.frame(
  Metric = c("Effect of Aid", "Standard Error", "Country Fixed Effect", "Year Fixed Effect", "Number of Countries", "Number of Years", "N"),
  Model_1 = c(round(coef(polity_short_1)["fit_EV"], 3), round(summary(polity_short_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_1)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_short_1)$nobs),
  Model_2 = c(round(coef(polity_short_2)["fit_EV"], 3), round(summary(polity_short_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_2)), "Yes", "No"),
              fixef_ccode[3], fixef_year[3], summary(polity_short_2)$nobs),
  Model_3 = c(round(coef(polity_short_3)["fit_EV"], 3), round(summary(polity_short_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_short_3)$nobs),
  Model_4 = c(round(coef(polity_short_4)["fit_EV"], 3), round(summary(polity_short_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_short_4)$nobs),
  Model_5 = c(round(coef(polity_long_1)["fit_EV"], 3), round(summary(polity_long_1)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_1)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_1)), "Yes", "No"),
              fixef_ccode[2], fixef_year[2], summary(polity_long_1)$nobs),
  Model_6 = c(round(coef(polity_long_2)["fit_EV"], 3), round(summary(polity_long_2)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_2)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_2)), "Yes", "No"),
              fixef_ccode[4], fixef_year[4], summary(polity_long_2)$nobs),
  Model_7 = c(round(coef(polity_long_3)["fit_EV"], 3), round(summary(polity_long_3)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_3)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_3)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_long_3)$nobs),
  Model_8 = c(round(coef(polity_long_4)["fit_EV"], 3), round(summary(polity_long_4)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long_4)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long_4)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(polity_long_4)$nobs))

##Create a gt table
polity_table_95 <- gt(gof_polity_95)


# Update titles and labels
polity_table_95 <- polity_table_95 %>%
  cols_label(
    Metric = "Polity IV",
    Model_1 = "Q1",
    Model_2 = "Q2",
    Model_3 = "Q3",
    Model_4 = "Q4",
    Model_5 = "Q1",
    Model_6 = "Q2",
    Model_7 = "Q3",
    Model_8 = "Q4",
  ) |>
  tab_spanner(
    label = "Simple",
    columns = c(Model_1, Model_2, Model_3, Model_4)
  ) |>
  tab_spanner(
    label = "Covariate-Adjusted",
    columns = c(Model_5, Model_6, Model_7, Model_8)) |>
  tab_source_note(
    source_note = "Q1 is the first quartile, which are countries with the the lowest mean male education. 
    The nine covariates and the nine dummy variables that control for missing covariate values are not shown 
    in the covariate-adjusted model. All models include robust standard errors clustered by country and year.")


# Print the gt table
print(polity_table_95)

#save table
polity_table_95 |> gtsave("polity_table_95.tex")

