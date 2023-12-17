#Table 2 Replication - Leyly Moridi and Isabel Linzer


library(tidyverse)
library(gt)
library(haven)
library(glue)
library(fixest)
library(modelsummary)
library(dplyr)
library(broom)
library(tibble)

#load and set up data----
data <- read_dta("data/Final_Main.dta")

# Filter data for the year 1987 onwards
data <- data |>
  filter(year >= 1987)


#Simple and co-variate regression for CIRI and Polity ----
##Column 1, CIRI simple regression
ciri_short <- feols(new_empinxavg ~ 1 | ccode + year | EV ~ l2CPcol2, data, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

##Column 2, CIRI covariate-adjusted regression
covars <- str_subset(colnames(data), "(^cov)")
form_ciri_long <- as.formula(glue("new_empinxavg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")) 
ciri_long <- feols(form_ciri_long, data,
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))


##Column 3, Polity simple regression  
polity_short <- feols(polity2avg ~ 1 | ccode + year | EV ~ l2CPcol2, data, 
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

## Column 4, Polity covariate-adjusted regression
form_polity_long <- as.formula(glue("polity2avg ~ {str_c(covars, collapse = ' + ')} | ccode + year | EV ~ l2CPcol2")) 
polity_long <- feols(form_polity_long, data,
              cluster = ~ ccode + year,
              ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

#Create table ----
#view model summary with p-values
pvalue_table <- modelsummary(list(ciri_short, ciri_long, polity_short, polity_long), coef_omit = "^cov",
             coef_rename = ("fit_EV" = "Aid"),
             gof_map = c("nobs", "FE: ccode", "FE: year"),
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01))


# Create a data frame with the information for the table 
fixef_ccode <- c(ciri_short$fixef_sizes["ccode"], ciri_long$fixef_sizes["ccode"],
                 polity_short$fixef_sizes["ccode"], polity_long$fixef_sizes["ccode"])

fixef_year <- c(ciri_short$fixef_sizes["year"], ciri_long$fixef_sizes["year"],
                polity_short$fixef_sizes["year"], polity_long$fixef_sizes["year"])

gof_info <- data.frame(
  Metric = c("Effect of Aid", "Standard Error", "Country Fixed Effect", "Year Fixed Effect", "Number of Countries", "Number of Years", "N"),
  Model_1 = c(round(coef(ciri_short)["fit_EV"], 3), round(summary(ciri_short)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_short)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_short)), "Yes", "No"),
              fixef_ccode[1], fixef_year[1], summary(ciri_short)$nobs),
  Model_2 = c(round(coef(ciri_long)["fit_EV"], 3), round(summary(ciri_long)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(ciri_long)), "Yes", "No"),
              ifelse("year" %in% names(fixef(ciri_long)), "Yes", "No"),
              fixef_ccode[2], fixef_year[2], summary(ciri_long)$nobs),
  Model_3 = c(round(coef(polity_short)["fit_EV"], 3), round(summary(polity_short)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_short)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_short)), "Yes", "No"),
              fixef_ccode[3], fixef_year[3], summary(polity_short)$nobs),
  Model_4 = c(round(coef(polity_long)["fit_EV"], 3), round(summary(polity_long)$se["fit_EV"], 3), 
              ifelse("ccode" %in% names(fixef(polity_long)), "Yes", "No"),
              ifelse("year" %in% names(fixef(polity_long)), "Yes", "No"),
              fixef_ccode[4], fixef_year[4], summary(polity_long)$nobs))

##Turn dataframe into gt table
gt_table <- gt(gof_info)

#Update titles and labels
gt_table <- gt_table |>
  cols_label(
    Metric = " ",
    Model_1 = "Simple",
    Model_2 = "Covariate-adjusted",
    Model_3 = "Simple",
    Model_4 = "Covariate-adjusted"
  ) %>%
  tab_spanner(
    label = "CIRI",
    columns = c(Model_1, Model_2)
  ) %>%
  tab_spanner(
    label = "Polity",
    columns = c(Model_3, Model_4)
  ) |>
  tab_source_note(
    source_note = "In columns 2 and 4 the nine covariates and the nine dummy variables that control for missing covariate values are not shown. All models include robust standard errors clustered by country and year."
  ) |>
  tab_header("Table 2: Instrumental Variable Estimation of the Effect of EU Foreign Aid (t-1) on the CIRI Human Empowerment Index and Polity IV Combined Score (t - t+3)")


# Print the gt table
print(gt_table)

#save table for paper ----
gt_table |> gtsave("replication_table.tex")
