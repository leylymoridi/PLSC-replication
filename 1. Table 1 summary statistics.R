#Table 1 Summary Statistics - Leyly Moridi and Isabel Linzer

library(tidyverse)
library(scales)
library(broom)
library(lme4)
library(rstanarm)
library(gt)
library(haven)
library(dplyr)
library(tidyr)

#load data ----
data <- read_dta("data/final_main.dta")

#clean data by replacing -99.00 (which appears to be a placeholder) with NA
data[data == -99.00] <- NA

#create list of variables
variables <- c("new_empinxavg", "polity2avg", "EV", "l2CPcol2", "covwdi_exp", "covwdi_imp", "covwdi_fdi", "covwvs_rel",
              "coviNY_GDP_PETR_RT_ZS", "covihme_ayem", "covdemregion", "covloggdp", "covloggdpC")

#calculate summary statistics of listed variables ----
summary_stats <- sapply(data[variables],
                        function(x) c(mean = mean(x, na.rm = TRUE), 
                                      sd = sd(x, na.rm = TRUE), 
                                      n = sum(!is.na(x))))


#restructure and relabel data
summary_stats_df <- as.data.frame(t(summary_stats))

colnames(summary_stats_df) <- c("Mean", "Standard Deviation", "n")

rownames(summary_stats_df) <- c("CIRI Human Empowerment Index (4-year avg)", "Polity IV Score (4-year avg)",
                                "EU Aid (logged millions of 1995 USD)", "Former Colony Status in Second Half of Year (lag 2)",
                                "Log Exports", "Log Imports", "FDI", "Religiosity", "Petroleum Revenues (percent of GDP)",
                                "Average Years of Education (males)", "Democracies in Region", "Log GDP",
                                "Log GDP per Capita")


summary_stats_df <- summary_stats_df |>
  rownames_to_column(var = "Variable")


#make summary table ----
summary_table <- summary_stats_df |>
  gt() |>
  fmt_number(columns = c("Mean", "Standard Deviation"),
             decimals = 2) |>
  tab_source_note(
    source_note = "For the purpose of this table, missing data (indicated by -99.0 in the Carnegie and Marinov (2017) dataset) were replaced with NAs, to avoid skewed summary statistics and as recommended in the CIRI user manual."
  ) |>
  tab_header("Table 1: Summary Statistics")

print(summary_table)

summary_table |> gtsave("summary_table.tex")
