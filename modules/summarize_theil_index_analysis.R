# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Hurst exponent for different lengths obtained in the initial analysis ----
get_theil_resume <- function(df_final_theil) {
  df_theil_resume <- df_final_theil %>%
    group_by(length) %>%
    summarise(
      theil_mean = mean(theil_index, na.rm = TRUE),
      theil_excess = mean(theil_index, na.rm = TRUE) - 1 + 0.57721566490153286,
      theil_sd = sd(theil_index, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(df_theil_resume)
}
