# Libraries ----
library(dplyr)
library(purrr)
library(data.table)

# Count of subregions with at least one case (or one death) during pandemic ----
# Active subregion act_sub: Subregion with at least one case (or one death)
count_active_subregions <- function(df_covid){
  # Count of active subregions data
  df_count <- df_covid %>%
    # Cross Join for identify all possibilities
    distinct(region, subregion, names) %>%
    left_join(
      data.table(
        date = seq(
          df_covid %>% pull(date) %>% min(na.rm = TRUE),
          df_covid %>% pull(date) %>% max(na.rm = TRUE),
          "day"
        )
      ),
      by = character()
    ) %>%
    left_join(
      df_covid,
      by = c("region", "subregion", "names", "date")
    ) %>%
    # Replace NA with zeros
    mutate(
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths))
    ) %>%
    # Correct reconstruction of cases and deaths
    group_by(region, subregion, names) %>%
    mutate(
      cum_cases = cumsum(cases),
      cum_deaths = cumsum(deaths)
    ) %>%
    ungroup() %>%
    # Random fields for identify active subregions
    mutate(
      # Subregions with cases and deaths during one day
      act_sub_cases = if_else(cases == 0, 0, 1),
      act_sub_deaths = if_else(deaths == 0, 0, 1),
      # Subregions with cases and deaths during the pandemic
      act_sub_cum_cases = if_else(cum_cases == 0, 0, 1),
      act_sub_cum_deaths = if_else(cum_deaths == 0, 0, 1)
    ) %>%
    # Generate auxiliary variables for counting of active subregions
    group_by(region, subregion, date) %>%
    summarise(
      # Subregions with cases and deaths during one day
      count_act_sub_cases = sum(act_sub_cases, na.rm = TRUE),
      count_act_sub_deaths = sum(act_sub_deaths, na.rm = TRUE),
      # Subregions with at least one case (death) during the pandemic
      count_act_sub_cum_cases = sum(act_sub_cum_cases, na.rm = TRUE),
      count_act_sub_cum_deaths = sum(act_sub_cum_deaths, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Filtration process (Excluding redundant data)
    filter(
      !(count_act_sub_cases == 0 &
          count_act_sub_deaths == 0 &
          count_act_sub_cum_cases == 0 &
          count_act_sub_cum_deaths == 0
       )
    )
  
  # Counts of subregions per region and begin of pandemic
  df_region <- df_count %>%
    group_by(region, subregion) %>%
    summarise(
      # Subregions with cases and deaths during one day
      max_act_sub_cases = max(count_act_sub_cases, na.rm = TRUE),
      max_act_sub_deaths = max(count_act_sub_deaths, na.rm = TRUE),
      # Subregions with at least one case (death) during the pandemic
      max_act_sub_cum_cases = max(count_act_sub_cum_cases, na.rm = TRUE),
      max_act_sub_cum_deaths = max(count_act_sub_cum_deaths, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Start date of cases in different regions
    left_join(
      df_covid %>%
        filter(cases != 0) %>%
        group_by(region, subregion) %>%
        summarise(date_start_cases = min(date, na.rm = TRUE)),
      by = c("region", "subregion")
    ) %>%
    # Start date of deaths in different regions
    left_join(
      df_covid %>%
        filter(deaths != 0) %>%
        group_by(region, subregion) %>%
        summarise(date_start_deaths = min(date, na.rm = TRUE)),
      by = c("region", "subregion")
    )
  
  # List with all count data
  list_count <- list(
    counts = df_count,
    resume = df_region
  )
  
  return(list_count)
}
