# Libraries ----
library(dplyr)
library(foreach)
library(data.table)
library(doParallel)

# Preparation of Covid Data for Temporal Fluctuation Scaling estimation ----
prepare_tfs_covid_data <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases"
) {
  # Auxiliary function for renaming of variable_name as value
  rename_variable <- function(x) {
    x <- if_else(
      x == "cases" | x == "deaths",
      "value",
      x
    )
    return(x)
  }
  # Spatially clustered data
  df_tfs <- df_covid %>%
    filter(date >= initial_date, date <= final_date) %>%
    # Cases and deaths group by date (sum over subregions)
    group_by(region, subregion, date) %>%
    summarise(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE)
    ) %>%
    mutate(index = cumsum(c(0, diff(date))) + 1) %>%
    ungroup() %>%
    # Selection of variable name
    select_at(all_of(
      c("region", "subregion", "date", "index", variable_name)),
      rename_variable
    ) %>%
    estimate_tfs_data()
  
  return(df_tfs)
}

# Estimate Temporal Fluctuation Scaling for a particular region and date ----
estimate_tfs_local <- function(
  df_tfs,
  days_treshold = 60
) {
  if(nrow(df_tfs) < days_treshold) {
    cat("Data frame doesn't have minimum number of rows\n")
    # Generate TFS Data
    df_tfs_parameters <- data.table()
  } else {
    # Generate TFS Data
    df_tfs_parameters <- adjust_tfs_power_law(df_tfs = df_tfs) %>%
      mutate(
        region = df_tfs %>% distinct(region) %>% pull(),
        subregion = df_tfs %>% distinct(subregion) %>% pull(),
        date = df_tfs %>% pull(date) %>% max(na.rm = TRUE)
      ) %>%
      relocate(region, subregion, date)
  }
  
  return(df_tfs_parameters)
}

# Estimate Temporal Fluctuation Scaling for a particular date ----
estimate_tfs_regions <- function(
  df_tfs,
  days_treshold = 60,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  # Selection of loop size
  loop_index <- df_tfs %>% distinct(region) %>% pull()
  
  # Parallel loop over loop_index for estimate TFS per region
  df_tfs_parameters <- foreach(
    i = loop_index,
    .combine = bind_rows,
    .errorhandling = c("remove"),
    .export = c(
      "estimate_tfs_data",
      "adjust_tfs_power_law",
      "estimate_tfs_local"
    ),
    .packages = c("dplyr", "purrr", "cumstats", "data.table")
  ) %dopar% {
    # Log output for monitoring progress
    if(verbose >= 1){
      setwd(path)
      cat(
        paste0(
          "Estimate Temporal Fluctuation Scaling for region ", i, "\n"
        ),
        file = "log_tfs_regions.txt",
        append = TRUE
      )
    }
    
    # Generate local TFS data
    df_temp <- estimate_tfs_local(
      df_tfs = df_tfs %>% filter(region == i),
      days_treshold = days_treshold
    )
    
    df_temp
  }
  
  return(df_tfs_parameters)
}

# Estimate Temporal Fluctuation Scaling by spatial clustering ----
estimate_tfs_covid <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04"
) {
  if(saved_all_data == FALSE) {
    # TFS data
    df_tfs <- prepare_tfs_covid_data(
      df_covid = df_covid,
      initial_date = initial_date,
      final_date = final_date,
      variable_name = variable_name
    )
    
    # Selection of loop size
    loop_index <- df_tfs %>%
      filter(index %% (n_days_skipped + 1) == 0) %>%
      distinct(date) %>%
      pull()
    
    # Parallel loop over loop_index for estimate TFS per date
    df_tfs_parameters <- foreach(
      i = loop_index,
      .combine = bind_rows,
      .errorhandling = c("remove"),
      .export = c(
        "estimate_tfs_data",
        "adjust_tfs_power_law",
        "estimate_tfs_local",
        "estimate_tfs_regions"
      ),
      .packages = c(
        "dplyr",
        "purrr",
        "foreach",
        "cumstats",
        "data.table",
        "doParallel"
      )
    ) %dopar% {
      # Log output for monitoring progress
      if(verbose >= 1){
        setwd(path)
        cat(
          paste0("Estimate Temporal Fluctuation Scaling for date ", i, "\n"),
          file = "log_tfs_covid_dates.txt",
          append = TRUE
        )
      }
      
      # Renovation of initial_date for fixed_window
      if(fixed_window == "fixed") {
        initial_date <- i - days_treshold
      }
      
      # Generate local TFS data
      df_temp <- estimate_tfs_regions(
        df_tfs = df_tfs %>%
          filter(date >= initial_date, date <= i) %>%
          group_by(region, subregion) %>%
          mutate(index = index - min(index, na.rm = TRUE)) %>%
          ungroup(),
        days_treshold = days_treshold,
        path = path,
        verbose = verbose
      )
      
      df_temp
    }
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_tfs_parameters,
      paste0(
        input_path_processed,
        "/df_tfs_covid_data_",
        variable_name,
        "_",
        fixed_window,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
  } else {
    # Load all data from processed data for not reprocess the information
    df_tfs_parameters <- fread(
      paste0(
        input_path_processed,
        "/df_tfs_covid_data_",
        variable_name,
        "_",
        fixed_window,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      encoding = "UTF-8"
    )
  }
  
  return(df_tfs_parameters)
}

# Auxiliary function for preparation of Temporal Fluctuation Scaling data ----
get_tfs_covid_resume <- function(
  df_tfs_cases,
  df_tfs_deaths,
  days_treshold = 60,
  fixed_window = "no_fixed"
) {
  # Cases
  df_tfs_cases <- left_join(
    x = df_tfs_cases %>%
      filter(statistic == "Estimate") %>%
      select(-statistic) %>%
      rename(
        tfs_coefficient_value = tfs_coefficient,
        tfs_exponent_value = tfs_exponent
      ),
    y = df_tfs_cases %>%
      filter(statistic == "Standard error") %>%
      select(-statistic) %>%
      rename(
        tfs_coefficient_sd = tfs_coefficient,
        tfs_exponent_sd = tfs_exponent
      ),
    by = c("region", "subregion", "date")
  ) %>% mutate(information = "cases")
  
  # Deaths
  df_tfs_deaths <- left_join(
    x = df_tfs_deaths %>%
      filter(statistic == "Estimate") %>%
      distinct() %>%
      select(-statistic) %>%
      rename(
        tfs_coefficient_value = tfs_coefficient,
        tfs_exponent_value = tfs_exponent
      ),
    y = df_tfs_deaths %>%
      filter(statistic == "Standard error") %>%
      distinct() %>%
      select(-statistic) %>%
      rename(
        tfs_coefficient_sd = tfs_coefficient,
        tfs_exponent_sd = tfs_exponent
      ),
    by = c("region", "subregion", "date")
  ) %>% mutate(information = "deaths")
  
  # Final merge
  df_tfs_resume <- df_tfs_cases %>%
    bind_rows(df_tfs_deaths) %>%
    # Estimate length of TFS data
    group_by(region, subregion, information) %>%
    mutate(
      window_flag = fixed_window,
      length = as.numeric(as.Date(date) - min(date, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    mutate(
      length = if_else(
        window_flag == "no_fixed",
        length + days_treshold,
        days_treshold
      )
    ) %>%
    distinct() %>%
    arrange(region, subregion, information, date)
  
  return(df_tfs_resume)
}
