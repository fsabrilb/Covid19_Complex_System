# Libraries ----
library(dplyr)
library(data.table)

# Preparation of Covid Data for Theil index estimation ----
prepare_theil_covid_data <- function(
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
  df_theil <- df_covid %>%
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
    )
  
  return(df_theil)
}

# Estimate Theil index for a particular region and date ----
estimate_entropy_indexes_local <- function(
  df_theil,
  days_treshold = 60,
  alpha = 1,
  epsilon = 1
) {
  if(nrow(df_theil) < days_treshold) {
    cat("Data frame doesn't have minimum number of rows\n")
    # Generate Theil Data
    df_entropy <- data.table()
  } else {
    # Generate Theil Data
    df_entropy <- estimate_theil_index(df_time_series = df_theil) %>%
      left_join(
        estimate_generalized_entropy_index(
          df_time_series = df_theil %>% select(-c(region, subregion, date)),
          alpha = alpha
        ),
        by = c("index", "value")
      ) %>%
      left_join(
        estimate_atkinson_index(
          df_time_series = df_theil %>% select(-c(region, subregion, date)),
          epsilon = epsilon
        ),
        by = c("index", "value")
      ) %>%
      mutate(
        region = df_theil %>% distinct(region) %>% pull(),
        subregion = df_theil %>% distinct(subregion) %>% pull(),
        date = df_theil %>% pull(date) %>% max(na.rm = TRUE)
      ) %>%
      select(-c(index, value)) %>%
      distinct()
  }
  
  return(df_entropy)
}

# Estimate Theil index for a particular date ----
estimate_entropy_indexes_regions <- function(
  df_theil,
  days_treshold = 60,
  alpha = 1,
  epsilon = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  # Selection of loop size
  loop_index <- df_theil %>% distinct(region) %>% pull()
  
  # Parallel loop over loop_index for estimate Theil per region
  df_entropy <- foreach(
    i = loop_index,
    .combine = bind_rows,
    .errorhandling = c("remove"),
    .export = c(
      "estimate_generalized_entropy_index",
      "estimate_theil_index",
      "estimate_atkinson_index",
      "estimate_entropy_indexes_local"
    ),
    .packages = c("dplyr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    # Log output for monitoring progress
    if(verbose >= 1){
      setwd(path)
      cat(
        paste0(
          "Estimate Theil for region ", i, "\n"
        ),
        file = "log_theil_covid_regions.txt",
        append = TRUE
      )
    }
    
    # Generate local Theil data
    df_temp <- estimate_entropy_indexes_local(
      df_theil = df_theil %>% filter(region == i),
      days_treshold = days_treshold,
      alpha = alpha,
      epsilon = epsilon
    )
    
    df_temp
  }
  
  return(df_entropy)
}

# Estimate Theil index by spatial clustering ----
estimate_entropy_indexes_covid <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  alpha = 1,
  epsilon = 1,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04"
) {
  if(saved_all_data == FALSE) {
    # Theil data
    df_theil <- prepare_theil_covid_data(
      df_covid = df_covid,
      initial_date = initial_date,
      final_date = final_date,
      variable_name = variable_name
    )
    
    # Selection of loop size
    loop_index <- df_theil %>%
      filter(index %% (n_days_skipped + 1) == 0) %>%
      distinct(date) %>%
      pull()
    
    # Parallel loop over loop_index for estimate Theil per date
    df_entropy <- foreach(
      i = loop_index,
      .combine = bind_rows,
      .errorhandling = c("remove"),
      .export = c(
        "estimate_generalized_entropy_index",
        "estimate_theil_index",
        "estimate_atkinson_index",
        "estimate_entropy_indexes_local",
        "estimate_entropy_indexes_regions"
      ),
      .packages = c("dplyr", "foreach", "data.table", "doParallel")
    ) %dopar% {
      # Log output for monitoring progress
      if(verbose >= 1){
        setwd(path)
        cat(
          paste0("Estimate Theil for date ", i, "\n"),
          file = "log_theil_covid_dates.txt",
          append = TRUE
        )
      }
      
      # Renovation of initial_date for fixed_window
      if(fixed_window == "fixed") {
        initial_date <- i - days_treshold
      }
      
      # Generate local Theil data
      df_temp <- estimate_entropy_indexes_regions(
        df_theil = df_theil %>%
          filter(date >= initial_date, date <= i) %>%
          group_by(region, subregion) %>%
          mutate(index = index - min(index, na.rm = TRUE)) %>%
          ungroup(),
        days_treshold = days_treshold,
        alpha = alpha,
        epsilon = epsilon, 
        path = path,
        verbose = verbose
      )
      
      df_temp
    }
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_entropy,
      paste0(
        input_path_processed,
        "/df_theil_covid_data_",
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
    df_entropy <- fread(
      paste0(
        input_path_processed,
        "/df_theil_covid_data_",
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
  
  return(df_entropy)
}

# Auxiliary function for preparation of Theil data ----
get_theil_covid_resume <- function(
  df_theil_cases,
  df_theil_deaths,
  days_treshold = 60,
  fixed_window = "no_fixed"
) {
  # Final merge
  df_theil_resume <- df_theil_cases %>%
    mutate(information = "cases") %>%
    bind_rows(df_theil_deaths %>% mutate(information = "deaths")) %>%
    # Estimate length of Theil data
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
    arrange(region, subregion, information, date)
  
  return(df_theil_resume)
}
