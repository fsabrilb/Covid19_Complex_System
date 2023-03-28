# Libraries ----
library(dplyr)
library(foreach)
library(data.table)
library(doParallel)

# Preparation of Covid Data for Temporal Theil Scaling estimation ----
prepare_tts_covid_data <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases",
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04"
) {
  if(saved_all_data == FALSE) {
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
    df_covid <- df_covid %>%
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
      select_at(
        all_of(c("region", "subregion", "date", "index", variable_name)),
        rename_variable
      )
    
    # Loop over regions and dates for diffusive algorithm
    df_tts <- data.table()
    for(i in df_covid %>% distinct(region) %>% pull()) {
      cat(
        paste0(
          "Generate diffusive paths in region: ",
          i,
          " with ",
          df_covid %>% filter(region == i) %>% nrow(),
          " rows\n"
        )
      )
      df_tts <- bind_rows(
        df_tts,
        estimate_diffusive_algorithm(
          df_time_series = df_covid %>% filter(region == i),
          t_min = df_covid %>%
            filter(region == i) %>%
            pull(index) %>%
            min(na.rm = TRUE),
          t_max = df_covid %>%
            filter(region == i) %>%
            pull(index) %>%
            max(na.rm = TRUE),
          path = path,
          verbose = verbose
        ) %>% mutate(region = i)
      )
    }
    
    # Final merge of region, subregion and date per time step
    df_tts <- df_tts %>%
      left_join(
        df_covid %>% select(region, subregion, date, index),
        by = c("time" = "index", "region")
      ) %>%
      relocate(region, subregion, date)
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_tts,
      paste0(
        input_path_processed,
        "/df_tts_covid_data_",
        variable_name,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
  } else {
    df_tts <- fread(
      paste0(
        input_path_processed,
        "/df_tts_covid_data_",
        variable_name,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      encoding = "UTF-8"
    ) %>% mutate(date = as.Date(date))
  }
  
  return(df_tts)
}

# Final preparation of Covid data for Temporal Theil Scaling estimation ----
prepare_tts_covid_final_data <- function(
  df_tts,
  variable_name = "cases",
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04"
) {
  if(saved_all_data == FALSE) {
    # Loop over regions for estimate TTS final data per region
    df_tts_final <- data.table()
    for(i in df_tts %>% distinct(region) %>% pull()) {
      # Function development
      cat(paste0("Merge diffusive path information for region ", i, "\n"))
      
      # Generate TTS data summary
      df_aux <- estimate_tts_data(
        df_diffusive_path = df_tts %>% filter(region == i)
      ) %>% mutate(region = i)
      
      df_tts_final <- df_tts_final %>% bind_rows(df_aux) 
    }
    
    # Final merge of region, subregion and date per time step
    df_tts_final <- df_tts_final %>%
      left_join(
        df_tts %>% distinct(region, subregion, date, time),
        by = c("index" = "time", "region")
      ) %>%
      relocate(region, subregion, date)
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_tts_final %>% filter(!is.na(date)),
      paste0(
        input_path_processed,
        "/df_tts_covid_final_data_",
        variable_name,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
  } else {
    df_tts_final <- fread(
      paste0(
        input_path_processed,
        "/df_tts_covid_final_data_",
        variable_name,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      encoding = "UTF-8"
    ) %>% mutate(date = as.Date(date))
  }
  
  return(df_tts_final)
}

# Estimate Temporal Theil Scaling for a particular region and date ----
estimate_tts_local <- function(
  df_tts,
  days_treshold = 60
) {
  if(nrow(df_tts) < days_treshold) {
    cat("Data frame doesn't have minimum number of rows\n")
    # Generate TTS Data
    df_tts_parameters <- data.table()
  } else {
    # Generate TTS Data
    df_tts_parameters <- adjust_tts_power_law(df_tts = df_tts) %>%
      mutate(
        region = df_tts %>% distinct(region) %>% pull(),
        subregion = df_tts %>% distinct(subregion) %>% pull(),
        date = df_tts %>% pull(date) %>% max(na.rm = TRUE)
      ) %>%
      relocate(region, subregion, date)
  }
  
  return(df_tts_parameters)
}

# Estimate Temporal Theil Scaling for a particular date ----
estimate_tts_regions <- function(
  df_tts,
  days_treshold = 60,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  # Selection of loop size
  loop_index <- df_tts %>% distinct(region) %>% pull()
  
  # Parallel loop over loop_index for estimate TTS per region
  df_tts_parameters <- foreach(
    i = loop_index,
    .combine = bind_rows,
    .errorhandling = c("remove"),
    .export = c(
      "estimate_tts_data",
      "adjust_tts_power_law",
      "estimate_tts_local"
    ),
    .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    # Log output for monitoring progress
    if(verbose >= 1){
      cat(
        paste0("Estimate Temporal Theil Scaling for region ", i, "\n"),
        file = paste0(path, "/log_tts_regions.txt"),
        append = TRUE
      )
    }
    
    # Generate local TTS data
    df_temp <- estimate_tts_local(
      df_tts = df_tts %>% filter(region == i),
      days_treshold = days_treshold
    )
    
    df_temp
  }
  
  return(df_tts_parameters)
}

# Estimate Temporal Theil Scaling by spatial clustering ----
estimate_tts_covid <- function(
  df_tts,
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
    # TTS data
    df_tts <- df_tts %>% filter(date >= initial_date, date <= final_date)
    
    # Selection of loop size
    loop_index <- df_tts %>%
      filter(index %% (n_days_skipped + 1) == 0) %>%
      distinct(date) %>%
      pull() %>%
      as.Date()
    
    # Parallel loop over loop_index for estimate TTS per date
    df_tts_parameters <- foreach(
      i = loop_index,
      .combine = bind_rows,
      .errorhandling = c("remove"),
      .export = c(
        "estimate_tts_data",
        "adjust_tts_power_law",
        "estimate_tts_local",
        "estimate_tts_regions"
      ),
      .packages = c(
        "dplyr",
        "purrr",
        "foreach",
        "data.table",
        "doParallel"
      )
    ) %dopar% {
      # Log output for monitoring progress
      if(verbose >= 1){
        cat(
          paste0("Estimate Temporal Theil Scaling for date ", i, "\n"),
          file = paste0(path, "/log_tts_covid_dates.txt"),
          append = TRUE
        )
      }
      
      # Renovation of initial_date for fixed_window
      if(fixed_window == "fixed") {
        initial_date <- i - days_treshold
      }
      
      # Generate local TTS data
      df_temp <- estimate_tts_regions(
        df_tts = df_tts %>%
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
      df_tts_parameters,
      paste0(
        input_path_processed,
        "/df_tts_covid_data_",
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
    df_tts_parameters <- fread(
      paste0(
        input_path_processed,
        "/df_tts_covid_data_",
        variable_name,
        "_",
        fixed_window,
        "_",
        gsub("-", "", input_date),
        ".csv"
      ),
      encoding = "UTF-8"
    ) %>% mutate(date = as.Date(date))
  }
  
  return(df_tts_parameters)
}

# Auxiliary function for preparation of Temporal Theil Scaling data ----
get_tts_covid_resume <- function(
  df_tts_cases,
  df_tts_deaths,
  days_treshold = 60,
  fixed_window = "no_fixed"
) {
  # Cases
  df_tts_cases <- df_tts_cases %>%
    filter(statistic %in% c("Estimate", "Standard error", "R2")) %>%
    pivot_wider(
      names_from = c(statistic),
      values_from = c(tts_coefficient, tts_exponent),
      names_sort = TRUE,
      values_fn = max
    ) %>%
    rename(
      tts_coefficient_value = 5,
      tts_exponent_value = 8,
      tts_coefficient_sd = 7,
      tts_exponent_sd = 10,
      tts_coefficient_r2 = 6,
      tts_exponent_r2 = 9
    ) %>%
    mutate(information = "cases")
  
  # Deaths
  df_tts_deaths <- df_tts_deaths %>%
    filter(statistic %in% c("Estimate", "Standard error", "R2")) %>%
    pivot_wider(
      names_from = c(statistic),
      values_from = c(tts_coefficient, tts_exponent),
      names_sort = TRUE,
      values_fn = max
    ) %>%
    rename(
      tts_coefficient_value = 5,
      tts_exponent_value = 8,
      tts_coefficient_sd = 7,
      tts_exponent_sd = 10,
      tts_coefficient_r2 = 6,
      tts_exponent_r2 = 9
    ) %>%
    mutate(information = "deaths")
  
  # Final merge
  df_tts_resume <- df_tts_cases %>%
    bind_rows(df_tts_deaths) %>%
    # Estimate length of TTS data
    group_by(region, subregion, information, time_series) %>%
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
    arrange(region, subregion, information, date, time_series)
  
  return(df_tts_resume)
}
