# Libraries ----
library(dplyr)
library(purrr)
library(foreach)
library(data.table)
library(doParallel)

# Preparation of Covid Data for Hurst exponent estimation ----
prepare_hurst_covid_data <- function(
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
  df_hurst <- df_covid %>%
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
  
  return(df_hurst)
}

# Estimate Hurst exponent for a particular region and date ----
estimate_hurst_exponent_local <- function(
  df_hurst,
  days_treshold = 60,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  scale_min = 0.2,
  scale_max = 1.0,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  if(nrow(df_hurst) < days_treshold) {
    cat("Data frame doesn't have minimum number of rows\n")
    # Generate Hurst Data
    if(hurst_method == "rs") {
      list_hurst <- list(
        "fluctuation" = data.table(),
        "hurst" = data.table()
      )
    } else {
      list_hurst <- list(
        "parameters" = data.table(),
        "residuals" = data.table(),
        "fluctuation" = data.table(),
        "hurst" = data.table()
      )
    }
  } else {
    # Generate Hurst Data
    if(hurst_method == "rs") {
      # Temporal dataframe of Hurst Exponent
      list_hurst <- estimate_hurst_rs(
        df_time_series = df_hurst,
        n_step = n_step
      )
      
      # Actualization of list_hurst
      list_hurst <- list(
        "fluctuation" = list_hurst %>%
          pluck("fluctuation")  %>%
          mutate(
            region = df_hurst %>% distinct(region) %>% pull(),
            subregion = df_hurst %>% distinct(subregion) %>% pull(),
            date = df_hurst %>% pull(date) %>% max(na.rm = TRUE)
          ) %>%
          relocate(region, subregion, date),
        "hurst" = list_hurst %>%
          pluck("hurst") %>%
          mutate(
            region = df_hurst %>% distinct(region) %>% pull(),
            subregion = df_hurst %>% distinct(subregion) %>% pull(),
            date = df_hurst %>% pull(date) %>% max(na.rm = TRUE)
          ) %>%
          relocate(region, subregion, date)
      )
    } else {
      # Temporal dataframe of Hurst Exponent
      list_hurst <- estimate_hurst_mfdfa(
        df_time_series = df_hurst,
        dfa_degree = dfa_degree,
        q_order = q_order,
        scale_min = scale_min,
        scale_max = scale_max,
        n_step = n_step,
        path = path,
        verbose = verbose
      )
      
      # Actualization of list_hurst
      list_hurst <- list(
        "parameters" = list_hurst %>%
          pluck("parameters") %>%
          mutate(
            region = df_hurst %>% distinct(region) %>% pull(),
            subregion = df_hurst %>% distinct(subregion) %>% pull(),
            date = df_hurst %>% pull(date) %>% max(na.rm = TRUE)
          ) %>%
          relocate(region, subregion, date),
        "residuals" = list_hurst %>%
          pluck("residuals") %>%
          mutate(
            region = df_hurst %>% distinct(region) %>% pull(),
            subregion = df_hurst %>% distinct(subregion) %>% pull(),
            date = df_hurst %>% pull(date) %>% max(na.rm = TRUE)
          ) %>%
          relocate(region, subregion, date),
        "fluctuation" = list_hurst %>%
          pluck("fluctuation") %>%
          mutate(
            region = df_hurst %>% distinct(region) %>% pull(),
            subregion = df_hurst %>% distinct(subregion) %>% pull(),
            date = df_hurst %>% pull(date) %>% max(na.rm = TRUE)
          ) %>%
          relocate(region, subregion, date),
        "hurst" = list_hurst %>%
          pluck("hurst") %>%
          mutate(
            region = df_hurst %>% distinct(region) %>% pull(),
            subregion = df_hurst %>% distinct(subregion) %>% pull(),
            date = df_hurst %>% pull(date) %>% max(na.rm = TRUE)
          ) %>%
          relocate(region, subregion, date)
      )
    }
  }
  
  return(list_hurst)
}

# Estimate Hurst Exponent for a particular date ----
estimate_hurst_exponent_regions <- function(
  df_hurst,
  days_treshold = 60,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  scale_min = 0.2,
  scale_max = 1.0,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  # Selection of loop size
  loop_index <- df_hurst %>% distinct(region) %>% pull()
  
  # Auxiliary function for merge list in correct form
  list_join <- function(x, y) {
    for(i in names(x)) {
      x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
    }
    return(x)
  }
  
  # Parallel loop over loop_index for estimate Hurst per region
  list_hurst <- foreach(
    i = loop_index,
    .combine = "list_join",
    .errorhandling = c("remove"),
    .export = c(
      "estimate_hurst_rs",
      "estimate_local_hurst_rs",
      "estimate_local_rs",
      "estimate_hurst_brownian_path_mfdfa",
      "estimate_hurst_mfdfa",
      "calculate_fluctuation",
      "get_trend_mfdfa",
      "get_local_trend_mfdfa",
      "estimate_hurst_exponent_local"
    ),
    .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    # Log output for monitoring progress
    if(verbose >= 1){
      cat(
        paste0(
          "Estimate Hurst for region ", i, " using ", hurst_method, " method\n"
        ),
        file = paste0(path, "/log_hurst_covid_regions.txt"),
        append = TRUE
      )
    }
    
    # Generate local Hurst data
    list_temp <- estimate_hurst_exponent_local(
      df_hurst = df_hurst %>% filter(region == i),
      days_treshold = days_treshold,
      hurst_method = hurst_method,
      dfa_degree = dfa_degree,
      q_order = q_order,
      scale_min = scale_min,
      scale_max = scale_max,
      n_step = n_step,
      path = path,
      verbose = verbose
    )
    
    list_temp
  }
  
  return(list_hurst)
}

# Estimate Hurst Exponent by spatial clustering ----
estimate_hurst_exponent_covid <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  scale_min = 0.2,
  scale_max = 1.0,
  n_step = 1,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04"
) {
  if(saved_all_data == FALSE) {
    # Hurst data
    df_hurst <- prepare_hurst_covid_data(
      df_covid = df_covid,
      initial_date = initial_date,
      final_date = final_date,
      variable_name = variable_name
    )
    
    # Selection of loop size
    loop_index <- df_hurst %>%
      filter(index %% (n_days_skipped + 1) == 0) %>%
      distinct(date) %>%
      pull()

    # Auxiliary function for merge list in correct form
    list_join <- function(x, y) {
      for(i in names(x)) {
        x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
      }
      return(x)
    }
    
    # Parallel loop over loop_index for estimate Hurst per date
    list_hurst <- foreach(
      i = loop_index,
      .combine = "list_join",
      .errorhandling = c("remove"),
      .export = c(
        "estimate_hurst_rs",
        "estimate_local_hurst_rs",
        "estimate_local_rs",
        "estimate_hurst_brownian_path_mfdfa",
        "estimate_hurst_mfdfa",
        "calculate_fluctuation",
        "get_trend_mfdfa",
        "get_local_trend_mfdfa",
        "estimate_hurst_exponent_local",
        "estimate_hurst_exponent_regions"
      ),
      .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
    ) %dopar% {
      
      # Log output for monitoring progress
      if(verbose >= 1){
        cat(
          paste0(
            "Estimate Hurst for date ",
            i,
            " window: ",
            fixed_window,
            " using ",
            hurst_method,
            " method\n"
          ),
          file = paste0(path, "/log_hurst_covid_dates.txt"),
          append = TRUE
        )
      }
      
      # Renovation of initial_date for fixed_window
      if(fixed_window == "fixed") {
        initial_date <- i - days_treshold
      }
      
      # Generate local Hurst data
      list_temp <- estimate_hurst_exponent_regions(
        df_hurst = df_hurst %>%
          filter(date >= initial_date, date <= i) %>%
          group_by(region, subregion) %>%
          mutate(index = index - min(index, na.rm = TRUE)) %>%
          ungroup(),
        days_treshold = days_treshold,
        hurst_method = hurst_method,
        dfa_degree = dfa_degree,
        q_order = q_order,
        scale_min = scale_min,
        scale_max = scale_max,
        n_step = n_step,
        path = path,
        verbose = verbose
      )
      
      list_temp
    }
    
    # Save all Covid data in processed data for not reprocess the information
    if(hurst_method == "rs") {
      write.csv(
        list_hurst %>% pluck("fluctuation"),
        paste0(
          input_path_processed,
          "/df_hurst_covid_rs_data_",
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
      
      write.csv(
        list_hurst %>% pluck("hurst"),
        paste0(
          input_path_processed,
          "/df_hurst_covid_rs_value_",
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
      write.csv(
        list_hurst %>% pluck("parameters"),
        paste0(
          input_path_processed,
          "/df_hurst_covid_mfdfa_dfa_data_",
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
      
      write.csv(
        list_hurst %>% pluck("residuals"),
        paste0(
          input_path_processed,
          "/df_hurst_covid_mfdfa_dfa_residuals_",
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
      
      write.csv(
        list_hurst %>% pluck("fluctuation"),
        paste0(
          input_path_processed,
          "/df_hurst_covid_mfdfa_data_",
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
      
      write.csv(
        list_hurst %>% pluck("hurst"),
        paste0(
          input_path_processed,
          "/df_hurst_covid_mfdfa_value_",
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
    }
  } else {
    # Load all data from processed data for not reprocess the information
    if(hurst_method == "rs") {
      list_hurst <- list(
        "fluctuation" = fread(
          paste0(
            input_path_processed,
            "/df_hurst_covid_rs_data_",
            variable_name,
            "_",
            fixed_window,
            "_",
            gsub("-", "", input_date),
            ".csv"
          ),
          encoding = "UTF-8"
        ),
        "hurst" = fread(
          paste0(
            input_path_processed,
            "/df_hurst_covid_rs_value_",
            variable_name,
            "_",
            fixed_window,
            "_",
            gsub("-", "", input_date),
            ".csv"
          ),
          encoding = "UTF-8"
        ) %>%
          # Normalization of columns type
          mutate(
            coefficient = as.numeric(coefficient),
            hurst_value = as.numeric(hurst_value)
          )
      )
    } else {
      list_hurst <- list(
        "parameters" = fread(
          paste0(
            input_path_processed,
            "/df_hurst_covid_mfdfa_dfa_data_",
            variable_name,
            "_",
            fixed_window,
            "_",
            gsub("-", "", input_date),
            ".csv"
          ),
          encoding = "UTF-8"
        ),
        "residuals" = fread(
          paste0(
            input_path_processed,
            "/df_hurst_covid_mfdfa_dfa_residuals_",
            variable_name,
            "_",
            fixed_window,
            "_",
            gsub("-", "", input_date),
            ".csv"
          ),
          encoding = "UTF-8"
        ),
        "fluctuation" = fread(
          paste0(
            input_path_processed,
            "/df_hurst_covid_mfdfa_data_",
            variable_name,
            "_",
            fixed_window,
            "_",
            gsub("-", "", input_date),
            ".csv"
          ),
          encoding = "UTF-8"
        ),
        "hurst" = fread(
          paste0(
            input_path_processed,
            "/df_hurst_covid_mfdfa_value_",
            variable_name,
            "_",
            fixed_window,
            "_",
            gsub("-", "", input_date),
            ".csv"
          ),
          encoding = "UTF-8"
        ) %>%
          # Normalization of columns type
          mutate(
            coefficient = as.numeric(coefficient),
            hurst_value = as.numeric(hurst_value)
          )
      )
    }
  }
  
  return(list_hurst)
}

# Auxiliary function for preparation of Hurst data ----
get_hurst_covid_resume <- function(
  list_hurst_cases,
  list_hurst_deaths,
  df_hurst_excess,
  days_treshold = 60,
  fixed_window = "no_fixed"
) {
  # Parameters of hurst excess
  excess_coefficient <- df_hurst_excess %>%
    filter(statistic == "Estimate") %>%
    pull(excess_coefficient)
  excess_exponent <- df_hurst_excess %>%
    filter(statistic == "Estimate") %>%
    pull(excess_exponent)
  excess_coefficient_sd <- df_hurst_excess %>%
    filter(statistic == "Standard error") %>%
    pull(excess_coefficient)
  excess_exponent_sd <- df_hurst_excess %>%
    filter(statistic == "Standard error") %>%
    pull(excess_exponent)
  
  # Cases
  df_hurst_cases <- left_join(
    x = list_hurst_cases %>%
      pluck("hurst") %>%
      filter(statistic == "Estimate") %>%
      select(-c(statistic, coefficient)),
    y = list_hurst_cases %>%
      pluck("hurst") %>%
      filter(statistic == "Standard error") %>%
      rename(hurst_sd = hurst_value) %>%
      select(-c(statistic, coefficient)),
    by = c("region", "subregion", "date")
  ) %>% mutate(information = "cases")
  
  # Deaths
  df_hurst_deaths <- left_join(
    x = list_hurst_deaths %>%
      pluck("hurst") %>%
      filter(statistic == "Estimate") %>%
      select(-c(statistic, coefficient)),
    y = list_hurst_deaths %>%
      pluck("hurst") %>%
      filter(statistic == "Standard error") %>%
      rename(hurst_sd = hurst_value) %>%
      select(-c(statistic, coefficient)),
    by = c("region", "subregion", "date")
  ) %>% mutate(information = "deaths")
  
  # Final merge removing Hurst excess
  df_hurst_resume <- df_hurst_cases %>%
    bind_rows(df_hurst_deaths) %>%
    # Estimate length of Hurst Excess
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
    # Estimate Hurst excess
    mutate(
      hurst_excess_fit = exp(excess_coefficient) * length^excess_exponent - 0.5
    ) %>%
    mutate(
      hurst_excess_lower = hurst_excess_fit *
        (1 - log(length) * excess_exponent_sd - excess_coefficient_sd),
      hurst_excess_upper = hurst_excess_fit *
        (1 + log(length) * excess_exponent_sd + excess_coefficient_sd)
    ) %>%
    mutate(
      hurst_value =
        hurst_value - hurst_excess_fit - hurst_sd - hurst_excess_upper,
      hurst_sd = hurst_sd + hurst_excess_upper
    ) %>%
    arrange(region, subregion, information, date)
  
  return(df_hurst_resume)
}
