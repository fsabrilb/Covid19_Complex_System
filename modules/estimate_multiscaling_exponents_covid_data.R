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

# Estimate Multiscaling exponents for a particular region and date ----
estimate_multiscaling_exponents_local <- function(
  df_hurst,
  days_treshold = 60,
  dfa_degree = 1,
  q_order_vector,
  scale_min = 0.2,
  scale_max = 1.0,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  if(nrow(df_hurst) < days_treshold) {
    cat("Data frame doesn't have minimum number of rows\n")
    # Generate Multiscaling Data
    list_hurst <- list(
      "parameters" = data.table(),
      "residuals" = data.table(),
      "fluctuation" = data.table(),
      "hurst" = data.table()
    )
  } else {
    # Generate Hurst Data
    list_hurst <- get_multiscaling_exponents_mfdfa(
      df_time_series = df_hurst,
      dfa_degree = dfa_degree,
      q_order_vector = q_order_vector,
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
  
  return(list_hurst)
}

# Estimate Multiscaling exponents for a particular date ----
estimate_multiscaling_exponents_regions <- function(
  df_hurst,
  days_treshold = 60,
  dfa_degree = 1,
  q_order_vector,
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
  
  # Parallel loop over loop_index for estimate Multiscaling exponents per region
  list_hurst <- foreach(
    i = loop_index,
    .combine = "list_join",
    .errorhandling = c("remove"),
    .export = c(
      "estimate_hurst_mfdfa",
      "calculate_fluctuation",
      "get_trend_mfdfa",
      "get_local_trend_mfdfa",
      "get_multiscaling_exponents_mfdfa",
      "estimate_multiscaling_exponents_local"
    ),
    .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    # Log output for monitoring progress
    if(verbose >= 1){
      cat(
        paste0(
          "Estimate multiscaling exponent for region ", i, "\n"
        ),
        file = paste0(path, "/log_multiscaling_exponents_covid_regions.txt"),
        append = TRUE
      )
    }
    
    # Generate local Hurst data
    list_temp <- estimate_multiscaling_exponents_local(
      df_hurst = df_hurst %>% filter(region == i),
      days_treshold = days_treshold,
      dfa_degree = dfa_degree,
      q_order_vector = q_order_vector,
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

# Estimate Multiscaling exponents by spatial clustering ----
estimate_multiscaling_exponents_covid <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  dfa_degree = 1,
  q_order_vector,
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
    # Multiscaling exponent data
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
    
    # Parallel loop over loop_index for estimate Multiscaling exponents per date
    list_hurst <- foreach(
      i = loop_index,
      .combine = "list_join",
      .errorhandling = c("remove"),
      .export = c(
        "estimate_hurst_mfdfa",
        "calculate_fluctuation",
        "get_trend_mfdfa",
        "get_local_trend_mfdfa",
        "get_multiscaling_exponents_mfdfa",
        "estimate_multiscaling_exponents_local",
        "estimate_multiscaling_exponents_regions"
      ),
      .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
    ) %dopar% {
      
      # Log output for monitoring progress
      if(verbose >= 1){
        cat(
          paste0(
            "Estimate Multiscaling exponents for date ",
            i,
            " window: ",
            fixed_window,
            "\n"
          ),
          file = paste0(path, "/log_multiscaling_exponents_covid_dates.txt"),
          append = TRUE
        )
      }
      
      # Renovation of initial_date for fixed_window
      if(fixed_window == "fixed") {
        initial_date <- i - days_treshold
      }
      
      # Generate local Hurst data
      list_temp <- estimate_multiscaling_exponents_regions(
        df_hurst = df_hurst %>%
          filter(date >= initial_date, date <= i) %>%
          group_by(region, subregion) %>%
          mutate(index = index - min(index, na.rm = TRUE)) %>%
          ungroup(),
        days_treshold = days_treshold,
        dfa_degree = dfa_degree,
        q_order_vector = q_order_vector,
        scale_min = scale_min,
        scale_max = scale_max,
        n_step = n_step,
        path = path,
        verbose = verbose
      )
      
      list_temp
    }
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      list_hurst %>% pluck("parameters"),
      paste0(
        input_path_processed,
        "/df_multiscaling_covid_mfdfa_dfa_data_",
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
        "/df_multiscaling_covid_mfdfa_dfa_residuals_",
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
        "/df_multiscaling_covid_mfdfa_data_",
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
        "/df_multiscaling_covid_mfdfa_value_",
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
    list_hurst <- list(
      "parameters" = fread(
        paste0(
          input_path_processed,
          "/df_multiscaling_covid_mfdfa_dfa_data_",
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
          "/df_multiscaling_covid_mfdfa_dfa_residuals_",
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
          "/df_multiscaling_covid_mfdfa_data_",
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
          "/df_multiscaling_covid_mfdfa_value_",
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
  
  return(list_hurst)
}

# Estimate Multifractal DFA analysis by spatial clustering ----
estimate_mfdfa_analysis_covid <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  dfa_degree = 1,
  q_order_vector = c(2),
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
    
    # Parallel loop over loop_index for estimate Hurst per date
    df_mfdfa <- foreach(
      i = loop_index,
      .combine = "bind_rows",
      .errorhandling = c("remove"),
      .packages = c(
        "dplyr",
        "purrr",
        "MFDFA",
        "foreach",
        "data.table",
        "doParallel"
      )
    ) %dopar% {
      # Renovation of initial_date for fixed_window
      if(fixed_window == "fixed") {
        initial_date <- i - days_treshold
      }
      
      # Generate local Hurst data
      df_hurst_aux <- df_hurst %>%
        filter(date >= initial_date, date <= i) %>%
        group_by(region, subregion) %>%
        mutate(index = index - min(index, na.rm = TRUE)) %>%
        ungroup()
      
      # Loop index over regions
      loop_index_2 <- df_hurst_aux %>%
        distinct(region) %>%
        pull()
      
      # Generate MFDFA analysis per region
      df_mfdfa_region <- foreach(
        j = loop_index_2,
        .combine = "bind_rows",
        .errorhandling = c("remove"),
        .packages = c(
          "dplyr",
          "purrr",
          "MFDFA",
          "foreach",
          "data.table",
          "doParallel"
        )
      ) %dopar% {
        # Generate MFDFA analysis per q-order
        df_mfdfa_q <- foreach(
          k = q_order_vector,
          .combine = "bind_rows",
          .errorhandling = c("remove"),
          .packages = c(
            "dplyr",
            "purrr",
            "MFDFA",
            "foreach",
            "data.table",
            "doParallel"
          )
        ) %dopar% {
          # Log output for monitoring progress
          if(verbose >= 1){
            cat(
              paste0(
                "Estimate ",
                fixed_window,
                " MFDFA for date ",
                i,
                " region: ",
                j,
                " order: ",
                k,
                "\n"
              ),
              file = paste0(path, "/log_mfdfa_analysis_covid.txt"),
              append = TRUE
            )
          }
          
          # Estimate MFDFA analysis using MFDFA library
          df_ts <- df_hurst_aux %>%
            filter(region == as.character(j)) %>%
            pull("value")
          
          list_temp <- MFDFA(
            tsx = df_ts,
            scale = c(floor(0.20 * length(df_ts)):floor(0.85 * length(df_ts))),
            m = dfa_degree,
            q = k
          )
          
          # Construct dataframe of MFDFA analysis
          df_temp <- data.table(
            region = j,
            date = i,
            q_order = k,
            generalized_hurst = list_temp %>% purrr::pluck("Hq"),
            mass_exponent = list_temp %>% purrr::pluck("tau_q")
          )
          
          df_temp
        }
        
        df_mfdfa_q
      }
      
      df_mfdfa_region
    }
    
    # Merge final data of subregion
    df_mfdfa <- df_mfdfa %>%
      left_join(
        df_hurst %>% distinct(region, subregion, date),
        by = c("region", "date")
      ) %>%
      relocate(subregion, .after = region) %>%
      arrange(region, subregion, date, q_order)
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_mfdfa,
      paste0(
        input_path_processed,
        "/df_mfdfa_analysis_covid_",
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
    df_mfdfa <- fread(
      paste0(
        input_path_processed,
        "/df_mfdfa_analysis_covid_",
        variable_name,
        "_",
        fixed_window,
        "_",
        gsub("-", "", input_date),
        ".csv"
      )
    ) %>% mutate(date = as.Date(date))
    
  }
  
  return(df_mfdfa)
}

# Auxiliary function for preparation of Multiscaling exponents data ----
get_multiscaling_exponents_covid_resume <- function(
  list_multiscaling_exponents_cases,
  list_multiscaling_exponents_deaths
) {
  # Cases
  df_hurst_cases <- left_join(
    x = list_multiscaling_exponents_cases %>%
      pluck("hurst") %>%
      filter(statistic == "Estimate") %>%
      select(-c(statistic, coefficient)),
    y = list_multiscaling_exponents_cases %>%
      pluck("hurst") %>%
      filter(statistic == "Standard error") %>%
      rename(hurst_sd = hurst_value) %>%
      select(-c(statistic, coefficient)),
    by = c("region", "subregion", "date", "order")
  ) %>% mutate(information = "cases")
  
  # Deaths
  df_hurst_deaths <- left_join(
    x = list_multiscaling_exponents_deaths %>%
      pluck("hurst") %>%
      filter(statistic == "Estimate") %>%
      select(-c(statistic, coefficient)),
    y = list_multiscaling_exponents_deaths %>%
      pluck("hurst") %>%
      filter(statistic == "Standard error") %>%
      rename(hurst_sd = hurst_value) %>%
      select(-c(statistic, coefficient)),
    by = c("region", "subregion", "date", "order")
  ) %>% mutate(information = "deaths")
  
  # Final merge
  df_hurst_resume <- df_hurst_cases %>%
    bind_rows(df_hurst_deaths) %>%
    arrange(region, subregion, information, date, order)
  
  return(df_hurst_resume)
}
