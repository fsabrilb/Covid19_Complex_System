# Libraries ----
library(dplyr)
library(purrr)
library(foreach)
library(data.table)
library(doParallel)

# Generation of Brownian paths ----
generate_brownian_path <- function(
  n_simulation,
  n_length,
  mean = 0,
  sd = 1
) {
  df_brownian_path <- data.table()
  for(i in seq(n_simulation)) {
    df_brownian_path <- df_brownian_path %>%
      bind_rows(
        data.table(
          simulation = i,
          length = n_length,
          index = seq(n_length),
          value = rnorm(n = n_length, mean = mean, sd = sd)
        )
      )
  }
  
  return(df_brownian_path)
}

# Auxiliary function for estimate local Rescaled range (R/S) ----
estimate_local_rs <- function(x) {
  # Profile
  y <- cumsum(x - mean(x, na.rm = TRUE))
  # Rescaled range
  rs <- if_else(
    max(y, na.rm = TRUE) != min(y, na.rm = TRUE),
    (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / sd(x, na.rm = TRUE),
    0
  )

  return(rs)
}

# Estimate Hurst Exponent using Rescaled range (R/S) analysis ----
estimate_local_hurst_rs <- function(x, window_size) {
  rs <- data.table(
    index = seq(length(x)),
    value = x
  ) %>%
    # Clustering of time series in windows of size window_size
    mutate(window_number = seq(length(x)) %/% window_size) %>%
    # Estimate local behavior of Rescaled range (R/S)
    group_by(window_number) %>%
    mutate(rs = estimate_local_rs(value)) %>%
    ungroup() %>%
    # Average over all partial time series of length window_size
    distinct(rs) %>%
    pull() %>%
    mean(na.rm = TRUE)
    
  return(rs)
}

# Estimate Hurst Exponent using Rescaled range (R/S) analysis ----
estimate_hurst_rs <- function(df_time_series, n_step = 1) {
  if(
    !"index" %in% names(df_time_series) | !"value" %in% names(df_time_series)
  ) {
    cat("Data frame doesn't have the correct names 'index' and 'value'\n")
    list_hurst <- list(
      "fluctuation" = data.table(),
      "hurst" = data.table()
    )
  } else {
    # Selection of windows size
    loop_index <- df_time_series %>%
      filter(
        index >= 0.52 * sqrt(nrow(df_time_series)), 
        index < 2.72 * sqrt(nrow(df_time_series)),
        index %% n_step == 0
      ) %>%
      distinct(index) %>%
      pull()
    
    # Parallel loop over loop_index for estimate rs
    df_fluctuation <- foreach(
      i = loop_index,
      .combine = bind_rows,
      .export = c("estimate_local_hurst_rs", "estimate_local_rs"),
      .packages = c("dplyr", "data.table")
    ) %dopar% {
      df_temp <- data.table(
        tau = i,
        rs = estimate_local_hurst_rs(df_time_series %>% pull(value), i)
      )
      
      df_temp
    }
    
    # Estimate Hurst Exponent as an exponent of power law of RS
    df_hurst <- lm(
      df_fluctuation %>% pull(rs) %>% log() ~
        df_fluctuation %>% pull(tau) %>% log()
    ) %>%
      summary() %>%
      pluck("coefficients") %>%
      t() %>%
      data.table() %>%
      rename_all(function(x) {c("coefficient", "hurst_value")}) %>%
      mutate(
        statistic = c("Estimate", "Standard error", "t value", "Pr(>|t|)")
      ) %>%
      relocate(statistic)
    
    list_hurst <- list(
      "fluctuation" = df_fluctuation,
      "hurst" = df_hurst
    )
  }
  
  return(list_hurst)
}

# Estimate Hurst Exponent of Brownian path using R/S analysis ----
estimate_hurst_brownian_path_rs <- function(
  df_brownian_path,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  # Selection of simulation
  loop_index <- df_brownian_path %>% distinct(simulation) %>% pull()
  
  # Auxiliary function for merge list in correct form
  list_join <- function(x, y) {
    for(i in names(x)) {
      x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
    }
    return(x)
  }
  
  # Parallel loop over loop_index for estimate Hurst in every simulation
  list_hurst_brownian_path <- foreach(
    i = loop_index,
    .combine = "list_join",
    .export = c(
      "estimate_hurst_rs", "estimate_local_hurst_rs", "estimate_local_rs"
    ),
    .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    list_temp <- estimate_hurst_rs(
      df_time_series = df_brownian_path %>% filter(simulation == i),
      n_step = n_step
    )
    
    # Log output for monitoring progress
    if(verbose >= 1){
      cat(
        paste0("Estimated Hurst for simulation of Brownian path: ", i, "\n"),
        file = paste0(path, "/log_hurst_normal_rs.txt"),
        append = TRUE
      )
    }
    
    # Actualization of list_temp
    list_temp <- list(
      "fluctuation" = list_temp %>%
        pluck("fluctuation")  %>%
        mutate(simulation = i),
      "hurst" = list_temp %>%
        pluck("hurst") %>%
        mutate(simulation = i)
    )
    
    list_temp
  }
  
  return(list_hurst_brownian_path)
}

# Estimate excess in the estimation of Hurst in Brownian path with R/S ----
estimate_excess_hurst_brownian_path_rs <- function(
  n_simulation_vector,
  n_length_vector,
  mean = 0,
  sd = 1,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04",
  number = 1
) {
  if(saved_all_data == FALSE) {
    # Selection of loop size
    loop_index <- seq(length(n_simulation_vector))
    
    # Auxiliary function for merge list in correct form
    list_join <- function(x, y) {
      for(i in names(x)) {
        x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
      }
      return(x)
    }
    
    # Parallel loop over loop_index for estimate Hurst's excess for every length
    list_hurst_brownian_path <- foreach(
      i = loop_index,
      .combine = "list_join",
      .export = c(
        "estimate_hurst_brownian_path_rs",
        "estimate_hurst_rs",
        "estimate_local_hurst_rs",
        "estimate_local_rs",
        "generate_brownian_path"
      ),
      .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
    ) %dopar% {
      # Generate Brownian path
      df_brownian_path <- generate_brownian_path(
        n_simulation = n_simulation_vector[i],
        n_length = n_length_vector[i],
        mean = mean,
        sd = sd
      )
      
      # Log output for monitoring progress
      if(verbose >= 1){
        cat(
          paste0(
            "------------ Length of Brownian path: ",
            n_length_vector[i],
            " ------------\n"
          ),
          file = paste0(path, "/log_hurst_optimum_rs.txt"),
          append = TRUE
        )
      }
      
      # Temporal dataframe of Hurst Exponent
      list_temp <- estimate_hurst_brownian_path_rs(
        df_brownian_path = df_brownian_path,
        n_step = n_step,
        path = path,
        verbose = verbose
      )
      
      # Actualization of list_temp
      list_temp <- list(
        "fluctuation" = list_temp %>%
          pluck("fluctuation") %>%
          mutate(length = n_length_vector[i]),
        "hurst" = list_temp %>%
          pluck("hurst") %>%
          mutate(length = n_length_vector[i])
      )
      
      list_temp
    }
    
    # Save all Hurst data in processed data for not reprocess the information
    write.csv(
      list_hurst_brownian_path %>% pluck("fluctuation"),
      paste0(
        input_path_processed,
        "/df_hurst_excess_rs_data_",
        gsub("-", "", input_date),
        "_",
        number,
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
    write.csv(
      list_hurst_brownian_path %>% pluck("hurst"),
      paste0(
        input_path_processed,
        "/df_hurst_excess_rs_value_",
        gsub("-", "", input_date),
        "_",
        number,
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
  } else {
    # Load all data from processed data for not reprocess the information
    list_hurst_brownian_path <- list(
      "fluctuation" = fread(
        paste0(
          input_path_processed,
          "/df_hurst_excess_rs_data_",
          gsub("-", "", input_date),
          "_",
          number,
          ".csv"
        ),
        encoding = "UTF-8"
      ),
      "hurst" = fread(
        paste0(
          input_path_processed,
          "/df_hurst_excess_rs_value_",
          gsub("-", "", input_date),
          "_",
          number,
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
  
  return(list_hurst_brownian_path)
}
