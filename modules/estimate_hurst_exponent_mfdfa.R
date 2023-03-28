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

# Get local trend of DFA in Multifractal Detrended Fluctuation Analysis ----
get_local_trend_mfdfa <- function(df_profile_time_series, dfa_degree = 1) {
  if(
    !"index" %in% names(df_profile_time_series) |
    !"value" %in% names(df_profile_time_series) |
    !"profile_value" %in% names(df_profile_time_series)
  ) {
    cat(
      "Data frame doesn't have the correct names 'index', 'value' and ",
      "'profile_value'\n"
    )
    df_dfa_parameters <- data.table()
    df_dfa_residuals <- data.table()
  } else {
    # Calculate local trend of segments (DFA of dfa_degree-th order)
    dfa_model <- lm(
      df_profile_time_series %>% pull(profile_value) ~
        poly(df_profile_time_series %>% pull(index), dfa_degree, raw = TRUE)
    ) %>% summary()
    
    # Parameters estimation of DFA algorithm
    df_dfa_parameters <- dfa_model %>%
      pluck("coefficients") %>%
      t() %>%
      data.table() %>%
      rename_all(function(x) {paste0("coefficient_", seq(0, dfa_degree))}) %>%
      mutate(
        statistic = c("Estimate", "Standard error", "t value", "Pr(>|t|)")
      ) %>%
      relocate(statistic)
      
    # Residuals after DFA algorithm (needed for variance per segment)
    df_dfa_residuals <- df_profile_time_series %>%
      mutate(residuals = dfa_model %>% pluck("residuals"))
  }
  
  list_dfa <- list(
    "parameters" = df_dfa_parameters,
    "residuals" = df_dfa_residuals
  )
  
  return(list_dfa)
}

# Get total trends of DFA in Multifractal Detrended Fluctuation Analysis ----
get_trend_mfdfa <- function(
  df_time_series,
  window_size,
  dfa_degree = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  if(
    !"index" %in% names(df_time_series) | !"value" %in% names(df_time_series)
  ) {
    cat("Data frame doesn't have the correct names 'index' and 'value'\n")
    list_dfa <- list(
      "parameters" = data.table(),
      "residuals" = data.table()
    )
  } else {
    # Get profile Time Series
    df_time_series <- df_time_series %>%
      # Extract compact support of time series (small fraction of zeros)
      filter(value != 0) %>%
      mutate(profile_value = cumsum(value - mean(value, na.rm = TRUE)))
    
    # Divides time series in non overlapping segments of equal length
    window_number <- nrow(df_time_series) %/% window_size
    window_residue <- nrow(df_time_series) %% window_size
    
    # Auxiliary function for merge list in correct form
    list_join <- function(x, y) {
      for(i in names(x)) {
        x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
      }
      return(x)
    }
    
    # Parallel loop over window_number for estimate local trend in MFDFA
    list_dfa <- foreach(
      i = seq(window_number),
      .combine = "list_join",
      .errorhandling = c("remove"),
      .export = c("get_local_trend_mfdfa"),
      .packages = c("dplyr", "purrr", "data.table")
    ) %dopar% {
      # Log output for monitoring progress
      if(verbose >= 1){
        cat(
          paste0(
            "Finish process in positive loop, window ",
            i,
            " length ",
            window_size,
            "\n"
          ),
          file = paste0(path, "/log_hurst_mfdfa_trend.txt"),
          append = TRUE
        )
      }
      
      # Estimate local trend of Multifractal Detrended Fluctuation Analysis
      list_temp <- get_local_trend_mfdfa(
        df_profile_time_series = df_time_series %>%
          filter(
            index >= (i-1) * window_size + 1,
            index <= i * window_size
          ),
        dfa_degree = dfa_degree
      )
      
      # Allocation of window size in list_temp and variance of residuals
      df_dfa_parameters <- list_temp %>%
        pluck("parameters") %>%
        mutate(
          window_size = window_size,
          window_number = i
        ) %>%
        relocate(c(window_size, window_number))
      
      df_dfa_residuals <- list_temp %>%
        pluck("residuals") %>%
        mutate(
          window_size = window_size,
          window_number = i,
          biased_variance = var(residuals, na.rm = TRUE),
          unbiased_variance = sum(residuals^2, na.rm = TRUE) / window_size
        ) %>%
        relocate(c(window_size, window_number))
      
      # Actualization of list_temp
      list_temp <- list(
        "parameters" = df_dfa_parameters,
        "residuals" = df_dfa_residuals
      )
      
      list_temp
    }
    
    # Estimate local trend when total length isn't a multiple of window_size
    if(window_residue != 0){
      # Parallel loop over window_number for estimate local trend in MFDFA
      list_dfa_aux <- foreach(
        i = seq(window_number),
        .combine = "list_join",
        .errorhandling = c("remove"),
        .export = c("get_local_trend_mfdfa"),
        .packages = c("dplyr", "purrr", "data.table")
      ) %dopar% {
        # Log output for monitoring progress
        if(verbose >= 1){
          cat(
            paste0(
              "Finish process in negative loop, window ",
              i,
              " length ",
              window_size,
              "\n"
            ),
            file = paste0(path, "/log_hurst_mfdfa_trend.txt"),
            append = TRUE
          )
        }
        
        # Estimate local trend of Multifractal Detrended Fluctuation Analysis
        list_temp <- get_local_trend_mfdfa(
          df_profile_time_series = df_time_series %>%
            filter(
              index >= (window_number - i) * window_size + 1,
              index <= (window_number - i + 1) * window_size
            ),
          dfa_degree = dfa_degree
        )
        
        # Allocation of window size in list_temp and variance of residuals
        df_dfa_parameters <- list_temp %>%
          pluck("parameters") %>%
          mutate(
            window_size = window_size,
            window_number = i + window_number
          ) %>%
          relocate(c(window_size, window_number))
        
        df_dfa_residuals <- list_temp %>%
          pluck("residuals") %>%
          mutate(
            window_size = window_size,
            window_number = i + window_number,
            biased_variance = var(residuals, na.rm = TRUE),
            unbiased_variance = sum(residuals^2, na.rm = TRUE) / window_size
          ) %>%
          relocate(c(window_size, window_number))
        
        # Actualization of list_temp
        list_temp <- list(
          "parameters" = df_dfa_parameters,
          "residuals" = df_dfa_residuals
        )
        
        list_temp
      }
      
      # Allocation of new values in parameters and residuals
      list_dfa <- list_dfa %>% list_join(list_dfa_aux)
    }
  }
  
  return(list_dfa)
}

# q-th order fluctuation in Multifractal Detrended Fluctuation Analysis ----
calculate_fluctuation <- function(df_dfa_residuals, q_order = 2) {
  if(q_order != 0) {
    df_fluctuation <- df_dfa_residuals %>%
      distinct(
        window_size,
        window_number,
        biased_variance,
        unbiased_variance
      ) %>%
      group_by(window_size) %>%
      summarise(
        biased_fluctuation =
          (sum(biased_variance^(q_order / 2), na.rm = TRUE) /
             max(window_number, na.rm = TRUE))^(1 / q_order),
        unbiased_fluctuation =
          (sum(unbiased_variance^(q_order / 2), na.rm = TRUE) /
             max(window_number, na.rm = TRUE))^(1 / q_order)
      ) %>%
      mutate(order = q_order) %>%
      relocate(order)
  } else {
    df_fluctuation <- df_dfa_residuals %>%
      distinct(
        window_size,
        window_number,
        biased_variance,
        unbiased_variance
      ) %>%
      group_by(window_size) %>%
      summarise(
        biased_fluctuation = exp(
          0.5 *
            sum(log(abs(biased_variance^2)), na.rm = TRUE) /
            max(window_number, na.rm = TRUE)
        ),
        unbiased_fluctuation = exp(
          0.5 *
            sum(log(abs(unbiased_variance^2)), na.rm = TRUE) /
            max(window_number, na.rm = TRUE)
        )
      ) %>%
      mutate(order = q_order) %>%
      relocate(order)
  }
  
  return(df_fluctuation)
}

# Estimate Hurst Exponent using Multifractal Detrended Fluctuation Analysis ----
estimate_hurst_mfdfa <- function(
  df_time_series,
  dfa_degree = 1,
  q_order = 2,
  scale_min = 0.2,
  scale_max = 1.0,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  if(
    !"index" %in% names(df_time_series) | !"value" %in% names(df_time_series)
  ) {
    cat("Data frame doesn't have the correct names 'index' and 'value'\n")
    list_hurst <- list(
      "parameters" = data.table(),
      "residuals" = data.table(),
      "fluctuation" = data.table(),
      "hurst" = data.table()
    )
  } else {
    # Selection of windows size
    loop_index <- df_time_series %>%
      filter(
        index >= scale_min * nrow(df_time_series), 
        index < scale_max * nrow(df_time_series),
        index %% n_step == 0
      ) %>%
      distinct(index) %>%
      pull()
    
    # Auxiliary function for merge list in correct form
    list_join <- function(x, y) {
      for(i in names(x)) {
        x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
      }
      return(x)
    }
    
    # Parallel loop over loop_index for estimate rs
    list_hurst <- foreach(
      i = loop_index,
      .combine = "list_join",
      .errorhandling = c("remove"),
      .export = c(
        "get_local_trend_mfdfa",
        "get_trend_mfdfa",
        "calculate_fluctuation"
      ),
      .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
    ) %dopar% {
      # Variance per window length
      list_temp <- get_trend_mfdfa(
        df_time_series = df_time_series,
        window_size = i,
        dfa_degree = dfa_degree,
        path = path,
        verbose = verbose
      )
      
      # Fluctuation per window length
      list_temp <- list_temp %>% list_merge(
        "fluctuation" = calculate_fluctuation(
          df_dfa_residuals = list_temp %>% pluck("residuals"),
          q_order = q_order
        )
      )
      
      list_temp
    }
    
    # Estimate Hurst Exponent as an exponent of power law of q-Fluctuation
    list_hurst <- list(
      "parameters" = list_hurst %>% pluck("parameters"),
      "residuals" = list_hurst %>% pluck("residuals"),
      "fluctuation" = list_hurst %>% pluck("fluctuation"),
      "hurst" = lm(
        list_hurst %>%
          pluck("fluctuation") %>%
          pull(unbiased_fluctuation) %>%
          log() ~
          list_hurst %>%
          pluck("fluctuation") %>%
          pull(window_size) %>%
          log()
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
    )
  }
  
  return(list_hurst)
}

# Estimate Hurst Exponent of Brownian path using Multifractal DFA ----
estimate_hurst_brownian_path_mfdfa <- function(
  df_brownian_path,
  dfa_degree = 1,
  q_order = 2,
  scale_min = 0.2,
  scale_max = 1.0,
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
    .errorhandling = c("remove"),
    .export = c(
      "estimate_hurst_mfdfa",
      "calculate_fluctuation",
      "get_trend_mfdfa",
      "get_local_trend_mfdfa"
    ),
    .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    list_temp <- estimate_hurst_mfdfa(
      df_time_series = df_brownian_path %>% filter(simulation == i),
      dfa_degree = dfa_degree,
      q_order = q_order,
      scale_min = scale_min,
      scale_max = scale_max,
      n_step = n_step,
      path = path,
      verbose = verbose
    )
    
    # Log output for monitoring progress
    if(verbose >= 1){
      cat(
        paste0("Estimated Hurst for simulation of Brownian path: ", i, "\n"),
        file = paste0(path, "/log_hurst_normal_mfdfa.txt"),
        append = TRUE
      )
    }
    
    # Actualization of list_temp
    list_temp <- list(
      "parameters" = list_temp %>%
        pluck("parameters") %>%
        mutate(simulation = i),
      "residuals" = list_temp %>%
        pluck("residuals") %>%
        mutate(simulation = i),
      "fluctuation" = list_temp %>%
        pluck("fluctuation") %>%
        mutate(simulation = i),
      "hurst" = list_temp %>%
        pluck("hurst") %>%
        mutate(simulation = i)
    )
    
    list_temp
  }
  
  return(list_hurst_brownian_path)
}

# Estimate excess in the estimation of Hurst in Brownian path with MFDFA ----
estimate_excess_hurst_brownian_path_mfdfa <- function(
  n_simulation_vector,
  n_length_vector,
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  scale_min = 0.2,
  scale_max = 1.0,
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
      .errorhandling = c("remove"),
      .export = c(
        "estimate_hurst_brownian_path_mfdfa",
        "estimate_hurst_mfdfa",
        "calculate_fluctuation",
        "get_trend_mfdfa",
        "get_local_trend_mfdfa",
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
          file = paste0(path, "/log_hurst_optimum_mfdfa.txt"),
          append = TRUE
        )
      }
      
      # Temporal dataframe of Hurst Exponent
      list_temp <- estimate_hurst_brownian_path_mfdfa(
        df_brownian_path = df_brownian_path,
        dfa_degree = dfa_degree,
        q_order = q_order,
        scale_min = scale_min,
        scale_max = scale_max,
        n_step = n_step,
        path = path,
        verbose = verbose
      )
      
      # Actualization of list_temp
      list_temp <- list(
        "parameters" = list_temp %>%
          pluck("parameters") %>%
          mutate(length = n_length_vector[i]),
        "residuals" = list_temp %>%
          pluck("residuals") %>%
          mutate(length = n_length_vector[i]),
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
      list_hurst_brownian_path %>% pluck("parameters"),
      paste0(
        input_path_processed,
        "/df_hurst_excess_mfdfa_dfa_data_",
        gsub("-", "", input_date),
        "_",
        number,
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
    write.csv(
      list_hurst_brownian_path %>% pluck("residuals"),
      paste0(
        input_path_processed,
        "/df_hurst_excess_mfdfa_dfa_residuals_",
        gsub("-", "", input_date),
        "_",
        number,
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
    write.csv(
      list_hurst_brownian_path %>% pluck("fluctuation"),
      paste0(
        input_path_processed,
        "/df_hurst_excess_mfdfa_data_",
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
        "/df_hurst_excess_mfdfa_value_",
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
      "parameters" = fread(
        paste0(
          input_path_processed,
          "/df_hurst_excess_mfdfa_dfa_data_",
          gsub("-", "", input_date),
          "_",
          number,
          ".csv"
        ),
        encoding = "UTF-8"
      ),
      "residuals" = fread(
        paste0(
          input_path_processed,
          "/df_hurst_excess_mfdfa_dfa_residuals_",
          gsub("-", "", input_date),
          "_",
          number,
          ".csv"
        ),
        encoding = "UTF-8"
      ),
      "fluctuation" = fread(
        paste0(
          input_path_processed,
          "/df_hurst_excess_mfdfa_data_",
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
          "/df_hurst_excess_mfdfa_value_",
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

# Estimate Multiscaling exponents using MFDFA with many q-order ----
get_multiscaling_exponents_mfdfa <- function(
  df_time_series,
  dfa_degree = 1,
  q_order_vector,
  scale_min = 0.2,
  scale_max = 1.0,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
) {
  # Auxiliary function for merge list in correct form
  list_join <- function(x, y) {
    for(i in names(x)) {
      x[[i]] <- x[[i]] %>% bind_rows(y[[i]])
    }
    return(x)
  }
  
  # Parallel loop over loop_index for estimate Hurst in every simulation
  list_multiscaling_exponents <- foreach(
    i = q_order_vector,
    .combine = "list_join",
    .errorhandling = c("remove"),
    .export = c(
      "estimate_hurst_mfdfa",
      "calculate_fluctuation",
      "get_trend_mfdfa",
      "get_local_trend_mfdfa"
    ),
    .packages = c("dplyr", "purrr", "foreach", "data.table", "doParallel")
  ) %dopar% {
    list_temp <- estimate_hurst_mfdfa(
      df_time_series = df_time_series,
      dfa_degree = dfa_degree,
      q_order = i,
      scale_min = scale_min,
      scale_max = scale_max,
      n_step = n_step,
      path = path,
      verbose = verbose
    )
    
    # Log output for monitoring progress
    if(verbose >= 1){
      cat(
        paste0("Estimated Hurst for q-order: ", i, "\n"),
        file = paste0(path, "/log_hurst_multiscaling_exponents_mfdfa.txt"),
        append = TRUE
      )
    }
    
    # Actualization of list_temp
    list_temp <- list(
      "parameters" = list_temp %>%
        pluck("parameters") %>%
        mutate(order = i),
      "residuals" = list_temp %>%
        pluck("residuals") %>%
        mutate(order = i),
      "fluctuation" = list_temp %>%
        pluck("fluctuation"),
      "hurst" = list_temp %>%
        pluck("hurst") %>%
        mutate(order = i)
    )
    
    list_temp
  }
  
  return(list_multiscaling_exponents)
}
