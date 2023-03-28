# Libraries ----
library(dplyr)
library(purrr)
library(MFDFA)
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

# Estimate excess in the estimation of Hurst in Brownian path with MFDFA ----
estimate_excess_brownian_path_mfdfa <- function(
  n_simulation_vector,
  n_length_vector,
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order_vector,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2023-02-17",
  number = 1
) {
  if(saved_all_data == FALSE) {
    # Selection of loop size
    loop_index <- seq(length(n_simulation_vector))
    
    # Parallel loop over loop_index for estimate Hurst's excess for every length
    df_hurst_brownian_path <- foreach(
      i = loop_index,
      .combine = "bind_rows",
      .errorhandling = c("remove"),
      .export = c("generate_brownian_path"),
      .packages = c(
        "dplyr",
        "purrr",
        "MFDFA",
        "foreach",
        "data.table",
        "doParallel"
      )
    ) %dopar% {
      # Generate Brownian path
      df_brownian_path <- generate_brownian_path(
        n_simulation = n_simulation_vector[i],
        n_length = n_length_vector[i],
        mean = mean,
        sd = sd
      )
      
      df_temp_hurst <- foreach(
        j = df_brownian_path %>% distinct(simulation) %>% pull(),
        .combine = "bind_rows",
        .errorhandling = c("remove"),
        .export = c("generate_brownian_path"),
        .packages = c(
          "dplyr",
          "purrr",
          "MFDFA",
          "foreach",
          "data.table",
          "doParallel"
        )
      ) %dopar% {
        df_hurst_q <- foreach(
          k = q_order_vector,
          .combine = "bind_rows",
          .errorhandling = c("remove"),
          .export = c("generate_brownian_path"),
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
                "Length of Brownian path: ",
                n_length_vector[i],
                ", simulation: ",
                j,
                ", q order: ",
                k,
                "\n"
              ),
              file = paste0(path, "/log_mfdfa_optimum_exponent.txt"),
              append = TRUE
            )
          }
          
          # Estimate Hurst Exponent using MFDFA
          list_temp <- MFDFA(
            tsx = df_brownian_path %>%
              filter(simulation == j) %>%
              pull("value"),
            scale = c(
              floor(0.20 * n_length_vector[i]):floor(0.95 * n_length_vector[i])
            ),
            m = dfa_degree,
            q = k
          )
          
          df_temp <- data.table(
            simulation = j,
            length = n_length_vector[i],
            dfa_degree = dfa_degree,
            q_order = k,
            generalized_hurst = list_temp %>% purrr::pluck("Hq"),
            mass_exponent = list_temp %>% purrr::pluck("tau_q")
          )
          
          df_temp
        }
        
        df_hurst_q
      }
      
      df_temp_hurst
    }
    
    # Save all Hurst data in processed data for not reprocess the information
    write.csv(
      df_hurst_brownian_path,
      paste0(
        input_path_processed,
        "/df_excess_mfdfa_data_",
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
    df_hurst_brownian_path <- fread(
      paste0(
        input_path_processed,
        "/df_excess_mfdfa_data_",
        gsub("-", "", input_date),
        "_",
        number,
        ".csv"
      ),
      encoding = "UTF-8"
    )
  }
  
  return(df_hurst_brownian_path)
}

# Hurst exponent for different lengths obtained in the initial analysis ----
get_mfdfa_resume <- function(df_hurst) {
  df_hurst_resume <- df_hurst %>%
    group_by(length, q_order, dfa_degree) %>%
    summarise(
      hurst_mean = mean(generalized_hurst, na.rm = TRUE),
      hurst_sd = sd(generalized_hurst, na.rm = TRUE) / sqrt(n()),
      mass_exponent_mean = mean(mass_exponent, na.rm = TRUE),
      mass_exponent_sd = sd(mass_exponent, na.rm = TRUE) / sqrt(n())
    ) %>%
    ungroup()
  
  return(df_hurst_resume)
}
