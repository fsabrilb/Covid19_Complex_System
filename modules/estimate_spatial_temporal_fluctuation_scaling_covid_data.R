# Libraries ----
library(dplyr)
library(data.table)

# Estimate temporal fluctuation scaling (TFS) by spatial clustering ----
estimate_spatial_tfs <- function(
  df_covid,
  days_vector,
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04",
  number = 1
) {
  # Function control
  if(length(days_vector) <= 1) {
    cat(paste0("Length of days_vector is invalid: ", length(days_vector), "\n"))
    df_tfs <- 0
  } else {
    if(saved_all_data == FALSE) {
      # Filtering non-zero data
      df_spatial_tfs <- df_covid %>%
        filter(cum_cases != 0, cum_deaths != 0)
      
      # Loop over dates
      df_tfs <- data.table()
      for(i in seq(2, length(days_vector))) {
        df_tfs <- df_tfs %>%
          bind_rows(
            df_spatial_tfs %>%
              # Filter by dates (clustering in dates)
              filter(
                date > days_vector[i-1],
                date <= days_vector[i]
              ) %>%
              # Creating Spatial Temporal fluctuation scaling (TFS) data
              group_by(region, subregion, date) %>%
              summarise(
                mean_cum_cases = mean(cum_cases, na.rm = TRUE),
                sd_cum_cases = sd(cum_cases, na.rm = TRUE),
                mean_cum_deaths = mean(cum_deaths, na.rm = TRUE),
                sd_cum_deaths = sd(cum_deaths, na.rm = TRUE),
                start_date = as.Date(days_vector[i-1]),
                end_date = as.Date(days_vector[i])
              ) %>%
              ungroup() %>%
              # Estimate Spatial TFS parameters (regression to power law)
              group_by(region, subregion, start_date, end_date) %>%
              mutate(
                coefficient_cases = 2 * cov(
                  x = log(mean_cum_cases),
                  y = log(sd_cum_cases),
                  use = "complete.obs"
                ) / var(log(mean_cum_cases), na.rm = TRUE),
                coefficient_deaths = 2 * cov(
                  x = log(mean_cum_deaths),
                  y = log(sd_cum_deaths),
                  use = "complete.obs"
                ) / var(log(mean_cum_deaths), na.rm = TRUE)
              ) %>%
              mutate(
                alpha_cases = exp(
                  2 * (
                    mean(log(sd_cum_cases), na.rm = TRUE) -
                      coefficient_cases *
                      mean(log(mean_cum_cases), na.rm = TRUE)
                  )
                ),
                alpha_deaths = exp(
                  2 * (
                    mean(log(sd_cum_deaths), na.rm = TRUE) -
                      coefficient_deaths *
                      mean(log(mean_cum_deaths), na.rm = TRUE)
                  )
                ),
                r2_cases = cor(
                  x = log(mean_cum_cases),
                  y = log(sd_cum_cases),
                  use = "complete.obs"
                )^2,
                r2_deaths = cor(
                  x = log(mean_cum_deaths),
                  y = log(sd_cum_deaths),
                  use = "complete.obs"
                )^2
              ) %>%
              ungroup()
          )
        
        # Function development
        if(verbose >= 1) {
          cat(
            paste0(
              "Spatial TFS for dates between ",
              as.Date(days_vector[i-1]),
              " and ",
              as.Date(days_vector[i]),
              "\n"
            )
          )
        }
      }
      
      # Normalization of columns type
      df_tfs <- df_tfs %>%
        relocate(c(region, subregion, start_date, end_date)) %>%
        mutate(
          coefficient_cases = as.numeric(coefficient_cases),
          alpha_cases = as.numeric(alpha_cases),
          r2_cases = as.numeric(r2_cases),
          coefficient_deaths = as.numeric(coefficient_deaths),
          alpha_deaths = as.numeric(alpha_deaths),
          r2_deaths = as.numeric(r2_deaths)
        ) %>%
        data.table()
      
      # Save all Covid data in processed data for not reprocess the information
      write.csv(
        df_tfs,
        paste0(
          input_path_processed,
          "/df_spatial_tfs_covid_data_",
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
      df_tfs <- fread(
        paste0(
          input_path_processed,
          "/df_spatial_tfs_covid_data_",
          gsub("-", "", input_date),
          "_",
          number,
          ".csv"
        ),
        encoding = "UTF-8"
      ) %>%
        # Normalization of columns type
        mutate(
          date = as.Date(date),
          start_date = as.Date(start_date),
          end_date = as.Date(end_date),
          coefficient_cases = as.numeric(coefficient_cases),
          alpha_cases = as.numeric(alpha_cases),
          r2_cases = as.numeric(r2_cases),
          coefficient_deaths = as.numeric(coefficient_deaths),
          alpha_deaths = as.numeric(alpha_deaths),
          r2_deaths = as.numeric(r2_deaths)
        )
    }
  }
  
  return(df_tfs)
}
