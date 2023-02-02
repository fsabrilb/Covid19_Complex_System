# Libraries ----
library(dplyr)
library(purrr)
library(data.table)

# Auxiliary function for estimate Partial autocorrelation (PACF) with dates ----
calculate_pacf <- function(dates, x) {
  # Partial autocorrelation
  df_pacf <- bind_rows(
    list(
      # Time lag
      "lag" = pacf(
        x,
        lag.max = length(x),
        plot = FALSE,
        na.action = na.omit
      ) %>%
        pluck("lag") %>%
        as.vector(),
      # Cross-correlation value
      "pacf" = pacf(
        x,
        lag.max = length(x),
        plot = FALSE,
        na.action = na.omit
      ) %>%
        pluck("acf") %>%
        as.vector()
    )
  ) %>%
    # Insertion of dates and relocation
    mutate("date" = dates) %>%
    relocate(date)
  
  return(df_pacf)
}

# Auxiliary function for estimate PACF and Cross-correlation with dates ----
calculate_correlations <- function(dates, x, y) {
  # Cross-correlation (normalization N - tau made in ACF in each time step)
  df_corrxy <- bind_rows(
    list(
      # Time lag
      "lag" = ccf(
        x,
        y,
        lag.max = length(x),
        plot = FALSE,
        na.action = na.omit
      ) %>%
        pluck("lag") %>%
        as.vector(),
      # Cross-correlation value
      "ccf" = ccf(
        x,
        y,
        lag.max = length(x),
        plot = FALSE,
        na.action = na.omit
      ) %>%
        pluck("acf") %>%
        as.vector()
    )
  ) %>%
    # Filtering process (Exclude negative values in lag by symmetry)
    filter(lag >= 0) %>%
    # Insertion of dates and relocation
    mutate("date" = dates) %>%
    # Partial autocorrelation of x and y
    left_join(
      calculate_pacf(dates %>% tail(-1), x),
      by = c("date", "lag")
    ) %>%
    rename(pacf_x = pacf) %>%
    left_join(
      calculate_pacf(dates %>% tail(-1), y),
      by = c("date", "lag")
    ) %>%
    rename(pacf_y = pacf) %>%
    relocate(date)
  
  return(df_corrxy)
}

# Cross-correlation between daily cases and deaths ----
calculate_cross_correlation <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  verbose = 1
){
  # Spatially clustered data
  df_correlation <- df_covid %>%
    filter(date >= initial_date, date <= final_date) %>%
    # Cases and deaths group by date (sum over subregions)
    group_by(region, subregion, date) %>%
    summarise(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Loop over regions
  df_ccf <- data.table()
  for(i in df_correlation %>% distinct(region) %>% pull()) {
    
    df_aux <- calculate_correlations(
      dates = df_correlation %>%
        filter(region == i) %>%
        pull(date),
      x = df_correlation %>%
        filter(region == i) %>%
        pull(cases),
      y = df_correlation %>%
        filter(region == i) %>%
        pull(deaths)
    ) %>%
      # Renaming of PACF variables
      rename(
        pacf_cases = pacf_x,
        pacf_deaths = pacf_y
      ) %>%
      # Union of region, subregion and date data
      left_join(
        df_correlation %>%
          filter(region == i) %>%
          select(region, subregion, date),
        by = "date"
      ) %>%
      relocate(c(region, subregion, date))
    
    df_ccf <- df_ccf %>% bind_rows(df_aux)
    
    # Function development
    if(verbose >= 1) {
      cat(
        paste0(
          "Cross-correlation and Partial autocorrelation region: ",
          i,
          "\n"
        )
      )
    }
  }
  
  return(df_ccf)
}

# Auxiliary function for estimate Power Spectral Density without dates ----
get_power_spectral_density <- function(x, p_norm = 2) {
  # Spectral density with fast fourier transform
  spec_density <- fft(x)
  
  # Power spectral density of a signal
  df_psd <- data.table(
    "lag" = seq(length(x)) - 1,
    "psd" = (Re(spec_density)^p_norm + Im(spec_density)^p_norm)^(1 / p_norm)
  ) %>%
    # By symmetry, we exclude the other half of data
    filter(lag < length(x) / 2)
  
  return(df_psd)
}

# Power Spectral Density (PSD) over cross-correlation time series ----
calculate_psd_cross_correlation <- function(
  df_cross_correlation,
  p_norm = 2,
  verbose = 1
) {
  # Loop over regions
  df_psd <- data.table()
  for(i in df_cross_correlation %>% distinct(region) %>% pull()) {
    
    df_psd <- df_psd %>%
      bind_rows(
        get_power_spectral_density(
          x = df_cross_correlation %>% filter(region == i) %>% pull(ccf),
          p_norm = p_norm
        ) %>%
          # Union of region, subregion and date data
          left_join(
            df_cross_correlation %>%
              filter(region == i) %>%
              select(region, subregion, date, lag),
            by = "lag"
          ) %>%
          relocate(c(region, subregion, date))
      )
    
    # Function development
    if(verbose >= 1) {
      cat(paste0("Power Spectral Density region: ", i, "\n"))
    }
  }
  
  # Estimation of daily frequency per region
  df_psd <- df_psd %>%
    # Estimation of daily frequency per region
    group_by(region, subregion) %>%
    mutate(
      frequency = c(0, diff(date)),
      cum_frequency = cumsum(c(0, diff(date)))
    ) %>%
    ungroup() %>%
    mutate(
      frequency = if_else(frequency != 0, 1 / frequency, 0),
      cum_frequency = if_else(cum_frequency != 0, 1 / cum_frequency, 0)
    ) %>%
    # Filtering non-NA frequencies
    filter(frequency != 0, cum_frequency != 0) %>%
    relocate(c(frequency, cum_frequency), .after = c(lag, lag)) %>%
    select(-c(date))
  
  return(df_psd)
}

# Minimum PACFs per region for moving average in overlapping ----
calculate_min_pacf <- function(
  df_cross_correlation,
  n_of_minima = 10
) {
  # Loop over regions
  df_min_pacf <- data.table()
  for(i in df_cross_correlation %>% distinct(region) %>% pull()) {
    df_min_pacf <- df_min_pacf %>%
      bind_rows(
        df_cross_correlation %>%
          # Selection of minima per region and subregion
          filter(region == i) %>%
          group_by(region, subregion) %>%
          # First and second derivative criterion for minimum (derivatives)
          mutate(
            # Days
            days = as.numeric(date - min(date, na.rm = TRUE)),
            # Derivatives
            pacf_cases_d1 = c(0, diff(pacf_cases)),
            pacf_cases_d2 = c(0, 0, diff(diff(pacf_cases))),
            pacf_deaths_d1 = c(0, diff(pacf_deaths)),
            pacf_deaths_d2 = c(0, 0, diff(diff(pacf_deaths)))
          ) %>%
          # First and second derivative criterion for minimum (uncertainty)
          mutate(
            pacf_cases_d1_sd =
              sd(pacf_cases_d1, na.rm = TRUE) * 3.0,
            pacf_cases_d2_sd =
              sd(pacf_cases_d2, na.rm = TRUE) * 3.0,
            pacf_deaths_d1_sd =
              sd(pacf_deaths_d1, na.rm = TRUE) * 3.0,
            pacf_deaths_d2_sd =
              sd(pacf_deaths_d2, na.rm = TRUE) * 3.0
          ) %>%
          # First and second derivative criterion for minimum (criterion)
          filter(
            # First derivative around zero +/- standard deviation
            abs(pacf_cases_d1) < pacf_cases_d1_sd,
            abs(pacf_deaths_d1) < pacf_deaths_d1_sd,
            # Second derivative > zero - standard deviation (lower bound)
            pacf_cases_d2 > -pacf_cases_d2_sd,
            pacf_deaths_d2 > -pacf_deaths_d2_sd,
            lag > 1
          ) %>%
          # Selection of minima (criterion of increasing PACF for first minimum)
          arrange(region, subregion, pacf_cases, pacf_deaths) %>%
          head(n_of_minima) %>%
          # Get simple moving average (SMA) days for overlapping time series
          mutate(minima = seq(n_of_minima)) %>%
          # Inform simple moving average days (centering data)
          mutate(ma_days = min(days, na.rm = TRUE)) %>%
          ungroup()
      )
  }
  
  return(df_min_pacf)
}

# Maximum frequencies per region for lag days in overlapping ----
calculate_max_frequencies <- function(
  df_psd,
  n_of_maxima = 10
) {
  # Loop over regions
  df_max_frequency <- data.table()
  for(i in df_psd %>% distinct(region) %>% pull()) {
    df_max_frequency <- df_max_frequency %>%
      bind_rows(
        df_psd %>%
          # Selection of maxima per region and subregion
          filter(region == i) %>%
          group_by(region, subregion) %>%
          # First and second derivative criterion for maximum (derivatives)
          mutate(
            # Days
            days = 1 / cum_frequency,
            # Derivatives
            psd_d1 = c(0, diff(psd)),
            psd_d2 = c(0, 0, diff(diff(psd)))
          ) %>%
          # First and second derivative criterion for maximum (uncertainty)
          mutate(
            psd_d1_sd = sd(psd_d1, na.rm = TRUE) / sqrt(length(psd_d1)),
            psd_d2_sd = sd(psd_d2, na.rm = TRUE) / sqrt(length(psd_d2))
          ) %>%
          # First and second derivative criterion for maximum (criterion)
          filter(
            # First derivative around zero +/- standard deviation
            abs(psd_d1) < psd_d1_sd,
            # Second derivative > zero - standard deviation (lower bound)
            psd_d2 > -psd_d2_sd,
            lag > 1
          ) %>%
          # Selection of maxima (criterion of decreasing PSD for first maximum)
          arrange(region, subregion, desc(psd)) %>%
          head(n_of_maxima) %>%
          # Get lag days for overlapping time series
          mutate(maxima = seq(n_of_maxima)) %>%
          # Inform the best lag days (centering data)
          mutate(lag_days = min(days, na.rm = TRUE)) %>%
          ungroup()
      )
  }
  
  return(df_max_frequency)
}

# Deployment of all correlation analysis functions ----
get_all_correlation_data <- function(
  df_covid,
  initial_date,
  final_date = Sys.Date(),
  p_norm = 2,
  n_of_minima = 10,
  n_of_maxima = 10,
  verbose = 1
) {
  # Calculate cross-correlation between cases and deaths per region
  df_cross_correlation <- calculate_cross_correlation(
    df_covid = df_covid,
    initial_date = initial_date,
    final_date = final_date,
    verbose = verbose
  )
  
  # Estimate Power Spectral density from cross-correlation time series
  df_psd <- calculate_psd_cross_correlation(
    df_cross_correlation = df_cross_correlation,
    p_norm = p_norm,
    verbose = verbose
  )
  
  # Estimate local minimums (minima) of Partial autocorrelation data
  df_min_pacf <- calculate_min_pacf(
    df_cross_correlation = df_cross_correlation,
    n_of_minima = n_of_minima
  )
  
  # Estimate local maximums (maxima) of Power Spectral Density data
  df_max_frequency <- calculate_max_frequencies(
    df_psd = df_psd,
    n_of_maxima = n_of_maxima
  )
  
  # Join Simple Moving Average (SMA) and Lag days to Spatially clustered data
  df_correlation <- df_covid %>%
    filter(date >= initial_date, date <= final_date) %>%
    # Cases and deaths group by date (sum over subregions)
    group_by(region, subregion, date) %>%
    summarise(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Union of SMA days estimate with overlapping data
    left_join(
      df_min_pacf %>% distinct(region, subregion, ma_days),
      by = c("region", "subregion")
    ) %>%
    # Union of lag days estimate with overlapping data
    left_join(
      df_max_frequency %>% distinct(region, subregion, lag_days),
      by = c("region", "subregion")
    )
  
  # Loop over region for make the moving average and Granger causality test
  df_temp <- data.table()
  df_granger_causality <- data.table()
  for(i in df_correlation %>% distinct(region) %>% pull()) {
    # Estimate SMA data
    df_temp <- df_temp %>%
      bind_rows(
        df_correlation %>%
          filter(region == i) %>%
          group_by(region, subregion) %>%
          mutate(
            # SMA cases
            ma_cases = forecast::ma(
              cases,
              df_correlation %>%
                filter(region == i) %>%
                pull(ma_days) %>%
                min(na.rm = TRUE)
            ),
            # SMA deaths
            ma_deaths = forecast::ma(
              deaths,
              df_correlation %>%
                filter(region == i) %>%
                pull(ma_days) %>%
                min(na.rm = TRUE)
            )
          ) %>%
          # Exclude negative values
          mutate(
            ma_cases = if_else(ma_cases >= 0, ma_cases, 0),
            ma_deaths = if_else(ma_deaths >= 0, ma_deaths, 0)
          ) %>%
          ungroup() %>%
          select(region, subregion, date, ma_cases, ma_deaths)
      )
    
    # Granger causality test between daily cases and deaths
    df_granger_causality <- df_granger_causality %>%
      bind_rows(
        lmtest::grangertest(
          cases ~ deaths,
          order = df_correlation %>%
            filter(region == i) %>%
            pull(ma_days) %>%
            min(na.rm = TRUE),
          data = df_correlation %>%
            filter(region == i)
        ) %>%
          rename_at(all_of("Pr(>F)"), function(x) {return("p_value")}) %>%
          mutate(
            alpha_0.001 = if_else(p_value <= 0.001, "causal", "non-causal"),
            alpha_0.01 = if_else(p_value <= 0.01, "causal", "non-causal"),
            alpha_0.05 = if_else(p_value <= 0.05, "causal", "non-causal"),
            alpha_0.1 = if_else(p_value <= 0.1, "causal", "non-causal"),
            region = df_correlation %>%
              filter(region == i) %>%
              distinct(region) %>%
              pull(),
            subregion = df_correlation %>%
              filter(region == i) %>%
              distinct(subregion) %>%
              pull()
          ) %>%
          relocate(c(region, subregion, p_value)) %>%
          select(-c("Res.Df", "Df", "F")) %>%
          tail(1)
      )
    
    # Function development
    if(verbose >= 1) {
      cat(paste0("Simple Moving Average and Granger Causality test: ", i, "\n"))
    }
  }
  
  # Merge SMA cases and deaths to Spatially clustered data
  df_correlation <- df_correlation %>%
    left_join(
      df_temp,
      by = c("region", "subregion", "date")
    )
  
  # Final data output
  list_df <- list(
    "spatially_clustered_data" = df_correlation,
    "cross_correlation" = df_cross_correlation,
    "psd_cross_correlation" = df_psd,
    "pacf_minimum" = df_min_pacf,
    "psd_maximum" = df_max_frequency,
    "granger_causality" = df_granger_causality
  )
  
  return(list_df)
}
