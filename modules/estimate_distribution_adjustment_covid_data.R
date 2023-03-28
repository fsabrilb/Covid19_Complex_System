# Libraries ----
library(VGAM)
library(dplyr)
library(purrr)
library(tidyr)
library(data.table)

# Auxiliary function for probability density function of Burr distribution ----
dburr <- function(x, scale, inequality, shape, log = FALSE) {
  # Burr distribution as a Pareto IV distribution
  pdf_burr <- dparetoIV(
    x = x,
    location = 0,
    scale = scale,
    inequality = 1 / inequality,
    shape = shape,
    log = log
  )
  
  return(pdf_burr)
}

# Auxiliary function for log-likelihood estimation ----
estimate_log_likelihood <- function(x, parameters, distribution_name) {
  if(distribution_name == "burr") {
    if(length(parameters) != 3) {
      cat(
        paste0(
          "The number of parameters (scale, inequality, shape) must be 3 ",
          "but have been given: ",
          length(parameters),
          "\n"
        )
      )
      log_likelihood <- 0
    } else {
      log_likelihood <- dburr(
        x = x,
        scale = parameters[1],
        inequality = parameters[2],
        shape = parameters[3],
        log = TRUE
      )
    }
  }
  if(distribution_name == "gamma") {
    if(length(parameters) != 2) {
      cat(
        paste0(
          "The number of parameters (shape, rate) must be 2 ",
          "but have been given: ",
          length(parameters),
          "\n"
        )
      )
      log_likelihood <- 0
    } else {
      log_likelihood <- dgamma(
        x = x,
        shape = parameters[1],
        rate = parameters[2],
        log = TRUE
      )
    }
  }
  if(distribution_name == "normal") {
    if(length(parameters) != 2) {
      cat(
        paste0(
          "The number of parameters (mean, sd) must be 2 ",
          "but have been given: ",
          length(parameters),
          "\n"
        )
      )
      log_likelihood <- 0
    } else {
      log_likelihood <- dnorm(
        x = x,
        mean = parameters[1],
        sd = parameters[2],
        log = TRUE
      )
    }
  }
  if(distribution_name == "lognormal") {
    if(length(parameters) != 2) {
      cat(
        paste0(
          "The number of parameters (meanlog, sdlog) must be 2 ",
          "but have been given: ",
          length(parameters),
          "\n"
        )
      )
      log_likelihood <- 0
    } else {
      log_likelihood <- dlnorm(
        x = x,
        meanlog = parameters[1],
        sdlog = parameters[2],
        log = TRUE
      )
    }
  }
  
  log_likelihood <- sum(log_likelihood, na.rm = TRUE)
  return(log_likelihood)
}

# Optimize log-likelihood function for parameters of adjustment ----
optimize_log_likelihood <- function(
  x,
  distribution_name,
  tolerance_optimization = .Machine %>% pluck("double.eps") * 1e6
) {
  # Likelihood estimation
  likelihood_function <- function(parameters) {
    log_likelihood <- estimate_log_likelihood(
      x = x,
      parameters = parameters,
      distribution_name = distribution_name
    )
    return(-log_likelihood)
  }
  
  # Auxiliary function for guess value in optimization
  guess_value <- function(x, distribution_name) {
    if(distribution_name == "burr"){
      guess <- c(
        var(x, na.rm = TRUE), # scale > 0
        1, # inequality > 0
        var(x, na.rm = TRUE) # shape > 0
      )
    }
    if(distribution_name == "gamma"){
      guess <- c(
        mean(x, na.rm = TRUE)^2 / var(x, na.rm = TRUE), # shape > 0
        var(x, na.rm = TRUE) / mean(x, na.rm = TRUE) # rate > 0
      )
    }
    if(distribution_name == "normal"){
      guess <- c(
        mean(x, na.rm = TRUE), # mean
        sd(x, na.rm = TRUE) # sd > 0
      )
    }
    if(distribution_name == "lognormal"){
      guess <- c(
        0.5 * log(
          mean(x, na.rm = TRUE)^4 /
            (var(x, na.rm = TRUE) + mean(x, na.rm = TRUE)^2)
        ), # meanlog
        sqrt(log(1 + var(x, na.rm = TRUE) / mean(x, na.rm = TRUE)^2)) # sdlog>0
      )
    }
    return(guess)
  }
  
  # Optimal parameters of maximum likelihood estimation
  optimal_parameters <- optim(
    # Guess Value
    par = guess_value(x, distribution_name),
    # Function for Optimization (Parameter input as vector)
    fn = likelihood_function,
    # Options for Optimization
    control = list(reltol = tolerance_optimization)
  )
  
  # Auxiliary function for parameters name in optimization
  parameters_name <- function(distribution_name) {
    if(distribution_name == "burr"){
      par_name <- c("scale", "inequality", "shape")
    }
    if(distribution_name == "gamma"){
      par_name <- c("shape", "rate")
    }
    if(distribution_name == "normal"){
      par_name <- c("mean", "sd")
    }
    if(distribution_name == "lognormal"){
      par_name <- c("meanlog", "sdlog")
    }
    return(par_name)
  }
  
  # Resume of optimization on adjusted parameters
  df_adjustment <- bind_rows(
    # Parameters name
    data.table(
      variable_name = parameters_name(distribution_name),
      value = optimal_parameters %>% pluck("par")
    ),
    # Log-likelihood
    data.table(
      variable_name = "log-likelihood",
      value = optimal_parameters %>% pluck("value")
    ),
    # Estimator of prediction (Akaike information criterion)
    data.table(
      variable_name = "aic",
      value = 2 * optimal_parameters %>% pluck("par") %>% length() +
        2 * optimal_parameters %>% pluck("value")
    ),
    # Estimator of prediction (Bayesian information criterion)
    data.table(
      variable_name = "bic",
      value = optimal_parameters %>% pluck("par") %>% length() * length(x) +
        2 * optimal_parameters %>% pluck("value")
    )
  ) %>% mutate(distribution_name = distribution_name)
  
  return(df_adjustment)
}

# Optimize log-likelihood function for Burr parameters of adjustment ----
optimize_log_likelihood_burr <- function(x, distribution_name) {
  if(distribution_name == "burr") {
    fit <- vglm(x ~ 1, paretoIV, trace = FALSE)
    df_adjustment <- Coef(fit) %>%
      t() %>%
      data.table() %>%
      cbind(data.table(aic = AICvlm(fit), bic = BICvlm(fit))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      mutate(
        distribution_name = distribution_name,
        value = if_else(
          variable_name == "inequality",
          if_else(value == 0, 0, 1/value),
          value
        )
      )
  }
  if(distribution_name == "gamma") {
    fit <- vglm(x ~ 1, gamma2, trace = FALSE)
    df_adjustment <- Coef(fit) %>%
      t() %>%
      data.table() %>%
      cbind(data.table(aic = AICvlm(fit), bic = BICvlm(fit))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      mutate(
        distribution_name = distribution_name,
        variable_name = if_else(
          variable_name == "mu",
          "scale",
          variable_name
        )
      )
  }
  if(distribution_name == "lognormal") {
    fit <- vglm(x ~ 1, lognormal, trace = TRUE)
    df_adjustment <- Coef(fit) %>%
      t() %>%
      data.table() %>%
      cbind(data.table(aic = AICvlm(fit), bic = BICvlm(fit))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable_name",
        values_to = "value"
      ) %>%
      mutate(distribution_name = distribution_name)
  }
  if(distribution_name == "weibull") {
    fit <- vglm(x ~ 1, weibullR, trace = FALSE)
    df_adjustment <- Coef(fit) %>%
      t() %>%
      data.table() %>%
      cbind(data.table(aic = AICvlm(fit), bic = BICvlm(fit))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "variable_name",
        values_to = "value"
      )
  }
  
  df_adjustment <- df_adjustment %>% data.table()
  
  return(df_adjustment)
}

# Adjust cumulative daily cases and deaths over subregion on specific date ----
fit_spatial_distribution <- function(
  df_covid,
  filter_day,
  tolerance_optimization = .Machine %>% pluck("double.eps") * 1e6,
  df_matrix_selection,
  verbose = 1
) {
  df_fit <- df_covid %>%
    # Union of selection matrix for adjustment per date
    left_join(
      df_matrix_selection,
      by = c("region", "date")
    ) %>%
    # Exclude non-zero data and select day for adjustment
    filter(
      cum_cases != 0,
      cum_deaths != 0,
      date == filter_day,
      value == "TRUE"
    )
  
  # Loop over regions
  df_spatial_adjustment <- data.table()
  for(i in df_fit %>% distinct(region) %>% pull()) {
    df_spatial_adjustment <- df_spatial_adjustment %>%
      bind_rows(
        bind_rows(
          # Adjustment of Cumulative Cases to Burr distribution
          optimize_log_likelihood_burr(
            x = df_fit %>%
              filter(region == i) %>%
              mutate(cum_cases = cum_cases + mean(cum_cases, na.rm = TRUE)) %>%
              pull(cum_cases),
            distribution_name = "burr"
          ) %>% mutate(information = "cum_cases"),
          # Adjustment of Cumulative Cases to Gamma distribution
          optimize_log_likelihood_burr(
            x = df_fit %>% filter(region == i) %>% pull(cum_cases),
            distribution_name = "gamma"
          ) %>% mutate(information = "cum_cases"),
          # Adjustment of Cumulative Cases to Log-normal distribution
          optimize_log_likelihood(
            x = df_fit %>% filter(region == i) %>% pull(cum_cases),
            distribution_name = "lognormal",
            tolerance_optimization = tolerance_optimization
          ) %>% mutate(information = "cum_cases"),
          # Adjustment of Cumulative Cases to Weibull distribution
          optimize_log_likelihood_burr(
            x = df_fit %>% filter(region == i) %>% pull(cum_cases),
            distribution_name = "weibull"
          ) %>% mutate(information = "cum_cases"),
          # Adjustment of Cumulative Deaths to Burr distribution
          optimize_log_likelihood_burr(
            x = df_fit %>%
              filter(region == i) %>%
              mutate(
                cum_deaths = cum_deaths + mean(cum_deaths, na.rm = TRUE)
              ) %>%
              pull(cum_deaths),
            distribution_name = "burr"
          ) %>% mutate(information = "cum_deaths"),
          # Adjustment of Cumulative Deaths to Gamma distribution
          optimize_log_likelihood_burr(
            x = df_fit %>% filter(region == i) %>% pull(cum_deaths),
            distribution_name = "gamma"
          ) %>% mutate(information = "cum_deaths"),
          # Adjustment of Cumulative Deaths to Log-normal distribution
          optimize_log_likelihood(
            x = df_fit %>% filter(region == i) %>% pull(cum_deaths),
            distribution_name = "lognormal",
            tolerance_optimization = tolerance_optimization
          ) %>% mutate(information = "cum_deaths")
        ) %>%
          mutate(
            region = i,
            subregion = df_fit %>%
              filter(region == i) %>%
              distinct(subregion) %>%
              pull(),
            date = filter_day
          )
      )
    
    # Function development
    if(verbose >= 1) {
      cat(
        paste0("Adjustment region: ", i, " on date ", as.Date(filter_day), "\n")
      )
    }
  }
  
  # Ordering columns of output data frame
  df_spatial_adjustment <- df_spatial_adjustment %>%
    relocate(c(region, subregion, date)) %>%
    arrange(region, subregion, date)
  
  return(df_spatial_adjustment)
}

# Adjust cumulative daily cases and deaths over subregion on multiple dates ----
fit_spatial_evolution <- function(
  df_covid,
  days_vector,
  tolerance_optimization = .Machine %>% pluck("double.eps") * 1e6,
  df_matrix_selection,
  verbose = 1,
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04"
) {
  if(saved_all_data == FALSE) {
    # Loop over dates
    df_adjustment <- data.table()
    for(i  in days_vector) {
      # Function development
      if(verbose >= 1) {
        cat(
          paste0(
            "------------------------ Spatial evolution on date: ",
            as.Date(i),
            " ------------------------\n"
          )
        )
      }
      
      # Local evolution on spatial distribution
      df_adjustment <- df_adjustment %>%
        bind_rows(
          fit_spatial_distribution(
            df_covid = df_covid,
            filter_day = i,
            tolerance_optimization = tolerance_optimization,
            df_matrix_selection = df_matrix_selection,
            verbose = verbose
          )
        )
    }
    
    df_adjustment <- df_adjustment %>%
      mutate(
        date = as.Date(date),
        value = as.numeric(value)
      ) %>%
      pivot_wider(names_from = variable_name, values_from = value) %>%
      arrange(region, subregion, date, distribution_name, information) %>%
      data.table()
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_adjustment,
      paste0(
        input_path_processed,
        "/df_spatial_adjustment_covid_data_",
        gsub("-", "", input_date),
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
  } else {
    # Load all Covid data from processed data for not reprocess the information
    df_adjustment <- fread(
      paste0(
        input_path_processed,
        "/df_spatial_adjustment_covid_data_",
        gsub("-", "", input_date),
        ".csv"
      ),
      encoding = "UTF-8"
    ) %>%
      mutate(date = as.Date(date))
  }
  
  return(df_adjustment)
}

# Estimate coupling of lognormal parameters between cases and deaths ----
estimate_coupling <- function(
  df_spatial_adjustment,
  output_path = "./output_files",
  save_data = FALSE,
  input_date = "2022-12-04"
) {
  df_parameters <- data.table()
  
  # Meanlog
  for(i in df_spatial_adjustment %>% distinct(region) %>% pull()) {
    # Linear regression between meanlog parameters
    list_aux <- lm(
      df_spatial_adjustment %>%
        filter(
          region == i,
          information == "cum_deaths",
          distribution_name == "lognormal"
        ) %>%
        pull(meanlog) ~
        df_spatial_adjustment %>%
        filter(
          region == i,
          information == "cum_cases",
          distribution_name == "lognormal"
        ) %>%
        pull(meanlog)
    ) %>%
      summary()
    
    # Coefficients of regression
    df_aux <- list_aux %>%
      pluck("coefficients") %>%
      t() %>%
      data.table() %>%
      rename_all(function(x) {c("coefficient", "slope")}) %>%
      mutate(
        statistic = c("Estimate", "Standard error", "t value", "Pr(>|t|)")
      ) %>%
      mutate(region = i, information = "meanlog") %>%
      relocate(region, statistic)
    
    # Coefficient of determination R2
    rsquared <- list_aux %>% pluck("r.squared")
    
    df_parameters <- df_parameters %>%
      bind_rows(
        df_aux,
        data.table(
          region = i,
          statistic = "R2",
          coefficient = rsquared,
          slope = rsquared,
          information = "meanlog"
        )
      )
  }
  
  # Sdlog
  for(i in df_spatial_adjustment %>% distinct(region) %>% pull()) {
    # Linear regression between sdlog parameters
    list_aux <- lm(
      df_spatial_adjustment %>%
        filter(
          region == i,
          information == "cum_deaths",
          distribution_name == "lognormal"
        ) %>%
        pull(sdlog) ~
        df_spatial_adjustment %>%
        filter(
          region == i,
          information == "cum_cases",
          distribution_name == "lognormal"
        ) %>%
        pull(sdlog)
    ) %>%
      summary()
    
    # Coefficients of regression
    df_aux <- list_aux %>%
      pluck("coefficients") %>%
      t() %>%
      data.table() %>%
      rename_all(function(x) {c("coefficient", "slope")}) %>%
      mutate(
        statistic = c("Estimate", "Standard error", "t value", "Pr(>|t|)")
      ) %>%
      mutate(region = i, information = "sdlog") %>%
      relocate(region, statistic)
    
    # Coefficient of determination R2
    rsquared <- list_aux %>% pluck("r.squared")
    
    df_parameters <- df_parameters %>%
      bind_rows(
        df_aux,
        data.table(
          region = i,
          statistic = "R2",
          coefficient = rsquared,
          slope = rsquared,
          information = "sdlog"
        )
      )
  }
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_data == TRUE) {
    write_csv(
      df_parameters,
      paste0(output_folder, "/df_spatially_adjustment_parameters_coupling.csv")
    )
  }
  
  return(df_parameters)
}
