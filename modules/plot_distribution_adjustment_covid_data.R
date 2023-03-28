# Libraries ----
library(Cairo)
library(dplyr)
library(readr)
library(ggplot2)
library(stringi)
library(data.table)

# Auxiliary function for spatial evolution parameters evolution plotting ----
plot_adjustment_distribution <- function(
  df_spatial_adjustment,
  variable_name,
  cases_or_deaths = "cum_cases",
  distribution = "burr",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 0.6,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  initial_y = 0,
  final_y = 1
) {
  # Auxiliary function for remove columns without information
  not_all_na <- function(x) {any(!is.na(x))}
  
  # Auxiliary function for remove columns without information
  not_all_nan <- function(x) {any(!is.nan(x))}
  
  # Normalization of data in every region and every variable
  if(variable_name == "aic" | variable_name == "bic") {
    df_graph <- df_spatial_adjustment %>%
      filter(information == cases_or_deaths) %>%
      # Correct some anomalous data
      mutate(
        aic = if_else(
          region == "Brazil" &
            date == as.Date("2020-08-15") &
            distribution_name == "lognormal",
          678.9276,
          aic
        ),
        bic = if_else(
          region == "Brazil" &
            date == as.Date("2020-08-15") &
            distribution_name == "lognormal",
          728.9276,
          bic
        )
      ) %>%
      # Normalize variables
      group_by(region, subregion, information) %>%
      mutate(
        aic = aic / max(aic, na.rm = TRUE),
        bic = aic / max(aic, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(where(not_all_na)) %>%
      select(where(not_all_nan))
  } else {
    df_graph <- df_spatial_adjustment %>%
      filter(distribution_name == distribution) %>%
      # Correct some anomalous data
      mutate(
        sdlog = if_else(
          region == "Brazil" &
            date == as.Date("2020-08-15") &
            information == "cum_cases",
          0.706544272,
          sdlog
        )
      ) %>%
      # Normalize variables
      group_by(region, subregion, information) %>%
      mutate(
        scale = scale / max(scale, na.rm = TRUE),
        inequality = inequality / max(inequality, na.rm = TRUE),
        shape = shape / max(shape, na.rm = TRUE),
        meanlog = meanlog / max(meanlog, na.rm = TRUE),
        sdlog = sdlog / max(sdlog, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(where(not_all_na)) %>%
      select(where(not_all_nan))
  }
  
  # Auxiliary function for select non-null parameters
  names_parameters <- function(df) {
    exclude_variables <- c(
      "region",
      "subregion",
      "date",
      "distribution_name",
      "information",
      "aic",
      "bic",
      "log-likelihood"
    )
    
    x <- df %>% names() %>% .[!. %in% exclude_variables]
    return(x)
  }
  
  # Auxiliary function for group data in graph output (plotting data)
  colour_group <- function(graph, x) {
    if(x == "aic" | x == "bic") {
      graph <- df_graph %>%
        ggplot() +
        # Plot data
        aes(
          x = df_graph %>% pull(date),
          y = df_graph %>% pull(variable_name),
          colour = paste0(region, "-", distribution_name)
        ) +
        geom_line(linewidth = line_size) +
        aes(
          x = df_graph %>% pull(date),
          y = df_graph %>% pull(variable_name),
          colour = paste0(region, "-", distribution_name)
        ) +
        geom_point(shape = 16, size = 2.4)
    } else {
      graph <- ggplot() +
        # Plot data
        aes(
          x = df_graph %>% pull(date),
          y = df_graph %>% pull(variable_name),
          colour = paste0(
            df_graph %>% pull(region),
            " - ",
            df_graph %>% pull(information),
            " - ",
            variable_name
          )
        ) +
        geom_line(linewidth = line_size) +
        aes(
          x = df_graph %>% pull(date),
          y = df_graph %>% pull(variable_name),
          colour = paste0(
            df_graph %>% pull(region),
            " - ",
            df_graph %>% pull(information),
            " - ",
            variable_name
          )
        ) +
        geom_point(shape = 16, size = 2.4)
    }
    return(graph)
  }
  
  # Auxiliary function for title selection in y coordinate
  title_y <- function(x, distribution) {
    x <- case_when(
      x == "aic" ~
        paste0(
          "Normalized Akaike information criterion - ",
          stri_trans_tolower(gsub("cum_", "cumulative ", cases_or_deaths))
        ),
      x == "bic" ~
        paste0(
          "Normalized Bayesian information criterion - ",
          stri_trans_tolower(gsub("cum_", "cumulative ", cases_or_deaths))
        ),
      TRUE ~ paste0(
        "Normalized ",
        stri_trans_tolower(variable_name),
        " ",
        stri_trans_totitle(distribution),
        " distribution - ",
        stri_trans_tolower(gsub("cum_", "cumulative ", cases_or_deaths))
      )
    )
    return(x)
  }
  
  # Graph structure (Axis and labels information)
  graph <- colour_group(graph, variable_name) +
    # Labels
    labs(
      x = "Date",
      y = title_y(variable_name, distribution)
    ) +
    # X- axis
    scale_x_date(
      date_breaks = date_breaks, 
      date_minor_breaks = date_minor_breaks,
      sec.axis = dup_axis()
    ) +
    # Y - axis
    scale_y_continuous(n.breaks = n_y_breaks) +
    # Limits
    coord_cartesian(
      xlim = c(as.Date(initial_date), as.Date(final_date)),
      ylim = c(initial_y, final_y)
    )
  
  # Graph structure (Data, theme and legend)
  graph <- graph +
    # Theme
    cowplot::theme_half_open(font_size) +
    theme(
      # Legend
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(axes_relative_size)),
      # Axis ticks
      axis.title.x.bottom = element_text(size = rel(axes_title_relative_size)),
      axis.title.x.top = element_blank(),
      axis.title.y.left = element_text(size = rel(axes_title_relative_size)),
      axis.title.y.right = element_blank(),
      axis.ticks = element_line(colour = "black", size = 1.05),
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text.x.top = element_blank(),
      axis.text.y.right = element_text(size = rel(axes_relative_size)),
      axis.text.x.bottom = element_text(size = rel(axes_relative_size)),
      axis.text.y.left = element_text(size = rel(axes_relative_size)),
      # Panel
      panel.border = element_rect(colour = "black", fill = NA, size = 1.15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent")
    ) +
    guides(colour = guide_legend(ncol = legend_cols))
  
  return(graph)
}

# Graph of spatial evolution of spatial parameters for cases and deaths ----
plot_spatial_evolution <- function(
  df_spatial_adjustment,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  line_size = 0.6,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  drop_days = 30,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
) {
  # Plot Akaike information criterion (Cases)
  plot_aic_cases <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment,
    variable_name = "aic",
    cases_or_deaths = "cum_cases",
    distribution = "",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 8,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.3,
    final_y = 1
  )
  
  # Plot Akaike information criterion (Deaths)
  plot_aic_deaths <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment,
    variable_name = "aic",
    cases_or_deaths = "cum_deaths",
    distribution = "",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 8,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.25,
    final_y = 1
  )
  
  # Plot Bayesian information criterion (Cases)
  plot_bic_cases <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment,
    variable_name = "bic",
    cases_or_deaths = "cum_cases",
    distribution = "",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 8,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.3,
    final_y = 1
  )
  
  # Plot Bayesian information criterion (Deaths)
  plot_bic_deaths <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment,
    variable_name = "bic",
    cases_or_deaths = "cum_deaths",
    distribution = "",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 8,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.25,
    final_y = 1
  )
  
  # Plot Parameters Log-normal Distribution (Meanlog cases)
  plot_lognormal_cases_1 <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment %>%
      filter(information == "cum_cases"),
    variable_name = "meanlog",
    cases_or_deaths = "cum_cases",
    distribution = "lognormal",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 5,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.45,
    final_y = 1
  )
  
  # Plot Parameters Log-normal Distribution (Sdlog cases)
  plot_lognormal_cases_2 <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment %>%
      filter(information == "cum_cases"),
    variable_name = "sdlog",
    cases_or_deaths = "cum_cases",
    distribution = "lognormal",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 5,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.5,
    final_y = 1
  )
  
  # Plot Parameters Log-normal Distribution (Meanlog deaths)
  plot_lognormal_deaths_1 <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment %>%
      filter(information == "cum_deaths"),
    variable_name = "meanlog",
    cases_or_deaths = "cum_deaths",
    distribution = "lognormal",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 5,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.3,
    final_y = 1
  )
  
  # Plot Parameters Log-normal Distribution (Sdlog deaths)
  plot_lognormal_deaths_2 <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment %>%
      filter(information == "cum_deaths"),
    variable_name = "sdlog",
    cases_or_deaths = "cum_deaths",
    distribution = "lognormal",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = 5,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + drop_days,
    final_date = as.Date(final_date) - drop_days,
    initial_y = 0.6,
    final_y = 1
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Akaike information criterion (Cases)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_aic_cases.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_aic_cases)
    dev.off()
    
    # Akaike information criterion (Deaths)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_aic_deaths.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_aic_deaths)
    dev.off()
    
    # Bayesian information criterion (Cases)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_bic_cases.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_bic_cases)
    dev.off()
    
    # Bayesian information criterion (Deaths)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_bic_deaths.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_bic_deaths)
    dev.off()
    
    # Parameters of Log-normal distribution evolution (Meanlog cases)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_distribution_lognormal_cases_meanlog.png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_lognormal_cases_1)
    dev.off()
    
    # Parameters of Log-normal distribution evolution (Sdlog cases)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_distribution_lognormal_cases_sdlog.png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_lognormal_cases_2)
    dev.off()
    
    # Parameters of Log-normal distribution evolution (Meanlog deaths)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_distribution_lognormal_deaths_meanlog.png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_lognormal_deaths_1)
    dev.off()
    
    # Parameters of Log-normal distribution evolution (Sdlog deaths)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_distribution_lognormal_deaths_sdlog.png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_lognormal_deaths_2)
    dev.off()
    
    # Adjusted distribution data obtained from estimator prediction analysis
    write_csv(
      df_spatial_adjustment,
      paste0(output_folder, "/df_spatially_adjustment_evolution.csv")
    )
  }
  
  return(0)
}
