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
  line_size = 1.1,
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
    df_graph <- df_spatial_adjustment %>% filter(information == cases_or_deaths)
  } else {
    df_graph <- df_spatial_adjustment %>%
      filter(distribution_name == distribution) %>%
      group_by(region, subregion, information) %>%
      mutate(
        scale = scale / max(scale, na.rm = TRUE),
        inequality = inequality / max(inequality, na.rm = TRUE),
        shape = shape / max(shape, na.rm = TRUE),
        rate = rate / max(rate, na.rm = TRUE),
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
        geom_line(linewidth = line_size)
    } else {
      graph <- df_graph %>% ggplot() 
      for(i in names_parameters(df_graph)) {
        graph <- graph +
          # Plot data
          aes(
            x = df_graph %>% pull(date),
            y = df_graph %>% pull(i),
            colour = paste0(region, "-", information, " - ", i)
          ) +
          geom_line(linewidth = line_size)
      }
    }
    return(graph)
  }
  
  # Auxiliary function for title selection in y coordinate
  title_y <- function(x, distribution) {
    x <- case_when(
      x == "aic" ~ "Akaike information criterion",
      x == "bic" ~ "Bayesian information criterion",
      TRUE ~ paste0(
        "Parameters ", stri_trans_totitle(distribution), " distribution"
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
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
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
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_adjustment %>%
      filter(
        information == "cum_cases",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(aic) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_adjustment %>%
      filter(
        information == "cum_cases",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(aic) %>%
      max(na.rm = TRUE)
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
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_adjustment %>%
      filter(
        information == "cum_deaths",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(aic) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_adjustment %>%
      filter(
        information == "cum_deaths",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(aic) %>%
      max(na.rm = TRUE)
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
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_adjustment %>%
      filter(
        information == "cum_cases",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(bic) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_adjustment %>%
      filter(
        information == "cum_cases",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(bic) %>%
      max(na.rm = TRUE)
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
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_adjustment %>%
      filter(
        information == "cum_deaths",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(bic) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_adjustment %>%
      filter(
        information == "cum_deaths",
        date >= initial_date,
        date <= final_date
      ) %>%
      pull(bic) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Parameters Burr Distribution
  plot_burr <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment %>%
      filter(information == "cum_cases"),
    variable_name = "",
    cases_or_deaths = "",
    distribution = "burr",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = 0,
    final_y = 1
  )
  
  # Plot Parameters Log-normal Distribution
  plot_lognormal <- plot_adjustment_distribution(
    df_spatial_adjustment = df_spatial_adjustment %>%
      filter(information == "cum_deaths"),
    variable_name = "",
    cases_or_deaths = "",
    distribution = "lognormal",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = 0,
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
    
    # Parameters of Burr distribution evolution
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_distribution_burr.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_burr)
    dev.off()
    
    # Parameters of Log-normal distribution evolution
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_distribution_lognormal.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_lognormal)
    dev.off()
    
    # Adjusted distribution data obtained from estimator prediction analysis
    write_csv(
      df_spatial_adjustment,
      paste0(output_folder, "/df_spatially_adjustment_evolution.csv")
    )
  }
  
  return(0)
}
