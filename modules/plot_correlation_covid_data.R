# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringi)
library(data.table)

# Auxiliary function for Cross-correlation, PACF and PSD plotting ----
plot_standard_correlation <- function(
  list_correlation,
  variable_name,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_x_breaks = 20,
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  initial_x = 0,
  final_x = 60,
  initial_y = -1,
  final_y = 1
) {
  # Normalization to maximum value in every region
  if(variable_name == "ccf") {
    df_graph <- list_correlation %>% pluck("cross_correlation")
  }
  if(variable_name == "psd") {
    df_graph <- list_correlation %>%
      pluck("psd_cross_correlation") %>%
      group_by(region, subregion) %>%
      mutate(max_psd = max(psd, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(psd = psd / max_psd)
  }
  if(variable_name == "ma_cases" | variable_name == "ma_deaths") {
    df_graph <- list_correlation %>%
      pluck("spatially_clustered_data") %>%
      group_by(region, subregion) %>%
      mutate(
        max_cases = max(cases, na.rm = TRUE),
        max_deaths = max(deaths, na.rm = TRUE),
        max_ma_cases = max(ma_cases, na.rm = TRUE),
        max_ma_deaths = max(ma_deaths, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        cases = cases / max_cases,
        deaths = deaths / max_deaths,
        ma_cases = ma_cases / max_cases,
        ma_deaths = ma_deaths / max_deaths
      )
  }
  
  # Auxiliary function for variable selection in x coordinate
  var_x <- function(x) {
    x <- case_when(
      x == "ccf" ~ "lag",
      x == "psd" ~ "lag",
      x == "ma_cases" ~ "date",
      TRUE ~ "date"
    )
    return(x)
  }
  
  # Auxiliary function for title selection in x coordinate
  title_x <- function(x) {
    x <- stri_trans_totitle(var_x(x))
    return(x)
  }
  
  # Auxiliary function for title selection in y coordinate
  title_y <- function(x) {
    x <- case_when(
      x == "ccf" ~ "Cross-correlation",
      x == "psd" ~ "Normalized to maximum PSD",
      x == "ma_cases" ~ "Normalized cases to maximum",
      TRUE ~ "Normalized deaths to maximum"
    )
    return(x)
  }
  
  # Auxiliary function for x-axis
  axis_graph <- function(graph, x) {
    if(x == "ma_cases" | x == "ma_deaths") {
      graph <- graph +
        # X- axis
        scale_x_date(
          date_breaks = date_breaks, 
          date_minor_breaks = date_minor_breaks,
          sec.axis = dup_axis()
        ) +
        # Y - axis
        scale_y_continuous(n.breaks = n_y_breaks)
    } else {
      graph <- graph +
        # X - axis
        scale_x_continuous(
          n.breaks = n_x_breaks,
          sec.axis = dup_axis()
        ) +
        # Y - axis
        scale_y_continuous(n.breaks = n_y_breaks)
    }
    return(graph)
  }
  
  # Auxiliary function for x-limits
  limits_graph <- function(graph, x) {
    if(x == "ma_cases" | x == "ma_deaths") {
      graph <- graph +
        coord_cartesian(
          xlim = c(as.Date(initial_date), as.Date(final_date)),
          ylim = c(initial_y, final_y)
        )
    } else {
      graph <- graph +
        coord_cartesian(
          xlim = c(initial_x, final_x),
          ylim = c(initial_y, final_y)
        )
    }
    return(graph)
  }
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data
    aes(
      x = df_graph %>% pull(var_x(variable_name)),
      y = df_graph %>% pull(variable_name),
      colour = region
    ) +
    geom_line(linewidth = line_size) +
    # Labels
    labs(
      x = title_x(variable_name),
      y = title_y(variable_name)
    )
  
  # Graph structure (Axes, limits, theme and legend)
  graph <- limits_graph(
    axis_graph(
      graph,
      variable_name
    ),
    variable_name
  ) +
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

# Graph of Cross-correlation, PACF, PSD for cases and deaths ----
plot_correlation_data <- function(
  list_correlation,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_x_breaks = 20,
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  initial_x = 0,
  final_x = 60,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
) {
  # Plot Cross-correlation
  plot_ccf <- plot_standard_correlation(
    list_correlation = list_correlation,
    variable_name = "ccf",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_x_breaks = n_x_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_x = initial_x,
    final_x = final_x,
    initial_y = list_correlation %>%
      pluck("cross_correlation") %>%
      filter(lag >= initial_x, lag <= final_x) %>%
      pull(ccf) %>%
      min(na.rm = TRUE),
    final_y = list_correlation %>%
      pluck("cross_correlation") %>%
      filter(lag >= initial_x, lag <= final_x) %>%
      pull(ccf) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Power Spectral Density
  plot_psd <- plot_standard_correlation(
    list_correlation = list_correlation,
    variable_name = "psd",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_x_breaks = n_x_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_x = initial_x,
    final_x = final_x,
    initial_y = 0,
    final_y = 1
  )
  
  # Plot SMA cases
  plot_sma_cases <- plot_standard_correlation(
    list_correlation = list_correlation,
    variable_name = "ma_cases",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_x_breaks = n_x_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_x = initial_x,
    final_x = final_x,
    initial_y = 0,
    final_y = 1
  )
  
  # Plot SMA deaths
  plot_sma_deaths <- plot_standard_correlation(
    list_correlation = list_correlation,
    variable_name = "ma_deaths",
    font_size = font_size,
    axes_title_relative_size = axes_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_x_breaks = n_x_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_x = initial_x,
    final_x = final_x,
    initial_y = 0,
    final_y = 1
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Cross-correlation
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_cross_correlation.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_ccf)
    dev.off()
    
    # Power Spectral Density
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_power_spectral_density.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_psd)
    dev.off()
    
    # SMA cases
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_sma_cases.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_sma_cases)
    dev.off()
    
    # SMA deaths
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_sma_deaths.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_sma_deaths)
    dev.off()
    
    # Moving average data obtained from correlation analysis
    write_csv(
      list_correlation %>% pluck("spatially_clustered_data"),
      paste0(output_folder, "/df_spatially_clustered_correlation.csv")
    )
    
    # Cross-Correlation, PACF and PSD obtained from correlation analysis
    write_csv(
      list_correlation %>%
        pluck("cross_correlation") %>%
        left_join(
          list_correlation %>% pluck("psd_cross_correlation"),
          by = c("region", "subregion", "lag")
        ),
      paste0(output_folder, "/df_correlation_measures.csv")
    )
    
    # Minimum partial autocorrelation obtained from PACF
    write_csv(
      list_correlation %>% pluck("pacf_minimum"),
      paste0(output_folder, "/df_pacf_minimum.csv")
    )
    
    # Maximum frequencies obtained from power spectral density
    write_csv(
      list_correlation %>% pluck("psd_maximum"),
      paste0(output_folder, "/df_psd_maximum.csv")
    )
    
    # Granger causality test obtained from correlation analysis
    write_csv(
      list_correlation %>% pluck("granger_causality"),
      paste0(output_folder, "/df_granger_causality_correlation.csv")
    )
  }
  
  return(0)
}

# Graph of overlapping in cases and deaths for every region ----
plot_overlapping <- function(
  list_correlation,
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
  final_y = 1,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400,
  verbose = 1
) {
  df_graph <- list_correlation %>%
    pluck("spatially_clustered_data") %>%
    group_by(region, subregion) %>%
    mutate(
      max_cases = max(cases, na.rm = TRUE),
      max_deaths = max(deaths, na.rm = TRUE),
      max_ma_cases = max(ma_cases, na.rm = TRUE),
      max_ma_deaths = max(ma_deaths, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      cases = cases / max_cases,
      deaths = deaths / max_deaths,
      ma_cases = ma_cases / max_cases,
      ma_deaths = ma_deaths / max_deaths,
      date_deaths = date - lag_days
    )
  
  # Loop over regions
  for(i in df_graph %>% distinct(region) %>% pull()) {
    # Graph structure (Data information)
    graph <- df_graph %>%
      filter(region == i) %>%
      ggplot() +
      # Plot data (Cases)
      aes(
        x = date,
        y = cases,
        colour = paste0(region," cases")
      ) %>%
      geom_point(size = line_size) +
      aes(
        x = date,
        y = ma_cases,
        colour = paste0(region," SMA cases")
      ) %>%
      geom_line(linewidth = line_size) +
      # Plot data (Deaths)
      aes(
        x = date_deaths,
        y = deaths,
        colour = paste0(region, " lag deaths")
      ) %>%
      geom_point(size = line_size) +
      aes(
        x = date_deaths,
        y = ma_deaths,
        colour = paste0(region, " lag SMA deaths")
      ) %>%
      geom_line(linewidth = line_size) +
      # X- axis
      scale_x_date(
        date_breaks = date_breaks, 
        date_minor_breaks = date_minor_breaks,
        sec.axis = dup_axis()
      ) +
      # Y - axis
      scale_y_continuous(n.breaks = n_y_breaks) +
      # Labels
      labs(
        x = "Date",
        y = "Normalized cases and deaths to maximum"
      ) +
      # Limits
      coord_cartesian(
        xlim = c(as.Date(initial_date), as.Date(final_date)),
        ylim = c(initial_y, final_y)
      ) +
      # Colors
      scale_color_manual(
        values = c("coral3", "darkblue", "darkblue", "coral3")
      ) +
      # Graph structure (Theme and legend)
      cowplot::theme_half_open(font_size) +
      theme(
        # Legend
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(axes_relative_size)),
        # Axis ticks
        axis.title.x.bottom = element_text(
          size = rel(axes_title_relative_size)
        ),
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
    
    # Saving plots and data in a folder with input date
    output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
    dir.create(output_folder)
    
    if(save_plots == TRUE) {
      Cairo(
        width = plot_width,
        height = plot_height,
        file = paste0(
          output_folder,
          "/plot_overlapped_cases_deaths_",
          stri_trans_tolower(i),
          ".png"
        ),
        type = "png", # tiff
        bg = "white", # white or transparent depending on your requirement 
        dpi = dots_per_inch,
        units = "px"  # you can change to pixels, etc
      )
      plot(graph)
      dev.off()
    }
    
    # Function development
    if(verbose >= 1) {
      cat(paste0("Saved plot for region: ", i, "\n"))
    }
  }
  
  return(0)
}
