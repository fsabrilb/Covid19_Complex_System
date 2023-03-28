# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Auxiliary function for Hurst Exponent plotting ----
plot_standard_hurst_exponent <- function(
  df_hurst_covid,
  variable_name = "cases",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-03-30",
  final_date = "2022-11-01",
  initial_y = 0,
  final_y = 1
) {
  # Hurst values per cases and deaths
  df_graph <- df_hurst_covid %>% filter(information == variable_name)
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data
    aes(
      x = date,
      y = hurst_value,
      colour = region
    ) %>%
    geom_point(size = line_size) +
    # Error bars
    aes(
      x = date,
      ymin = hurst_value - hurst_sd,
      ymax = hurst_value + hurst_sd,
      fill = region
    ) %>%
    geom_ribbon(alpha = 0.09) +
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
      y = paste0("Hurst exponent ", variable_name)
    ) +
    # Limits
    coord_cartesian(
      xlim = c(as.Date(initial_date), as.Date(final_date)),
      ylim = c(initial_y, final_y)
    ) +
    # Graph structure (Theme and legend)
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

# Graph of Hurst exponent for cases and deaths ----
plot_hurst_exponent_data <- function(
  df_hurst_covid,
  hurst_method = "rs",
  fixed_window = "no_fixed",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-03-30",
  final_date = "2022-11-01",
  initial_drop_days = 30,
  final_drop_days = 30,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
) {
  # Plot Hurst exponent cases
  plot_hurst_cases <- plot_standard_hurst_exponent(
    df_hurst_covid = df_hurst_covid,
    variable_name = "cases",
    font_size = font_size,
    axes_title_relative_size = axes_title_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + initial_drop_days,
    final_date = as.Date(final_date) - final_drop_days,
    initial_y = df_hurst_covid %>%
      filter(information == "cases") %>%
      pull(hurst_value) %>%
      min(na.rm = TRUE),
    final_y = df_hurst_covid %>%
      filter(information == "cases") %>%
      pull(hurst_value) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Hurst exponent deaths
  plot_hurst_deaths <- plot_standard_hurst_exponent(
    df_hurst_covid = df_hurst_covid,
    variable_name = "deaths",
    font_size = font_size,
    axes_title_relative_size = axes_title_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = as.Date(initial_date) + initial_drop_days,
    final_date = as.Date(final_date) - final_drop_days,
    initial_y = df_hurst_covid %>%
      filter(information == "deaths") %>%
      pull(hurst_value) %>%
      min(na.rm = TRUE),
    final_y = df_hurst_covid %>%
      filter(information == "deaths") %>%
      pull(hurst_value) %>%
      max(na.rm = TRUE)
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Hurst exponent cases
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_hurst_exponent_cases_",
        hurst_method,
        "_",
        fixed_window,
        ".png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_hurst_cases)
    dev.off()
    
    # Hurst exponent deaths
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_hurst_exponent_deaths_",
        hurst_method,
        "_",
        fixed_window,
        ".png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_hurst_deaths)
    dev.off()
    
    # Hurst Exponent per region and date with errors
    write_csv(
      df_hurst_covid,
      paste0(
        output_folder,
        "/df_hurst_exponent_data_",
        hurst_method,
        "_",
        fixed_window,
        ".csv"
      )
    )
  }
  
  return(0)
}

# Graph of Hurst exponent in cases and deaths for every region ----
plot_hurst_overlapping <- function(
  df_hurst_covid,
  hurst_method = "rs",
  fixed_window = "no_fixed",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-03-30",
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
  df_graph <- df_hurst_covid %>%
    filter(information == "cases") %>%
    select(region, subregion, date, hurst_value, hurst_sd) %>%
    rename(hurst_value_cases = hurst_value, hurst_sd_cases = hurst_sd) %>%
    left_join(
      df_hurst_covid %>%
        filter(information == "deaths") %>%
        select(region, subregion, date, hurst_value, hurst_sd) %>%
        rename(hurst_value_deaths = hurst_value, hurst_sd_deaths = hurst_sd),
      by = c("region", "subregion", "date")
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
        y = hurst_value_cases,
        colour = paste0(region, " cases")
      ) %>%
      geom_point(size = line_size) +
      aes(
        x = date,
        y = hurst_value_cases,
        colour = paste0(region, " cases")
      ) %>%
      geom_line(linewidth = line_size) +
      # Error bars (Cases)
      aes(
        x = date,
        ymin = hurst_value_cases - hurst_sd_cases,
        ymax = hurst_value_cases + hurst_sd_cases,
        fill = paste0(region, " cases")
      ) %>%
      geom_ribbon(alpha = 0.09) +
      # Plot data (Deaths)
      aes(
        x = date,
        y = hurst_value_deaths,
        colour = paste0(region, " deaths")
      ) %>%
      geom_point(size = line_size) +
      aes(
        x = date,
        y = hurst_value_deaths,
        colour = paste0(region, " deaths")
      ) %>%
      geom_line(linewidth = line_size) +
      # Error bars (Deaths)
      aes(
        x = date,
        ymin = hurst_value_deaths - hurst_sd_deaths,
        ymax = hurst_value_deaths + hurst_sd_deaths,
        fill = paste0(region, " deaths")
      ) %>%
      geom_ribbon(alpha = 0.09) +
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
        y = "Hurst exponent"
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
          "/plot_hurst_overlapped_",
          stri_trans_tolower(i),
          "_",
          hurst_method,
          "_",
          fixed_window,
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
