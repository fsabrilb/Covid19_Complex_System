# Libraries ----
library(Cairo)
library(dplyr)
library(readr)
library(ggplot2)
library(stringi)
library(data.table)

# Auxiliary function for evolution of spatial TFS plotting ----
plot_standard_spatial_tfs <- function(
  df_spatial_tfs,
  variable_name,
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
  initial_y = -1,
  final_y = 1
) {
  # Mean over days in every region
  df_graph <- df_spatial_tfs %>%
    select(
      -date,
      -mean_cum_cases,
      -sd_cum_cases,
      -mean_cum_deaths,
      -sd_cum_deaths
    ) %>%
    distinct() %>%
    filter(
      alpha_cases <
        quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
      alpha_cases >
        quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
      alpha_deaths <
        quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
      alpha_deaths >
        quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric(),
      r2_cases >= 0.95,
      r2_deaths >= 0.95
    )
  
  # Auxiliary function for title selection in y coordinate
  title_y <- function(x) {
    x <- x %>% stri_replace_all_regex("_", " ") %>% stri_trans_totitle()
    return(x)
  }
  
  # Auxiliary function for y-axis scale
  scale_y <- function(x) {
    if(x == "coefficient_cases" | x == "coefficient_deaths") {
      x <- "identity"
    } else {
      x <- "log10"
    }
    return(x)
  }
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data
    aes(
      x = df_graph %>% pull(end_date),
      y = df_graph %>% pull(variable_name),
      colour = region
    ) +
    geom_line(linewidth = line_size) +
    # X- axis
    scale_x_date(
      date_breaks = date_breaks, 
      date_minor_breaks = date_minor_breaks,
      sec.axis = dup_axis()
    ) +
    # Y - axis
    scale_y_continuous(n.breaks = n_y_breaks, trans = scale_y(variable_name)) +
    # Labels
    labs(
      x = "Date",
      y = title_y(variable_name)
    ) +
    coord_cartesian(
      xlim = c(as.Date(initial_date), as.Date(final_date)),
      ylim = c(initial_y, final_y)
    )
  
  # Graph structure (Axes, limits, theme and legend)
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

# Graph of spatial evolution of spatial TFS for cases and deaths ----
plot_spatial_evolution <- function(
  df_spatial_tfs,
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
  # Plot Alpha evolution (Cases)
  plot_alpha_cases <- plot_standard_spatial_tfs(
    df_spatial_tfs = df_spatial_tfs,
    variable_name = "alpha_cases",
    font_size = font_size,
    axes_title_relative_size = axes_title_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(alpha_cases) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(alpha_cases) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Alpha evolution (Deaths)
  plot_alpha_deaths <- plot_standard_spatial_tfs(
    df_spatial_tfs = df_spatial_tfs,
    variable_name = "alpha_deaths",
    font_size = font_size,
    axes_title_relative_size = axes_title_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(alpha_deaths) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(alpha_deaths) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Coefficient evolution (Cases)
  plot_coefficient_cases <- plot_standard_spatial_tfs(
    df_spatial_tfs = df_spatial_tfs,
    variable_name = "coefficient_cases",
    font_size = font_size,
    axes_title_relative_size = axes_title_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(coefficient_cases) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(coefficient_cases) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Coefficient evolution (Deaths)
  plot_coefficient_deaths <- plot_standard_spatial_tfs(
    df_spatial_tfs = df_spatial_tfs,
    variable_name = "coefficient_deaths",
    font_size = font_size,
    axes_title_relative_size = axes_title_relative_size,
    axes_relative_size = axes_relative_size,
    legend_cols = legend_cols,
    line_size = line_size,
    date_breaks = date_breaks,
    date_minor_breaks = date_minor_breaks,
    n_y_breaks = n_y_breaks,
    initial_date = initial_date,
    final_date = final_date,
    initial_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(coefficient_deaths) %>%
      min(na.rm = TRUE),
    final_y = df_spatial_tfs %>%
      filter(
        date >= initial_date,
        date <= final_date,
        alpha_cases <
          quantile(alpha_cases, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_cases >
          quantile(alpha_cases, 0.05, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths <
          quantile(alpha_deaths, 0.95, na.rm = TRUE) %>% as.numeric(),
        alpha_deaths >
          quantile(alpha_deaths, 0.05, na.rm = TRUE) %>% as.numeric()
      ) %>%
      pull(coefficient_deaths) %>%
      max(na.rm = TRUE)
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Alpha evolution (Cases)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_spatial_tfs_alpha_cases.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_alpha_cases)
    dev.off()
    
    # Alpha evolution (Deaths)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_spatial_tfs_alpha_deaths.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_alpha_deaths)
    dev.off()
    
    # Coefficient evolution (Cases)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_spatial_tfs_coefficient_cases.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_coefficient_cases)
    dev.off()
    
    # Coefficient evolution (Deaths)
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_spatial_tfs_coefficient_deaths.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_coefficient_deaths)
    dev.off()
    
    # Spatial TFS data obtained from power law regression
    write_csv(
      df_spatial_tfs,
      paste0(output_folder, "/df_spatially_tfs_evolution.csv")
    )
  }
  
  return(0)
}
