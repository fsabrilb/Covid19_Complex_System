# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Auxiliary function for Temporal Fluctuation Scaling plotting ----
plot_standard_tfs <- function(
  df_tfs_covid,
  information_name = "cases",
  variable_name = "tfs_coefficient_value",
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
  # Temporal Fluctuation Scaling values per cases and deaths
  df_graph <- df_tfs_covid %>% filter(information == information_name)
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data
    aes(
      x = date,
      y = df_graph %>% pull(variable_name),
      colour = region
    ) %>%
    geom_point(size = line_size) +
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
      y = paste0(
        gsub("_", " ", stri_trans_totitle(variable_name)),
        " ",
        information_name
      )
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

# Graph of Temporal Fluctuation Scaling for cases and deaths ----
plot_tfs_data <- function(
  df_tfs_covid,
  variable_name = "tfs_coefficient_value",
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
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
) {
  # Plot Temporal Fluctuation Scaling cases
  plot_tfs_cases <- plot_standard_tfs(
    df_tfs_covid = df_tfs_covid,
    information_name = "cases",
    variable_name = variable_name,
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
    initial_y = df_tfs_covid %>%
      filter(information == "cases") %>%
      pull(variable_name) %>%
      min(na.rm = TRUE),
    final_y = df_tfs_covid %>%
      filter(information == "cases") %>%
      pull(variable_name) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Temporal Fluctuation Scaling deaths
  plot_tfs_deaths <- plot_standard_tfs(
    df_tfs_covid = df_tfs_covid,
    information_name = "deaths",
    variable_name = variable_name,
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
    initial_y = df_tfs_covid %>%
      filter(information == "deaths") %>%
      pull(variable_name) %>%
      min(na.rm = TRUE),
    final_y = df_tfs_covid %>%
      filter(information == "deaths") %>%
      pull(variable_name) %>%
      max(na.rm = TRUE)
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Temporal Fluctuation Scaling cases
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_",
        variable_name,
        "_cases_",
        fixed_window,
        ".png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_tfs_cases)
    dev.off()
    
    # Temporal Fluctuation Scaling deaths
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_",
        variable_name,
        "_deaths_",
        fixed_window,
        ".png"
      ),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_tfs_deaths)
    dev.off()
    
    # Temporal Fluctuation Scaling per region and date with errors
    write_csv(
      df_tfs_covid,
      paste0(
        output_folder,
        "/df_tfs_data_",
        fixed_window,
        ".csv"
      )
    )
  }
  
  return(0)
}
