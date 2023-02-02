# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Auxiliary function for Theil index plotting ----
plot_standard_theil_index <- function(
  df_theil_covid,
  information_name = "cases",
  variable_name = "norm_sum_theil_index",
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
  # Theil values per cases and deaths
  df_graph <- df_theil_covid %>% filter(information == information_name)
  
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

# Graph of Theil index for cases and deaths ----
plot_theil_index_data <- function(
  df_theil_covid,
  variable_name = "norm_sum_theil_index",
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
  # Plot Theil index cases
  plot_theil_cases <- plot_standard_theil_index(
    df_theil_covid = df_theil_covid,
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
    initial_y = df_theil_covid %>%
      filter(information == "cases") %>%
      pull(variable_name) %>%
      min(na.rm = TRUE),
    final_y = df_theil_covid %>%
      filter(information == "cases") %>%
      pull(variable_name) %>%
      max(na.rm = TRUE)
  )
  
  # Plot Theil index deaths
  plot_theil_deaths <- plot_standard_theil_index(
    df_theil_covid = df_theil_covid,
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
    initial_y = df_theil_covid %>%
      filter(information == "deaths") %>%
      pull(variable_name) %>%
      min(na.rm = TRUE),
    final_y = df_theil_covid %>%
      filter(information == "deaths") %>%
      pull(variable_name) %>%
      max(na.rm = TRUE)
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Theil index cases
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
    plot(plot_theil_cases)
    dev.off()
    
    # Theil index deaths
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
    plot(plot_theil_deaths)
    dev.off()
    
    # Theil index per region and date with errors
    write_csv(
      df_theil_covid,
      paste0(
        output_folder,
        "/df_theil_index_data_",
        fixed_window,
        ".csv"
      )
    )
  }
  
  return(0)
}

# Graph of all entropy index for cases and deaths ----
plot_entropy_index_data <- function(
  df_theil_covid,
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
  index_loop <- data.table("temp" = df_theil_covid %>% names()) %>%
    filter(temp %like% c("index")) %>%
    pull(temp)
  
  for(i in index_loop) {
    # Graph of Theil index for cases and deaths ----
    plot_theil_index_data(
      df_theil_covid = df_theil_covid,
      variable_name = i,
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
      output_path = output_path,
      save_plots = save_plots,
      input_date = input_date,
      plot_width = plot_width,
      plot_height = plot_height,
      dots_per_inch = dots_per_inch
    )
    
    cat(
      paste0(
        "Plot Entropy graph with variable: ",
        gsub("_", " ", i),
        "\n"
      )
    )
  }
  return(0)
}