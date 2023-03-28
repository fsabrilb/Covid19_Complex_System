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
    distinct()
  
  # Auxiliary function for title selection in y coordinate
  title_y <- function(x) {
    x <- x %>%
      stri_replace_all_regex(c("_", "cases"), c(" ", "")) %>%
      stri_trans_totitle()
    if(x == "alpha_cases" | x == "alpha_deaths") {
      x <- paste0("Coefficient", " of ensemble fluctuation scaling")
    } else {
      x <- paste0("Exponent", " of ensemble fluctuation scaling")
    }
    return(x)
  }
  
  # Auxiliary function for y-axis scale
  scale_y <- function(x) {
    if(x == "alpha_cases" | x == "alpha_deaths") {
      x <- "identity"
    } else {
      x <- "log10"
    }
    return(x)
  }
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data (Cases)
    aes(
      x = df_graph %>% pull(end_date),
      y = df_graph %>% pull(variable_name),
      colour = paste0(region, " - cases")
    ) %>%
    geom_point(size = 2.1) +
    aes(
      x = df_graph %>% pull(end_date),
      y = df_graph %>% pull(variable_name),
      colour = paste0(region, " - cases")
    ) %>%
    geom_line(linewidth = line_size) +
    # Plot data (Deaths)
    aes(
      x = df_graph %>% pull(end_date),
      y = df_graph %>% pull(gsub("cases", "deaths", variable_name)),
      colour = paste0(region, " - deaths")
    ) %>%
    geom_point(size = 2.1) +
    aes(
      x = df_graph %>% pull(end_date),
      y = df_graph %>% pull(gsub("cases", "deaths", variable_name)),
      colour = paste0(region, " - deaths")
    ) %>%
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
plot_spatial_evolution_tfs <- function(
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
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  for(i in df_spatial_tfs %>% distinct(region) %>% pull()) {
    cat(paste0("- Plot Ensemble Fluctuation Scaling: ", i, "\n"))
    
    # Plot Alpha evolution
    plot_alpha <- plot_standard_spatial_tfs(
      df_spatial_tfs = df_spatial_tfs %>% filter(region == i),
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
          region == i,
          date >= initial_date,
          date <= final_date
        ) %>%
        mutate(
          minimum = pmin(alpha_cases, alpha_deaths),
          maximum = pmax(alpha_cases, alpha_deaths)
        ) %>%
        pull(minimum) %>%
        min(na.rm = TRUE),
      final_y = df_spatial_tfs %>%
        filter(
          region == i,
          date >= initial_date,
          date <= final_date
        ) %>%
        mutate(
          minimum = pmin(alpha_cases, alpha_deaths),
          maximum = pmax(alpha_cases, alpha_deaths)
        ) %>%
        pull(maximum) %>%
        max(na.rm = TRUE)
    )
    
    # Plot Coefficient evolution
    plot_coefficient <- plot_standard_spatial_tfs(
      df_spatial_tfs = df_spatial_tfs %>% filter(region == i),
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
          region == i,
          date >= initial_date,
          date <= final_date
        ) %>%
        mutate(
          minimum = pmin(coefficient_cases, coefficient_deaths),
          maximum = pmax(coefficient_cases, coefficient_deaths)
        ) %>%
        pull(minimum) %>%
        min(na.rm = TRUE),
      final_y = df_spatial_tfs %>%
        filter(
          region == i,
          date >= initial_date,
          date <= final_date
        ) %>%
        mutate(
          minimum = pmin(coefficient_cases, coefficient_deaths),
          maximum = pmax(coefficient_cases, coefficient_deaths)
        ) %>%
        pull(maximum) %>%
        max(na.rm = TRUE)
    )
    
    if(save_plots == TRUE) {
      # Alpha evolution
      Cairo(
        width = plot_width,
        height = plot_height,
        file = paste0(
          output_folder,
          "/plot_spatial_tfs_alpha_",
          stri_trans_tolower(i),
          ".png"
        ),
        type = "png", # tiff
        bg = "white", # white or transparent depending on your requirement 
        dpi = dots_per_inch,
        units = "px"  # you can change to pixels, etc
      )
      plot(plot_alpha)
      dev.off()
      
      # Coefficient evolution
      Cairo(
        width = plot_width,
        height = plot_height,
        file = paste0(
          output_folder,
          "/plot_spatial_tfs_coefficient_",
          stri_trans_tolower(i),
          ".png"
          ),
        type = "png", # tiff
        bg = "white", # white or transparent depending on your requirement 
        dpi = dots_per_inch,
        units = "px"  # you can change to pixels, etc
      )
      plot(plot_coefficient)
      dev.off()
    }
  }
  
  if(save_plots == TRUE) {
    # Spatial TFS data obtained from power law regression
    write_csv(
      df_spatial_tfs,
      paste0(output_folder, "/df_spatially_tfs_evolution.csv")
    )
  }
  
  return(0)
}
