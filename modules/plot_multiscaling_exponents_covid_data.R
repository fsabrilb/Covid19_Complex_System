# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Graph of Multiscaling exponents in cases and deaths for every region ----
plot_multiscaling_overlapping <- function(
  df_multiscaling_exponents_covid,
  fixed_window = "no_fixed",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  n_x_breaks = 25,
  n_y_breaks = 25,
  initial_x = -1,
  final_x = 2,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400,
  verbose = 1
) {
  df_graph_cases <- df_multiscaling_exponents_covid %>%
    filter(information == "cases") %>%
    select(region, subregion, date, order, hurst_value, hurst_sd) %>%
    rename(hurst_value_cases = hurst_value, hurst_sd_cases = hurst_sd)
  
  df_graph_deaths <- df_multiscaling_exponents_covid %>%
    filter(information == "deaths") %>%
    select(region, subregion, date, order, hurst_value, hurst_sd) %>%
    rename(hurst_value_deaths = hurst_value, hurst_sd_deaths = hurst_sd)
  
  # Loop over regions
  loop_index <- c(
    df_graph_cases %>% distinct(region) %>% pull(),
    df_graph_deaths %>% distinct(region) %>% pull()
  ) %>% unique()
  for(i in loop_index) {
    initial_y_cases <- df_graph_cases %>%
      filter(region == i) %>%
      pull(hurst_value_cases) %>%
      min(na.rm = TRUE)
    final_y_cases <- df_graph_cases %>%
      filter(region == i) %>%
      pull(hurst_value_cases) %>%
      max(na.rm = TRUE)
    initial_y_deaths <- df_graph_deaths %>%
      filter(region == i) %>%
      pull(hurst_value_deaths) %>%
      min(na.rm = TRUE)
    final_y_deaths <- df_graph_deaths %>%
      filter(region == i) %>%
      pull(hurst_value_deaths) %>%
      max(na.rm = TRUE)
    
    # Graph structure (Data information)
    graph_cases <- df_graph_cases %>%
      filter(region == i) %>%
      ggplot() +
      # Plot data (Cases)
      aes(
        x = order,
        y = hurst_value_cases,
        colour = paste0("Cases, ", date)
      ) %>%
      geom_point(size = 1.8) +
      aes(
        x = order,
        y = hurst_value_cases,
        colour = paste0("Cases, ", date)
      ) %>%
      geom_line(linewidth = line_size) +
      # Error bars (Cases)
      aes(
        x = order,
        ymin = hurst_value_cases - hurst_sd_cases,
        ymax = hurst_value_cases + hurst_sd_cases,
        fill = paste0("Cases, ", date)
      ) %>%
      geom_ribbon(alpha = 0.09) +
      # X- axis
      scale_x_continuous(n.breaks = n_x_breaks) +
      # Y - axis
      scale_y_continuous(n.breaks = n_y_breaks) +
      # Labels
      labs(
        x = "Order q",
        y = "Generalized Hurst H(q)"
      ) +
      # Limits
      coord_cartesian(
        xlim = c(initial_x, final_x),
        ylim = c(initial_y_cases, final_y_cases)
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
    
    graph_deaths <- df_graph_deaths %>%
      filter(region == i) %>%
      ggplot() +
      # Plot data (Deaths)
      aes(
        x = order,
        y = hurst_value_deaths,
        colour = paste0("Deaths, ", date)
      ) %>%
      geom_point(size = line_size) +
      aes(
        x = order,
        y = hurst_value_deaths,
        colour = paste0("Deaths, ", date)
      ) %>%
      geom_line(linewidth = line_size) +
      # Error bars (Cases)
      aes(
        x = order,
        ymin = hurst_value_deaths - hurst_sd_deaths,
        ymax = hurst_value_deaths + hurst_sd_deaths,
        fill = paste0("Cases, ", date)
      ) %>%
      geom_ribbon(alpha = 0.05) +
      # X- axis
      scale_x_continuous(n.breaks = n_x_breaks) +
      # Y - axis
      scale_y_continuous(n.breaks = n_y_breaks) +
      # Labels
      labs(
        x = "Order q",
        y = "Generalized Hurst H(q)"
      ) +
      # Limits
      coord_cartesian(
        xlim = c(initial_x, final_x),
        ylim = c(initial_y_deaths, final_y_deaths)
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
      # Cases
      Cairo(
        width = plot_width,
        height = plot_height,
        file = paste0(
          output_folder,
          "/plot_multiscaling_cases_",
          stri_trans_tolower(i),
          "_",
          fixed_window,
          ".png"
        ),
        type = "png", # tiff
        bg = "white", # white or transparent depending on your requirement 
        dpi = dots_per_inch,
        units = "px"  # you can change to pixels, etc
      )
      plot(graph_cases)
      dev.off()
      
      # Deaths
      Cairo(
        width = plot_width,
        height = plot_height,
        file = paste0(
          output_folder,
          "/plot_multiscaling_deaths_",
          stri_trans_tolower(i),
          "_",
          fixed_window,
          ".png"
        ),
        type = "png", # tiff
        bg = "white", # white or transparent depending on your requirement 
        dpi = dots_per_inch,
        units = "px"  # you can change to pixels, etc
      )
      plot(graph_deaths)
      dev.off()
    }
    
    # Function development
    if(verbose >= 1) {
      cat(paste0("Saved plot for region: ", i, "\n"))
    }
  }
  
  return(0)
}
