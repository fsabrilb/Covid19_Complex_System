# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Auxiliary function for Temporal Theil Scaling plotting ----
plot_standard_tts <- function(
  df_tts_covid,
  information_name = "cases",
  variable_name = "tts_coefficient_value",
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
  # Temporal Theil Scaling values per cases and deaths
  df_graph <- df_tts_covid %>%
    filter(information == information_name) %>%
    group_by(region) %>%
    mutate(
      increase_tts_coefficient = tts_coefficient_value -
        min(tts_coefficient_value, na.rm = TRUE),
      rank_tts_coefficient = max(tts_coefficient_value, na.rm = TRUE) -
        min(tts_coefficient_value, na.rm = TRUE),
      increase_tts_exponent = tts_exponent_value -
        min(tts_exponent_value, na.rm = TRUE),
      rank_tts_exponent = max(tts_exponent_value, na.rm = TRUE) -
        min(tts_exponent_value, na.rm = TRUE)
    ) %>%
    mutate(
      tts_coefficient_value = increase_tts_coefficient / rank_tts_coefficient,
      tts_exponent_value = increase_tts_exponent / rank_tts_exponent,
      tts_coefficient_sd = tts_coefficient_sd / rank_tts_coefficient,
      tts_exponent_sd = tts_exponent_sd / rank_tts_exponent
    ) %>%
    ungroup()
  
  # Auxiliar function for Y title
  rename_ytitle <- function(variable_name) {
    if(variable_name == "tts_coefficient_value") {
      x <- "Normalized logarithm of TTS coefficient"
    } else {
      x <- "Normalized TTS exponent"
    }
    x <- paste0(
      x,
      " - ",
      stri_trans_totitle(information_name)
    )
    return(x)
  }
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data
    aes(
      x = date,
      y = df_graph %>% pull(variable_name),
      colour = paste0(region, " - ", time_series)
    ) %>%
    geom_point(size = line_size) +
    # Error bars (variable)
    aes(
      x = date,
      ymin = df_graph %>% pull(variable_name) -
        df_graph %>% pull(gsub("value", "sd", variable_name)),
      ymax = df_graph %>% pull(variable_name) +
        df_graph %>% pull(gsub("value", "sd", variable_name)),
      fill = paste0(region, " - ", time_series)
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
      y = rename_ytitle(variable_name = variable_name)
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

# Graph of Temporal Theil Scaling for cases and deaths ----
plot_tts_data <- function(
  df_tts_covid,
  variable_name = "tts_coefficient_value",
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
  # Plot Temporal Fluctuation Scaling cases
  plot_tts_cases <- plot_standard_tts(
    df_tts_covid = df_tts_covid,
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
    initial_date = as.Date(initial_date) + initial_drop_days,
    final_date = as.Date(final_date) - final_drop_days,
    initial_y = 0,
    final_y = 1
  )
  
  # Plot Temporal Fluctuation Scaling deaths
  plot_tts_deaths <- plot_standard_tts(
    df_tts_covid = df_tts_covid,
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
    initial_date = as.Date(initial_date) + initial_drop_days,
    final_date = as.Date(final_date) - final_drop_days,
    initial_y = 0,
    final_y = 1
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Temporal Theil Scaling cases
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
    plot(plot_tts_cases)
    dev.off()
    
    # Temporal Theil Scaling deaths
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
    plot(plot_tts_deaths)
    dev.off()
    
    # Temporal Theil Scaling per region and date with errors
    write_csv(
      df_tts_covid,
      paste0(
        output_folder,
        "/df_tts_data_",
        fixed_window,
        ".csv"
      )
    )
    
    # Temporal Theil Scaling Statistical significance with R2
    write_csv(
      df_tts_covid %>%
        group_by(region, subregion, information, time_series) %>%
        mutate(
          min_tts_coefficient = min(tts_coefficient_value, na.rm = TRUE),
          max_tts_coefficient = max(tts_coefficient_value, na.rm = TRUE),
          min_tts_exponent = min(tts_exponent_value, na.rm = TRUE),
          max_tts_exponent = max(tts_exponent_value, na.rm = TRUE)
        ) %>%
        summarise(
          min_tts_coefficient = mean(min_tts_coefficient, na.rm = TRUE),
          max_tts_coefficient = mean(max_tts_coefficient, na.rm = TRUE),
          min_tts_exponent = mean(min_tts_exponent, na.rm = TRUE),
          max_tts_exponent = mean(max_tts_exponent, na.rm = TRUE),
          tts_r2_95 = quantile(
            tts_exponent_r2,
            na.rm = TRUE,
            probs = c(0.05),
            names = FALSE
          ),
          tts_r2_85 = quantile(
            tts_exponent_r2,
            na.rm = TRUE,
            probs = c(0.15),
            names = FALSE
          ),
          tts_r2_75 = quantile(
            tts_exponent_r2,
            na.rm = TRUE,
            probs = c(0.25),
            names = FALSE
          )
        ) %>%
        ungroup(),
      paste0(
        output_folder,
        "/df_tts_resume_data_",
        fixed_window,
        ".csv"
      )
    )
  }
  
  return(0)
}
