# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Auxiliary function for active subregions plotting ----
plot_standard_count <- function(
  df_count,
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
  initial_drop = 17,
  final_drop = 17
) {
  # Normalization to maximum count in every region
  df_count <- df_count %>%
    group_by(region, subregion) %>%
    mutate(
      # Subregions with cases and deaths during one day
      max_sub_cases = max(count_act_sub_cases, na.rm = TRUE),
      max_sub_deaths = max(count_act_sub_deaths, na.rm = TRUE),
      # Subregions with at least one case (death) during the pandemic
      max_sub_cum_cases = max(count_act_sub_cum_cases, na.rm = TRUE),
      max_sub_cum_deaths = max(count_act_sub_cum_deaths, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      # Subregions with cases and deaths during one day
      cases = count_act_sub_cases / max_sub_cases,
      deaths = count_act_sub_deaths / max_sub_deaths,
      # Subregions with at least one case (death) during the pandemic
      cum_cases = count_act_sub_cum_cases / max_sub_cum_cases,
      cum_deaths = count_act_sub_cum_deaths / max_sub_cum_deaths
    ) %>%
    # Correct some anomalous data
    mutate(
      cum_cases = if_else(
        region == "Spain" & date >= as.Date("2022-03-28"),
        1,
        cum_cases
      ),
      cum_deaths = if_else(
        region == "Spain" & date >= as.Date("2022-03-28"),
        1,
        cum_deaths
      )
    )
  
  # Graph structure (Data information)
  graph <- df_count %>%
    ggplot() +
    # Plot data
    aes(
      x = date,
      y = df_count %>% pull(variable_name),
      colour = paste0(region, " ", gsub("cum_", "", variable_name))
    ) +
    geom_point(size = line_size) +
    # Axes
    scale_x_date(
      date_breaks = date_breaks, 
      date_minor_breaks = date_minor_breaks,
      sec.axis = dup_axis()
    ) +
    scale_y_continuous(n.breaks = n_y_breaks) +
    scale_shape_manual(seq(df_count %>% distinct(region) %>% nrow())) +
    # Labels
    labs(
      x = "Date",
      y = paste0("Subregions with ", gsub("cum_", "", variable_name))
    ) +
    # Limits
    coord_cartesian(
      xlim = c(
        as.Date(initial_date) + initial_drop,
        as.Date(final_date) - final_drop
      ),
      ylim = c(0, 1)
    )
  
  # Graph structure (Theme and legend)
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

# Graph of active subregions for cases and deaths----
plot_active_subregions <- function(
  list_count,
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
  initial_drop = 17,
  final_drop = 17,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
) {
  # Plot Cases
  plot_cases <- plot_standard_count(
    df_count = list_count %>% pluck("counts"),
    variable_name = "cum_cases",
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
    initial_drop = initial_drop,
    final_drop = final_drop
  )
  
  # Plot Deaths
  plot_deaths <- plot_standard_count(
    df_count = list_count %>% pluck("counts"),
    variable_name = "cum_deaths",
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
    initial_drop = initial_drop,
    final_drop = final_drop
  )
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    # Cases
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_count_cases.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_cases)
    dev.off()
    
    # Deaths
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(output_folder, "/plot_count_deaths.png"),
      type = "png", # tiff
      bg = "white", # white or transparent depending on your requirement 
      dpi = dots_per_inch,
      units = "px"  # you can change to pixels, etc
    )
    plot(plot_deaths)
    dev.off()
    
    # Resume data
    write_csv(
      list_count %>% pluck("resume"),
      paste0(output_folder, "/df_counts.csv")
    )
  }
  
  return(0)
}
