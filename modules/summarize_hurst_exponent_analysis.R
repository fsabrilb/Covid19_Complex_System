# Libraries ----
library(Cairo)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(data.table)

# Merge of list with common keys ----
list_join <- function(x, y) {
  for(i in names(x)) {x[[i]] <- x[[i]] %>% bind_rows(y[[i]])}
  return(x)
}

# Hurst exponent for different lengths obtained in the initial analysis ----
get_hurst_resume <- function(list_final_hurst) {
  df_hurst_resume <- left_join(
    x = list_final_hurst %>%
      pluck("hurst") %>%
      filter(statistic == "Estimate") %>%
      group_by(length) %>%
      summarise(
        hurst_mean = mean(hurst_value, na.rm = TRUE),
        hurst_excess = mean(hurst_value, na.rm = TRUE) - 0.5,
        hurst_sd_simulation = sd(hurst_value, na.rm = TRUE),
        simulation = max(simulation, na.rm = TRUE)
      ) %>%
      ungroup(),
    y = list_final_hurst %>%
      pluck("hurst") %>%
      filter(statistic == "Standard error") %>%
      group_by(length) %>%
      summarise(hurst_sd_method = mean(hurst_value, na.rm = TRUE)) %>%
      ungroup(),
    by = "length"
  ) %>% 
    mutate(
      hurst_sd =
        sqrt((hurst_sd_simulation / sqrt(simulation))^2 + hurst_sd_method^2)
    )
  
  return(df_hurst_resume)
}

# Fitting to power law of Hurst resume for estimate excess in Hurst ----
adjust_hurst_excess_power_law <- function(df_final_hurst) {
  # Estimate Hurst Exponent excess as a power law of length
  df_hurst_excess_sd_power_law <- lm(
    df_final_hurst %>% pull(hurst_mean) %>% log() ~
      df_final_hurst %>% pull(length) %>% log()
  ) %>%
    summary() %>%
    pluck("coefficients") %>%
    t() %>%
    data.table() %>%
    rename_all(function(x) {c("excess_coefficient", "excess_exponent")}) %>%
    mutate(
      statistic = c("Estimate", "Standard error", "t value", "Pr(>|t|)")
    ) %>%
    relocate(statistic) %>%
    left_join(
      lm(
        df_final_hurst %>% pull(hurst_sd) %>% log() ~
          df_final_hurst %>% pull(length) %>% log()
      ) %>%
        summary() %>%
        pluck("coefficients") %>%
        t() %>%
        data.table() %>%
        rename_all(function(x) {c("sd_coefficient", "sd_exponent")}) %>%
        mutate(
          statistic = c("Estimate", "Standard error", "t value", "Pr(>|t|)")
        ) %>%
        relocate(statistic),
      by = "statistic"
    )
  
  return(df_hurst_excess_sd_power_law)
}

# Graph of Hurst Excess obtained from simulations of Brownian paths ----
plot_hurst_excess <- function(
  df_final_hurst,
  df_hurst_resume,
  hurst_method = "rs",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  n_x_breaks = 20,
  n_y_breaks = 25,
  initial_x = 0,
  final_x = 60,
  initial_y = 0,
  final_y = 1,
  output_path = "./output_files",
  save_plots = FALSE,
  input_date = "2022-12-04",
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
) {
  # Parameters of power law regression
  excess_c <- df_hurst_resume %>%
    filter(statistic == "Estimate") %>%
    pull(excess_coefficient) %>%
    as.numeric()
  
  excess_e <- df_hurst_resume %>%
    filter(statistic == "Estimate") %>%
    pull(excess_exponent) %>%
    as.numeric()
  
  sd_c <- df_hurst_resume %>%
    filter(statistic == "Standard error") %>%
    mutate(sd = excess_coefficient) %>%
    pull(sd) %>%
    as.numeric()
  
  sd_e <- df_hurst_resume %>%
    filter(statistic == "Standard error") %>%
    mutate(sd = excess_exponent) %>%
    pull(sd) %>%
    as.numeric()
  
  # Estimate power law regression on original data of resume
  df_graph <- df_final_hurst %>%
    mutate(hurst_excess_fit = exp(excess_c) * (length)^excess_e - 0.5) %>%
    mutate(
      hurst_excess_lower = hurst_excess_fit *
        (1 - log(length) * sd_e - sd_c),
      hurst_excess_upper = hurst_excess_fit *
        (1 + log(length) * sd_e + sd_c)
    )
  
  # Graph structure (Data information)
  graph <- df_graph %>%
    ggplot() +
    # Plot data (Excess simulated)
    aes(
      x = length,
      y = hurst_excess,
      colour = "simulated data"
    ) %>%
    geom_point(size = line_size * 2) +
    geom_errorbar(
      aes(
        x = length,
        y = hurst_excess,
        ymin = hurst_excess - hurst_sd,
        ymax = hurst_excess + hurst_sd,
        colour = "simulated data"
      ),
      size = 0.7,
      width = sd(df_graph %>% pull(length), na.rm = TRUE) /
        sqrt(max(df_graph %>% pull(length), na.rm = TRUE))
    ) +
    # Plot data (Excess fitting)
    aes(
      x = length,
      y = hurst_excess_fit,
      colour = "fitting"
    ) %>%
    geom_line(size = line_size) +
    geom_ribbon(
      aes(x = length, ymin = hurst_excess_lower, ymax = hurst_excess_upper),
      fill= "darkblue",
      label = "fitting",
      alpha = 0.2
    ) +
    # X- axis
    scale_x_continuous(n.breaks = n_x_breaks) +
    # Y - axis
    scale_y_continuous(n.breaks = n_y_breaks) +
    # Labels
    labs(
      x = "Time series length",
      y = "Hurst exponent excess"
    ) +
    # Limits
    coord_cartesian(
      xlim = c(initial_x, final_x),
      ylim = c(initial_y, final_y)
    ) +
    # Colors
    scale_color_manual(
      values = c("darkblue", "coral3", "darkblue", "coral3")
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
  
  # Saving plots and data in a folder with input date
  output_folder <- paste0(output_path, "/", gsub("-", "", input_date))
  dir.create(output_folder)
  
  if(save_plots == TRUE) {
    Cairo(
      width = plot_width,
      height = plot_height,
      file = paste0(
        output_folder,
        "/plot_hurst_excess_",
        stri_trans_tolower(hurst_method),
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
  
  return(0)
}
