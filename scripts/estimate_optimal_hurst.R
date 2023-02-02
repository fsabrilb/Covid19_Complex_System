# ------------------------------ HURST ANALYSIS ------------------------------ #
# Author: Felipe Segundo Abril Bermúdez
rm(list = ls())

# Local path of modules for deployment ----
setwd("/home/ASIS/Temp_Felipe")
source("./modules/estimate_hurst_exponent_rs.R")
source("./modules/estimate_hurst_exponent_mfdfa.R")
source("./modules/summarize_hurst_exponent_analysis.R")

# Local path of data frames and global variables ----
setwd("/home/ASIS/Temp_Felipe")
input_path_raw <- "./input_files/raw_data"
input_path_processed <- "./input_files/processed_data"
input_path_data_dictionary <- "./input_files/data_dictionary"
output_path <- "./output_files"
input_generation_date <- "2022-12-04"

# Setup parallel backend to use many processors (Turn on cluster) ----
cluster <- makeCluster(detectCores() - 2)
registerDoParallel(cluster)

# Generation of Brownian paths for estimate optimal Hurst Exponent ----
df_brownian_path <- generate_brownian_path(
  n_simulation = 100,
  n_length = 1000,
  mean = 0,
  sd = 1
)

# Estimation of Hurst exponent over many simulations of Brownian paths (R/S)----
list_hurst_normal_rs <- estimate_hurst_brownian_path_rs(
  df_brownian_path = df_brownian_path,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0
)

# Estimation of Hurst exp over many simulations of Brownian paths MFDFA ----
list_hurst_normal_mfdfa <- estimate_hurst_brownian_path_mfdfa(
  df_brownian_path = df_brownian_path %>% filter(simulation %in% c(1:3)),
  dfa_degree = 1,
  q_order = 2,
  n_step = 5,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0
)

# Estimation of excess of the Hurst exponent in Brownian paths (R/S) ----
list_final_hurst_rs_1 <- estimate_excess_hurst_brownian_path_rs(
  n_simulation_vector = rep(1000, 14),
  n_length_vector = c(
    60, 74, 91, 105, 121, 135, 152, 166, 182, 196, 213, 227, 244, 258
  ),
  mean = 0,
  sd = 1,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 1
)

list_final_hurst_rs_2 <- estimate_excess_hurst_brownian_path_rs(
  n_simulation_vector = rep(1000, 14),
  n_length_vector = c(
    274, 288, 305, 319, 335, 349, 366, 380, 397, 411, 425, 439, 456, 470
  ),
  mean = 0,
  sd = 1,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 2
)

list_final_hurst_rs_3 <- estimate_excess_hurst_brownian_path_rs(
  n_simulation_vector = rep(1000, 14),
  n_length_vector = c(
    486, 500, 517, 531, 547, 561, 578, 592, 609, 623, 639, 653, 670, 684
  ),
  mean = 0,
  sd = 1,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 3
)

list_final_hurst_rs_4 <- estimate_excess_hurst_brownian_path_rs(
  n_simulation_vector = rep(1000, 14),
  n_length_vector = c(
    700, 714, 731, 745, 762, 776, 790, 804, 821, 835, 851, 865, 882, 896
  ),
  mean = 0,
  sd = 1,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 4
)

# Summarize of excess of the Hurst exponent in Brownian paths (R/S) ----
list_final_hurst_rs <- list_final_hurst_rs_1 %>%
  list_join(list_final_hurst_rs_2) %>%
  list_join(list_final_hurst_rs_3) %>%
  list_join(list_final_hurst_rs_4)

df_final_hurst_rs <- get_hurst_resume(list_final_hurst = list_final_hurst_rs)
df_hurst_resume_rs <- adjust_hurst_excess_power_law(df_final_hurst_rs)
plot_hurst_excess(
  df_final_hurst = df_final_hurst_rs,
  df_hurst_resume = df_hurst_resume_rs,
  hurst_method = "rs",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  n_x_breaks = 20,
  n_y_breaks = 25,
  initial_x = df_final_hurst_rs %>% pull(length) %>% min(na.rm = TRUE),
  final_x = df_final_hurst_rs %>% pull(length) %>% max(na.rm = TRUE),
  initial_y = df_final_hurst_rs %>%
    mutate(temp = hurst_excess - hurst_sd) %>%
    pull(temp) %>%
    min(na.rm = TRUE),
  final_y = df_final_hurst_rs %>%
    mutate(temp = hurst_excess + hurst_sd) %>%
    pull(temp) %>%
    max(na.rm = TRUE),
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

write.csv(
  df_hurst_resume_rs,
  paste0(
    input_path_processed,
    "/df_hurst_excess_rs_summary_",
    gsub("-", "", input_generation_date),
    ".csv"
  ),
  fileEncoding = "UTF-8",
  row.names = FALSE
)

# Estimation of excess of the Hurst exponent in Brownian paths MFDFA ----
list_final_hurst_mfdfa_1 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(1000, 7),
  n_length_vector = c(60, 74, 91, 105, 121, 135, 152),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 3,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 1
)

list_final_hurst_mfdfa_2 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(166, 182, 196, 213, 227, 244, 258),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 2
)

list_final_hurst_mfdfa_3 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(274, 288, 305, 319, 335, 349, 366),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 3
)

list_final_hurst_mfdfa_4 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(380, 397, 411, 425, 439, 456, 470),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 4
)

list_final_hurst_mfdfa_5 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(486, 500, 517, 531, 547, 561, 578),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 5
)

list_final_hurst_mfdfa_6 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(592, 609, 623, 639, 653, 670, 684),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 6
)

list_final_hurst_mfdfa_7 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(700, 714, 731, 745, 762, 776, 790),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 7
)

list_final_hurst_mfdfa_8 <- estimate_excess_hurst_brownian_path_mfdfa(
  n_simulation_vector = rep(100, 7),
  n_length_vector = c(804, 821, 835, 851, 865, 882, 896),
  mean = 0,
  sd = 1,
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 8
)

# Summarize of excess of the Hurst exponent in Brownian paths MFDFA ----
list_final_hurst_mfdfa <- list_final_hurst_mfdfa_1 %>%
  list_join(list_final_hurst_mfdfa_2) %>%
  list_join(list_final_hurst_mfdfa_3) %>%
  list_join(list_final_hurst_mfdfa_4) %>%
  list_join(list_final_hurst_mfdfa_5) %>%
  list_join(list_final_hurst_mfdfa_6) %>%
  list_join(list_final_hurst_mfdfa_7) %>%
  list_join(list_final_hurst_mfdfa_8)

df_final_hurst_mfdfa <- get_hurst_resume(list_final_hurst_mfdfa)
df_hurst_resume_mfdfa <- adjust_hurst_excess_power_law(df_final_hurst_mfdfa)
plot_hurst_excess(
  df_final_hurst = df_final_hurst_mfdfa,
  df_hurst_resume = df_hurst_resume_mfdfa,
  hurst_method = "mfdfa",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  n_x_breaks = 20,
  n_y_breaks = 25,
  initial_x = df_final_hurst_mfdfa %>% pull(length) %>% min(na.rm = TRUE),
  final_x = df_final_hurst_mfdfa %>% pull(length) %>% max(na.rm = TRUE),
  initial_y = df_final_hurst_mfdfa %>%
    mutate(temp = hurst_excess - hurst_sd) %>%
    pull(temp) %>%
    min(na.rm = TRUE),
  final_y = df_final_hurst_mfdfa %>%
    mutate(temp = hurst_excess + hurst_sd) %>%
    pull(temp) %>%
    max(na.rm = TRUE),
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

write.csv(
  df_hurst_resume_mfdfa,
  paste0(
    input_path_processed,
    "/df_hurst_excess_mfdfa_summary_",
    gsub("-", "", input_generation_date),
    ".csv"
  ),
  fileEncoding = "UTF-8",
  row.names = FALSE
)

# Estimation of multiscaling exponents with MFDFA (Example) ----
list_hurst_multsicaling_mfdfa <- get_multiscaling_exponents_mfdfa(
  df_time_series = df_brownian_path %>% filter(simulation == 1),
  dfa_degree = 1,
  q_order_vector = c(1:5),
  n_step = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 1
)

# Uninstall parallel backend to use many processors (Turn off cluster) ----
stopCluster(cluster)

# Clear workspace and free used memory ----
rm(list = ls())
gc()
