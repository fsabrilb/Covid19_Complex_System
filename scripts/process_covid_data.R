# -------------------------------- COVID DATA -------------------------------- #
# Author: Felipe Segundo Abril Bermúdez
rm(list = ls())
input_path <- "/home/ASIS/Temp_Felipe"

# Local path of modules for deployment ----
setwd(input_path)
source("./modules/generate_covid_data.R")
source("./modules/count_regions_covid_data.R")
source("./modules/plot_regions_covid_data.R")
source("./modules/estimate_correlation_covid_data.R")
source("./modules/plot_correlation_covid_data.R")
source("./modules/estimate_distribution_adjustment_covid_data.R")
source("./modules/plot_distribution_adjustment_covid_data.R")
source("./modules/estimate_spatial_temporal_fluctuation_scaling_covid_data.R")
source("./modules/plot_spatial_temporal_fluctuation_scaling_covid_data.R")
source("./modules/estimate_hurst_exponent_rs.R")
source("./modules/estimate_hurst_exponent_mfdfa.R")
source("./modules/estimate_hurst_exponent_covid_data.R")
source("./modules/plot_hurst_exponent_covid_data.R")
source("./modules/estimate_multiscaling_exponents_covid_data.R")
source("./modules/plot_multiscaling_exponents_covid_data.R")
source("./modules/estimate_theil_index.R")
source("./modules/estimate_theil_index_covid_data.R")
source("./modules/plot_theil_index_covid_data.R")
source("./modules/estimate_temporal_fluctuation_scaling.R")
source("./modules/estimate_temporal_fluctuation_scaling_covid_data.R")
source("./modules/plot_temporal_fluctuation_scaling_covid_data.R")

# Local path of data frames and global variables ----
setwd(input_path)
input_path_raw <- "./input_files/raw_data"
input_path_processed <- "./input_files/processed_data"
input_path_data_dictionary <- "./input_files/data_dictionary"
output_path <- "./output_files"
input_generation_date <- "2022-12-04"

# Generate Covid data through different regions (Preprocess data) ----
df_covid <- generate_all_covid_data(
  saved_world_data = TRUE,
  saved_usa_data = TRUE,
  saved_usa_county_data = TRUE,
  saved_brasil_data = TRUE,
  saved_europe_data = TRUE,
  saved_spain_data = TRUE,
  saved_india_data = TRUE,
  saved_colombia_data = TRUE,
  saved_all_data = TRUE,
  input_date = input_generation_date
)

# Estimate subregions with at least one case (death) during all pandemic ----
list_count <- count_active_subregions(df_covid = df_covid)
plot_active_subregions(
  list_count = list_count,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 8,
  line_size = 0.9,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Estimate correlation between cases and death in every region ----
list_correlation <- get_all_correlation_data(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  p_norm = 2,
  n_of_minima = 10,
  n_of_maxima = 10,
  verbose = 1
)
plot_correlation_data(
  list_correlation = list_correlation,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 8,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_x_breaks = 20,
  n_y_breaks = 25,
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  initial_x = 0,
  final_x = 60,
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)
plot_overlapping(
  list_correlation = list_correlation,
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
  initial_y = 0,
  final_y = 1,
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400,
  verbose = 1
)

# Estimate adjustment of spatial evolution cases and death in every region ----
df_spatial_adjustment <- fit_spatial_evolution(
  df_covid = df_covid,
  days_vector = c(
    "2020-04-16", "2020-05-01", "2020-05-15",
    "2020-06-01", "2020-06-15", "2020-07-01", "2020-07-15",
    "2020-08-01", "2020-08-15", "2020-09-01", "2020-09-15",
    "2020-10-01", "2020-10-15", "2020-11-01", "2020-11-15",
    "2020-12-01", "2020-12-15", "2021-01-01", "2021-01-15",
    "2021-02-01", "2021-02-15", "2021-03-01", "2021-03-15",
    "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15",
    "2021-06-01", "2021-06-15", "2021-07-01", "2021-07-15",
    "2021-08-01", "2021-08-15", "2021-09-01", "2021-09-15",
    "2021-10-01", "2021-10-15", "2021-11-01", "2021-11-15",
    "2021-12-01", "2021-12-15", "2022-01-01", "2022-01-15",
    "2022-02-01", "2022-02-15", "2022-03-01", "2022-03-15",
    "2022-04-01", "2022-04-15", "2022-05-01", "2022-05-15",
    "2022-06-01", "2022-06-15", "2022-07-01", "2022-07-15",
    "2022-08-01", "2022-08-15", "2022-09-01", "2022-09-15",
    "2022-10-01", "2022-10-15", "2022-11-01", "2022-11-15"
  ),
  tolerance_optimization = .Machine %>% pluck("double.eps") * 1e3,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
plot_spatial_evolution(
  df_spatial_adjustment = df_spatial_adjustment,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-04-16",
  final_date = "2022-11-01",
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Estimate spatial temporal fluctuation scaling (TFS) in every region ----
df_spatial_tfs_1 <- estimate_spatial_tfs(
  df_covid = df_covid,
  days_vector = c(
    "2020-04-16", "2020-05-01", "2020-05-15",
    "2020-06-01", "2020-06-15", "2020-07-01", "2020-07-15",
    "2020-08-01", "2020-08-15", "2020-09-01", "2020-09-15",
    "2020-10-01", "2020-10-15", "2020-11-01", "2020-11-15",
    "2020-12-01", "2020-12-15", "2021-01-01", "2021-01-15",
    "2021-02-01", "2021-02-15", "2021-03-01", "2021-03-15",
    "2021-04-01", "2021-04-15", "2021-05-01", "2021-05-15",
    "2021-06-01", "2021-06-15", "2021-07-01", "2021-07-15",
    "2021-08-01", "2021-08-15", "2021-09-01", "2021-09-15",
    "2021-10-01", "2021-10-15", "2021-11-01", "2021-11-15",
    "2021-12-01", "2021-12-15", "2022-01-01", "2022-01-15",
    "2022-02-01", "2022-02-15", "2022-03-01", "2022-03-15",
    "2022-04-01", "2022-04-15", "2022-05-01", "2022-05-15",
    "2022-06-01", "2022-06-15", "2022-07-01", "2022-07-15",
    "2022-08-01", "2022-08-15", "2022-09-01", "2022-09-15",
    "2022-10-01", "2022-10-15", "2022-11-01", "2022-11-15"
  ),
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 1
)
df_spatial_tfs_2 <- estimate_spatial_tfs(
  df_covid = df_covid,
  days_vector = c(
    "2020-04-21", "2020-05-06", "2020-05-20",
    "2020-06-06", "2020-06-20", "2020-07-06", "2020-07-20",
    "2020-08-06", "2020-08-20", "2020-09-06", "2020-09-20",
    "2020-10-06", "2020-10-20", "2020-11-06", "2020-11-20",
    "2020-12-06", "2020-12-20", "2021-01-06", "2021-01-20",
    "2021-02-06", "2021-02-20", "2021-03-06", "2021-03-20",
    "2021-04-06", "2021-04-20", "2021-05-06", "2021-05-20",
    "2021-06-06", "2021-06-20", "2021-07-06", "2021-07-20",
    "2021-08-06", "2021-08-20", "2021-09-06", "2021-09-20",
    "2021-10-06", "2021-10-20", "2021-11-06", "2021-11-20",
    "2021-12-06", "2021-12-20", "2022-01-06", "2022-01-20",
    "2022-02-06", "2022-02-20", "2022-03-06", "2022-03-20",
    "2022-04-06", "2022-04-20", "2022-05-06", "2022-05-20",
    "2022-06-06", "2022-06-20", "2022-07-06", "2022-07-20",
    "2022-08-06", "2022-08-20", "2022-09-06", "2022-09-20",
    "2022-10-06", "2022-10-20", "2022-11-06", "2022-11-20"
  ),
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 2
)
df_spatial_tfs_3 <- estimate_spatial_tfs(
  df_covid = df_covid,
  days_vector = c(
    "2020-04-26", "2020-05-11", "2020-05-25",
    "2020-06-11", "2020-06-25", "2020-07-11", "2020-07-25",
    "2020-08-11", "2020-08-25", "2020-09-11", "2020-09-25",
    "2020-10-11", "2020-10-25", "2020-11-11", "2020-11-25",
    "2020-12-11", "2020-12-25", "2021-01-11", "2021-01-25",
    "2021-02-11", "2021-02-25", "2021-03-11", "2021-03-25",
    "2021-04-11", "2021-04-25", "2021-05-11", "2021-05-25",
    "2021-06-11", "2021-06-25", "2021-07-11", "2021-07-25",
    "2021-08-11", "2021-08-25", "2021-09-11", "2021-09-25",
    "2021-10-11", "2021-10-25", "2021-11-11", "2021-11-25",
    "2021-12-11", "2021-12-25", "2022-01-11", "2022-01-25",
    "2022-02-11", "2022-02-25", "2022-03-11", "2022-03-25",
    "2022-04-11", "2022-04-25", "2022-05-11", "2022-05-25",
    "2022-06-11", "2022-06-25", "2022-07-11", "2022-07-25",
    "2022-08-11", "2022-08-25", "2022-09-11", "2022-09-25",
    "2022-10-11", "2022-10-25"
  ),
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date,
  number = 3
)
df_spatial_tfs <- bind_rows(
  list(df_spatial_tfs_1, df_spatial_tfs_2, df_spatial_tfs_3)
)

plot_spatial_evolution(
  df_spatial_tfs = df_spatial_tfs,
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-04-16",
  final_date = "2022-11-01",
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Setup parallel backend to use many processors (Turn on cluster)
cluster <- makeCluster(detectCores() - 2)
registerDoParallel(cluster)

# Estimate Hurst exponent of cases and death in every region (unfixed windo)----
list_hurst_cases_rs <- estimate_hurst_exponent_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
list_hurst_deaths_rs <- estimate_hurst_exponent_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 60,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
list_hurst_cases_mfdfa <- estimate_hurst_exponent_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  hurst_method = "mfdfa",
  dfa_degree = 1,
  q_order = 2,
  n_step = 3,
  n_days_skipped = 28,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
list_hurst_deaths_mfdfa <- estimate_hurst_exponent_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 60,
  hurst_method = "mfdfa",
  dfa_degree = 1,
  q_order = 2,
  n_step = 3,
  n_days_skipped = 28,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)

df_hurst_excess_rs <- fread(
  paste0(
    input_path_processed,
    "/df_hurst_excess_rs_summary_",
    gsub("-", "", input_generation_date),
    ".csv"
  ),
  encoding = "UTF-8"
)
df_hurst_excess_mfdfa <- fread(
  paste0(
    input_path_processed,
    "/df_hurst_excess_mfdfa_summary_",
    gsub("-", "", input_generation_date),
    ".csv"
  ),
  encoding = "UTF-8"
)

df_hurst_covid_rs <- get_hurst_covid_resume(
  list_hurst_cases = list_hurst_cases_rs,
  list_hurst_deaths = list_hurst_deaths_rs,
  df_hurst_excess = df_hurst_excess_rs,
  days_treshold = 60,
  fixed_window = "no_fixed"
)
df_hurst_covid_mfdfa <- get_hurst_covid_resume(
  list_hurst_cases = list_hurst_cases_mfdfa,
  list_hurst_deaths = list_hurst_deaths_mfdfa,
  df_hurst_excess = df_hurst_excess_mfdfa,
  days_treshold = 60,
  fixed_window = "no_fixed"
)

plot_hurst_exponent_data(
  df_hurst_covid = df_hurst_covid_rs,
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
  initial_date = "2020-01-30",
  final_date = "2022-11-01",
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)
plot_hurst_exponent_data(
  df_hurst_covid = df_hurst_covid_mfdfa,
  hurst_method = "mfdfa",
  fixed_window = "no_fixed",
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Estimate Hurst exponent of cases and death in every region (fixed window) ----
list_hurst_cases_fixed <- estimate_hurst_exponent_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 90,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  n_days_skipped = 0,
  fixed_window = "fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
list_hurst_deaths_fixed <- estimate_hurst_exponent_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 90,
  hurst_method = "rs",
  dfa_degree = 1,
  q_order = 2,
  n_step = 1,
  n_days_skipped = 0,
  fixed_window = "fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_hurst_covid_fixed <- get_hurst_covid_resume(
  list_hurst_cases = list_hurst_cases_fixed,
  list_hurst_deaths = list_hurst_deaths_fixed,
  df_hurst_excess = df_hurst_excess_rs,
  days_treshold = 90,
  fixed_window = "fixed"
)
plot_hurst_exponent_data(
  df_hurst_covid = df_hurst_covid_fixed,
  hurst_method = "rs",
  fixed_window = "fixed",
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)
plot_hurst_overlapping(
  df_hurst_covid = df_hurst_covid_fixed,
  hurst_method = "rs",
  fixed_window = "fixed",
  font_size = 18,
  axes_title_relative_size = 0.7,
  axes_relative_size = 0.6,
  legend_cols = 7,
  line_size = 1.1,
  date_breaks = "2 weeks",
  date_minor_breaks = "1 week",
  n_y_breaks = 25,
  initial_date = "2020-04-15",
  final_date = "2022-11-01",
  initial_y = 0,
  final_y = 1,
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400,
  verbose = 1
)

# Estimate Multiscaling exponents in every region (unfixed window) ----
list_multiscaling_exponents_cases <- estimate_multiscaling_exponents_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  dfa_degree = 1,
  q_order_vector = seq(from = -1.1, to = 2.1, by = 0.2),
  n_step = 3,
  n_days_skipped = 240,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
list_multiscaling_exponents_deaths <- estimate_multiscaling_exponents_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 60,
  dfa_degree = 1,
  q_order_vector = seq(from = -1.1, to = 2.1, by = 0.2),
  n_step = 3,
  n_days_skipped = 240,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_multiscaling_exponents_covid <- get_multiscaling_exponents_covid_resume(
  list_multiscaling_exponents_cases = list_multiscaling_exponents_cases,
  list_multiscaling_exponents_deaths = list_multiscaling_exponents_deaths
)
plot_multiscaling_overlapping(
  df_multiscaling_exponents_covid = df_multiscaling_exponents_covid,
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400,
  verbose = 1
)

# Estimate Entropy indexes in every region (unfixed window) ----
df_entropy_indexes_cases <- estimate_entropy_indexes_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  alpha = 2,
  epsilon = 1,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_entropy_indexes_deaths <- estimate_entropy_indexes_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 60,
  alpha = 2,
  epsilon = 1,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_theil_covid <- get_theil_covid_resume(
  df_theil_cases = df_entropy_indexes_cases,
  df_theil_deaths = df_entropy_indexes_deaths,
  days_treshold = 60,
  fixed_window = "no_fixed"
)
plot_entropy_index_data(
  df_theil_covid = df_theil_covid,
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Estimate Temporal Fluctuation Scaling in every region (unfixed window) ----
df_tfs_cases <- estimate_tfs_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 60,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_tfs_deaths <- estimate_tfs_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 60,
  n_days_skipped = 0,
  fixed_window = "no_fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_tfs_covid <- get_tfs_covid_resume(
  df_tfs_cases = df_tfs_cases,
  df_tfs_deaths = df_tfs_deaths,
  days_treshold = 60,
  fixed_window = "no_fixed"
)
plot_tfs_data(
  df_tfs_covid = df_tfs_covid,
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)
plot_tfs_data(
  df_tfs_covid = df_tfs_covid,
  variable_name = "tfs_exponent_value",
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Estimate Temporal Fluctuation Scaling in every region (fixed window) ----
df_tfs_cases_fixed <- estimate_tfs_covid(
  df_covid = df_covid,
  initial_date = "2020-01-01",
  final_date = Sys.Date(),
  variable_name = "cases",
  days_treshold = 90,
  n_days_skipped = 0,
  fixed_window = "fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_tfs_deaths_fixed <- estimate_tfs_covid(
  df_covid = df_covid,
  initial_date = "2020-03-01",
  final_date = Sys.Date(),
  variable_name = "deaths",
  days_treshold = 90,
  n_days_skipped = 0,
  fixed_window = "fixed",
  path = input_path,
  verbose = 1,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)
df_tfs_covid_fixed <- get_tfs_covid_resume(
  df_tfs_cases = df_tfs_cases_fixed,
  df_tfs_deaths = df_tfs_deaths_fixed,
  days_treshold = 90,
  fixed_window = "fixed"
)
plot_tfs_data(
  df_tfs_covid = df_tfs_covid_fixed,
  variable_name = "tfs_coefficient_value",
  fixed_window = "fixed",
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)
plot_tfs_data(
  df_tfs_covid = df_tfs_covid_fixed,
  variable_name = "tfs_exponent_value",
  fixed_window = "fixed",
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
  output_path = output_path,
  save_plots = TRUE,
  input_date = input_generation_date,
  plot_width = 6571,
  plot_height = 3563,
  dots_per_inch = 400
)

# Uninstall parallel back-end to use many processors (Turn off cluster) ----
stopCluster(cluster)

# Clear workspace and free used memory ----
rm(list = ls())
gc()
