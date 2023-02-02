# ------------------------------ THEIL ANALYSIS ------------------------------ #
# Author: Felipe Segundo Abril Bermúdez
rm(list = ls())

# Local path of modules for deployment ----
setwd("/home/ASIS/Temp_Felipe")
source("./modules/estimate_theil_index.R")
source("./modules/summarize_theil_index_analysis.R")

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

# Generation of exponential paths for estimate optimal Theil index ----
df_exponential_sample <- generate_exponential_sample(
  n_simulation = 100,
  n_length = 1000,
  rate = 1
)

# Estimation of Theil index over many simulations of exponential samples ----
df_theil_exponential <- estimate_theil_exponential_sample(
  df_exponential_sample = df_exponential_sample,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0
)

# Estimation of excess of the Theil index in exponential samples ----
df_final_theil <- estimate_excess_theil_exponential_sample(
  n_simulation_vector = rep(100, 56),
  n_length_vector = c(
    60, 74, 91, 105, 121, 135, 152, 166, 182, 196, 213, 227, 244, 258,
    274, 288, 305, 319, 335, 349, 366, 380, 397, 411, 425, 439, 456, 470,
    486, 500, 517, 531, 547, 561, 578, 592, 609, 623, 639, 653, 670, 684,
    700, 714, 731, 745, 762, 776, 790, 804, 821, 835, 851, 865, 882, 896
  ),
  rate = 1,
  path = "/home/ASIS/Temp_Felipe",
  verbose = 0,
  saved_all_data = TRUE,
  input_path_processed = input_path_processed,
  input_date = input_generation_date
)

# Summarize of excess of the Theil index in exponential sample ----
df_theil_resume <- get_theil_resume(df_final_theil)

write.csv(
  df_theil_resume,
  paste0(
    input_path_processed,
    "/df_theil_excess_summary_",
    gsub("-", "", input_generation_date),
    ".csv"
  ),
  fileEncoding = "UTF-8",
  row.names = FALSE
)

# Uninstall parallel backend to use many processors (Turn off cluster) ----
stopCluster(cluster)

# Clear workspace and free used memory ----
rm(list = ls())
gc()
