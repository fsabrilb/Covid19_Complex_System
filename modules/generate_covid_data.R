# Libraries ----
library(dplyr)
library(readr)
library(rlist)
library(tidyr)
library(stringi)
library(data.table)

# Extract World cases and deaths per country ----
generate_raw_data_world <- function(
  input_path_raw = "./input_files/raw_data",
  url = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
  saved_data = FALSE
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_world.csv"))
  }
  
  # Africa: Saint Helena + 54 countries
  # Asia: Taiwan, Hong Kong, Macao + 48 countries
  # Europe: Jersey, European Union, Isle of Man, Faeroe Islands, 
  #         Gibraltar, Kosovo, Vatican
  # North America: Bermuda, Greenland, Bonaire Sint Eustatius and Saba
  # Oceania: New Caledonia, French Polynesia, Cook Islands, Niue, 
  #          Wallis and Futuna, Tokelau, Pitcairn
  # South America: Falkland Islands, Anguilla, Aruba, British Virgin Islands,
  #                Cayman Islands, Curacao, Montserrat,
  #                Sint Maarten (Dutch part), Turks and Caicos Islands
  # Other: International, World, Low Lower Upper High Income
  # Cyprus with two continent
  
  # Covid data generation
  df_covid <- fread(paste0(input_path_raw, "/data_world.csv")) %>%
    # Normalization of columns names
    rename(
      names = location,
      cases = new_cases,
      cum_cases = total_cases,
      deaths = new_deaths,
      cum_deaths = total_deaths
    ) %>%
    # Normalization of columns information
    mutate(
      date = as.Date(date),
      region = "World",
      subregion = "Country",
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      cum_cases = if_else(is.na(cum_cases), 0, as.numeric(cum_cases)),
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths)),
      cum_deaths = if_else(is.na(cum_deaths), 0, as.numeric(cum_deaths)),
    ) %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Filtering process (no new information per continent)
    filter(
      !names %in% c(
        "Africa",
        #"Saint Helena",
        "Asia",
        #"Taiwan",
        #"Hong Kong",
        #"Macao",
        "Europe",
        "European Union",
        #"Jersey",
        #"Isle of Man",
        #"Faeroe Islands",
        #"Gibraltar",
        #"Kosovo",
        #"Vatican",
        "North America",
        #"Bermuda",
        #"Greenland",
        #"Bonaire Sint Eustatius and Saba",
        "Oceania",
        #"New Caledonia",
        #"French Polynesia",
        #"Cook Islands",
        #"Niue",
        #"Wallis and Futuna",
        #"Tokelau",
        #"Pitcairn",
        "South America",
        #"Anguilla",
        #"Aruba",
        #"British Virgin Islands",
        #"Falkland Islands",
        #"Cayman Islands",
        #"Curacao",
        #"Montserrat",
        #"Sint Maarten (Dutch part)",
        #"Turks and Caicos Islands",
        "World",
        "International",
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    ) %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract United States of America (USA) cases and deaths per state ----
generate_raw_data_usa <- function(
  input_path_raw = "./input_files/raw_data",
  url = "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD",
  saved_data = FALSE,
  input_path_data_dictionary = "./input_files/data_dictionary",
  states_dictionary_name = "usa_states.csv"
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_usa.csv"))
  }
  
  # Data dictionary loading
  states_dictionary <- fread(
    paste0(input_path_data_dictionary, "/", states_dictionary_name)
  )
  
  # Covid data generation
  df_covid <- fread(paste0(input_path_raw, "/data_usa.csv")) %>%
    # Allocation process (states names through ISO-2 code)
    left_join(
      states_dictionary,
      by = "state"
    ) %>%
    # Normalization of columns names
    rename(
      date = submission_date,
      names = state_name,
      cum_cases = tot_cases,
      cum_deaths = tot_death,
    ) %>%
    # Normalization of columns information
    mutate(
      date = as.Date(date, format = '%m/%d/%Y'),
      region = "USA",
      subregion = "State",
      cum_cases = if_else(is.na(cum_cases), 0, as.numeric(cum_cases)),
      cum_deaths = if_else(is.na(cum_deaths), 0, as.numeric(cum_deaths))
    ) %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Correct reconstruction of cases and deaths
    # We take c(diff(.), 0) for take previous day of reporting bad value
    group_by(names) %>%
    mutate(
      cases = c(diff(cum_cases), 0),
      deaths = c(diff(cum_deaths), 0),
    ) %>%
    # Filtering process (Reestimating cases and deaths)
    # We take c(0, diff(.)) for take present day of reporting
    filter(cases >= 0, deaths >= 0) %>%
    mutate(
      cases = c(0, diff(cum_cases)),
      deaths = c(0, diff(cum_deaths))
    ) %>%
    ungroup() %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract cases and deaths in one USA state per county ----
generate_raw_data_usa_states <- function(
  input_path_raw = "./input_files/raw_data",
  url_cases = paste0(
    "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usa",
    "facts.csv?_ga=2.172812709.72077348.1638840631-572244296.1638840631"
  ),
  url_deaths = paste0(
    "https://static.usafacts.org/public/data/covid-19/covid_deaths_usa",
    "facts.csv?_ga=2.210027667.72077348.1638840631-572244296.1638840631"
  ),
  saved_data = FALSE,
  input_path_data_dictionary = "./input_files/data_dictionary",
  states_dictionary_name = "usa_states.csv",
  filtered_state_name
) {
  # Download Covid data
  if(saved_data == FALSE) {
    # Cases per county
    download.file(
      url_cases,
      destfile = paste0(input_path_raw, "/data_usa_states_cases.csv")
    )
    
    # Deaths per county
    download.file(
      url_deaths,
      destfile = paste0(input_path_raw, "/data_usa_states_deaths.csv")
    )
  }
  
  # Data dictionary loading
  states_dictionary <- fread(
    paste0(input_path_data_dictionary, "/", states_dictionary_name)
  )
  
  # Covid data generation (USA has between 3006 and 3243 counties (Cases))
  df_cases <- fread(paste0(input_path_raw, "/data_usa_states_cases.csv")) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("County Name", "State", "countyFIPS", "StateFIPS")),
      function(x) {return(c("names", "state", "county_f", "state_f"))}
    ) %>%
    # Allocation process (states names through ISO-2 code)
    left_join(states_dictionary, by = "state") %>%
    filter(
      names != "Statewide Unallocated",
      state_name == filtered_state_name
    ) %>%
    # Normalization of columns information (Structuring data)
    select(-c(state, county_f, state_f)) %>%
    pivot_longer(
      !all_of(c("state_name", "names")),
      names_to = "date",
      values_to = "cum_cases"
    ) %>%
    # Normalization of columns information (Transforming data)
    mutate(
      date = as.Date(date),
      region = filtered_state_name,
      subregion = "County",
      cases = if_else(is.na(cum_cases), 0, as.numeric(cum_cases))
    )
  
  # Covid data generation (USA has between 3006 and 3243 counties (Deaths))
  df_deaths <- fread(paste0(input_path_raw, "/data_usa_states_deaths.csv")) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("County Name", "State", "countyFIPS", "StateFIPS")),
      function(x) {return(c("names", "state", "county_f", "state_f"))}
    ) %>%
    # Allocation process (states names through ISO-2 code)
    left_join(states_dictionary, by = "state") %>%
    filter(
      names != "Statewide Unallocated",
      state_name == filtered_state_name
    ) %>%
    # Normalization of columns information (Structuring data)
    select(-c(state, county_f, state_f)) %>%
    pivot_longer(
      !all_of(c("state_name", "names")),
      names_to = "date",
      values_to = "cum_deaths"
    ) %>%
    # Normalization of columns information (Transforming data)
    mutate(
      date = as.Date(date),
      region = filtered_state_name,
      subregion = "County",
      deaths = if_else(is.na(cum_deaths), 0, as.numeric(cum_deaths))
    )
  
  # Cross cases and deaths per county for the given state
  df_covid <- df_cases %>%
    # full_join: In some dates, we have deaths but no cases
    full_join(df_deaths, by = c("date", "region", "subregion", "names")) %>%
    mutate(
      cum_cases = if_else(is.na(cum_cases), 0, as.numeric(cum_cases)),
      cum_deaths = if_else(is.na(cum_deaths), 0, as.numeric(cum_deaths)),
    ) %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Correct reconstruction of cases and deaths
    # We take c(diff(.), 0) for take previous day of reporting bad value
    group_by(names) %>%
    mutate(
      cases = c(diff(cum_cases), 0),
      deaths = c(diff(cum_deaths), 0),
    ) %>%
    # Filtering process (Reestimating cases and deaths)
    # We take c(0, diff(.)) for take present day of reporting
    filter(cases >= 0, deaths >= 0) %>%
    mutate(
      cases = c(0, diff(cum_cases)),
      deaths = c(0, diff(cum_deaths))
    ) %>%
    ungroup() %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract Brasil cases and deaths per state ----
generate_raw_data_brasil <- function(
  input_path_raw = "./input_files/raw_data",
  url = paste0(
    "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil",
    "-states.csv"
  ),
  saved_data = FALSE,
  input_path_data_dictionary = "./input_files/data_dictionary",
  states_dictionary_name = "brasil_states.csv"
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_brasil.csv"))
  }
  
  # Data dictionary loading
  states_dictionary <- fread(
    paste0(input_path_data_dictionary, "/", states_dictionary_name)
  )
  
  # Covid data generation
  df_covid <- fread(paste0(input_path_raw, "/data_brasil.csv")) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("newDeaths", "deaths", "newCases", "totalCases")),
      function(x) {return(c("deaths", "cum_deaths", "cases", "cum_cases"))}
    ) %>%
    # Allocation process (states names through ISO-2 code)
    left_join(
      states_dictionary,
      by = "state"
    ) %>%
    rename(names = state_name) %>%
    # Normalization of columns information
    mutate(
      date = as.Date(date),
      region = "Brasil",
      subregion = "State",
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      cum_cases = if_else(is.na(cum_cases), 0, as.numeric(cum_cases)),
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths)),
      cum_deaths = if_else(is.na(cum_deaths), 0, as.numeric(cum_deaths))
    ) %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Filtering process (Exclude total data in country)
    filter(state != "TOTAL") %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract Europe cases and deaths per country ----
generate_raw_data_europe <- function(
  input_path_raw = "./input_files/raw_data",
  url = paste0(
    "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_",
    "ei/csv/data.csv"
  ),
  saved_data = FALSE
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_europe.csv"))
  }
  
  # Covid data generation
  df_covid <- fread(paste0(input_path_raw, "/data_europe.csv")) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("countriesAndTerritories")),
      function(x) {return(c("names"))}
    ) %>%
    # Normalization of columns information
    mutate(
      date = as.Date(paste0(year, "-", month, "-", day)),
      region = "Europe",
      subregion = "Country",
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths))
    ) %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Correct reconstruction of cases and deaths
    group_by(names) %>%
    mutate(
      cum_cases = cumsum(cases),
      cum_deaths = cumsum(deaths)
    ) %>%
    ungroup() %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract Spain cases and deaths per autonomous community or province ----
generate_raw_data_spain <- function(
  input_path_raw = "./input_files/raw_data",
  url = paste0(
    "https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_",
    "edad_provres.csv"
  ),
  saved_data = FALSE,
  input_path_data_dictionary = "./input_files/data_dictionary",
  provinces_dictionary_name = "spain_provinces.csv"
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_spain.csv"))
  }
  
  # Data dictionary loading
  provinces_dictionary <- fread(
    paste0(input_path_data_dictionary, "/", provinces_dictionary_name)
  )
  
  # Covid data generation
  df_covid <- fread(paste0(input_path_raw, "/data_spain.csv")) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("provincia_iso", "fecha", "num_casos", "num_def")),
      function(x) {return(c("province", "date", "cases", "deaths"))}
    ) %>%
    # Allocation process (states names through ISO-2 code)
    left_join(
      provinces_dictionary,
      by = "province"
    ) %>%
    rename(names = province_name) %>%
    # Normalization of columns information (Transforming)
    mutate(
      date = as.Date(date),
      region = "Spain",
      subregion = "Province",
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths))
    ) %>%
    # Normalization of columns information (Structuring)
    group_by(names, date, region, subregion) %>%
    summarise(
      cases = sum(cases, na.rm = TRUE),
      deaths = sum(deaths, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Correct reconstruction of cases and deaths
    group_by(names) %>%
    mutate(
      cum_cases = cumsum(cases),
      cum_deaths = cumsum(deaths)
    ) %>%
    ungroup() %>%
    # Filtering process (Exclude NA provinces)
    filter(!is.na(names)) %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract India cases and deaths per state ----
generate_raw_data_india <- function(
  input_path_raw = "./input_files/raw_data",
  url = "https://prsindia.org/covid-19/cases/download",
  saved_data = FALSE
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_india.csv"))
  }
  
  # Covid data generation
  df_covid <- fread(paste0(input_path_raw, "/data_india.csv")) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("Date", "Region", "Confirmed Cases", "Death")),
      function(x) {return(c("date", "names", "cum_cases", "cum_deaths"))}
    ) %>%
    # Normalization of columns information
    mutate(
      date = as.Date(date, format = '%d/%m/%Y'),
      region = "India",
      subregion = "State",
      cum_cases = if_else(is.na(cum_cases), 0, as.numeric(cum_cases)),
      cum_deaths = if_else(is.na(cum_deaths), 0, as.numeric(cum_deaths))
    ) %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Filtering process (Exclude atypical information)
    filter(
      date >= as.Date("2020-01-01"),
      !names %in% c("India", "World", "State assignment pending")
    ) %>%
    # Correct reconstruction of cases and deaths
    # We take c(diff(.), 0) for take previous day of reporting bad value
    group_by(names) %>%
    mutate(
      cases = c(diff(cum_cases), 0),
      deaths = c(diff(cum_deaths), 0),
    ) %>%
    # Filtering process (Reestimating cases and deaths)
    # We take c(0, diff(.)) for take present day of reporting
    filter(cases >= 0, deaths >= 0) %>%
    mutate(
      cases = c(0, diff(cum_cases)),
      deaths = c(0, diff(cum_deaths))
    ) %>%
    ungroup() %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Extract India cases and deaths per state ----
generate_raw_data_colombia <- function(
  input_path_raw = "./input_files/raw_data",
  url = paste0(
    "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=",
    "DOWNLOAD"
  ),
  saved_data = FALSE
) {
  # Download Covid data
  if(saved_data == FALSE) {
    download.file(url, destfile = paste0(input_path_raw, "/data_colombia.csv"))
  }
  
  # Covid data generation (Cases)
  df_cases <- fread(
    paste0(input_path_raw, "/data_colombia.csv"),
    encoding = "UTF-8" 
  ) %>%
    # Normalization of columns names
    rename_at(
      all_of(c("Fecha de notificación", "Nombre municipio")),
      function(x) {return(c("date", "names"))}
    ) %>%
    # Normalization of columns information
    count(date, names, name = "cases") %>%
    mutate(
      date = as.Date(date),
      names = stri_trans_totitle(names),
      region = "Colombia",
      subregion = "Municipalities",
      cases = if_else(is.na(cases), 0, as.numeric(cases))
    )
  
  # Covid data generation (Deaths)
  df_deaths <- fread(
    paste0(input_path_raw, "/data_colombia.csv"),
    encoding = "UTF-8"
  ) %>%
    # Filtering process (Select deaths)
    filter(Estado == "Fallecido") %>%
    # Normalization of columns names
    rename_at(
      all_of(c("Fecha de muerte", "Nombre municipio")),
      function(x) {return(c("date", "names"))}
    ) %>%
    # Normalization of columns information
    count(date, names, name = "deaths") %>%
    mutate(
      date = as.Date(date),
      names = stri_trans_totitle(names),
      region = "Colombia",
      subregion = "Municipalities",
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths))
    )
  
  # Cross cases and deaths per municipality in Colombia
  df_covid <- df_cases %>%
    # full_join: In some dates, we have deaths but no cases
    full_join(df_deaths, by = c("date", "region", "subregion", "names")) %>%
    mutate(
      cases = if_else(is.na(cases), 0, as.numeric(cases)),
      deaths = if_else(is.na(deaths), 0, as.numeric(deaths)),
    ) %>%
    # Correct reconstruction of cases and deaths
    group_by(names) %>%
    mutate(
      cum_cases = cumsum(cases),
      cum_deaths = cumsum(deaths)
    ) %>%
    ungroup() %>%
    # Arrange per subregion and date
    arrange(names, date) %>%
    # Selection of cases and deaths information per date
    select(
      date,
      region,
      subregion,
      names,
      cases,
      cum_cases,
      deaths,
      cum_deaths
    )
  
  return(df_covid)
}

# Merge all data in global Dataframe ----
generate_all_covid_data <- function(
  input_path_raw = "./input_files/raw_data",
  url_world = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
  url_usa = 
    "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD",
  url_usa_county_cases = paste0(
    "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usa",
    "facts.csv?_ga=2.172812709.72077348.1638840631-572244296.1638840631"
  ),
  url_usa_county_deaths = paste0(
    "https://static.usafacts.org/public/data/covid-19/covid_deaths_usa",
    "facts.csv?_ga=2.210027667.72077348.1638840631-572244296.1638840631"
  ),
  url_brasil = paste0(
    "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil",
    "-states.csv"
  ),
  url_europe = paste0(
    "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_",
    "ei/csv/data.csv"
  ),
  url_spain = paste0(
    "https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_",
    "edad_provres.csv"
  ),
  url_india = "https://prsindia.org/covid-19/cases/download",
  url_colombia = paste0(
    "https://www.datos.gov.co/api/views/gt2j-8ykr/rows.csv?accessType=",
    "DOWNLOAD"
  ),
  saved_world_data = FALSE,
  saved_usa_data = FALSE,
  saved_usa_county_data = FALSE,
  saved_brasil_data = FALSE,
  saved_europe_data = FALSE,
  saved_spain_data = FALSE,
  saved_india_data = FALSE,
  saved_colombia_data = FALSE,
  input_path_data_dictionary = "./input_files/data_dictionary",
  usa_states_dictionary_name = "usa_states.csv",
  brasil_states_dictionary_name = "brasil_states.csv",
  spain_provinces_dictionary_name = "spain_provinces.csv",
  leaked_usa_state_names = c(
    "Georgia",
    "Illinois",
    "Kansas",
    "Kentucky",
    "Missouri",
    "North Carolina",
    "Texas",
    "Virginia"
  ),
  saved_all_data = FALSE,
  input_path_processed = "./input_files/processed_data",
  input_date = "2022-12-04",
  verbose = 1
) {
  if(saved_all_data == FALSE) {
    list_raw_data <- list(
      generate_raw_data_world(
        input_path_raw = input_path_raw,
        url = url_world,
        saved_data = saved_world_data
      ),
      generate_raw_data_usa(
        input_path_raw = input_path_raw,
        url = url_usa,
        saved_data = saved_usa_data,
        input_path_data_dictionary = input_path_data_dictionary,
        states_dictionary_name = usa_states_dictionary_name
      ),
      generate_raw_data_brasil(
        input_path_raw = input_path_raw,
        url = url_brasil,
        saved_data = saved_brasil_data,
        input_path_data_dictionary = input_path_data_dictionary,
        states_dictionary_name = brasil_states_dictionary_name
      ),
      generate_raw_data_europe(
        input_path_raw = input_path_raw,
        url = url_europe,
        saved_data = saved_europe_data
      ),
      generate_raw_data_spain(
        input_path_raw = input_path_raw,
        url = url_spain,
        saved_data = saved_spain_data,
        input_path_data_dictionary = input_path_data_dictionary,
        provinces_dictionary_name = spain_provinces_dictionary_name
      ),
      generate_raw_data_india(
        input_path_raw = input_path_raw,
        url = url_india,
        saved_data = saved_india_data
      ),
      generate_raw_data_colombia(
        input_path_raw = input_path_raw,
        url = url_colombia,
        saved_data = saved_colombia_data
      )
    )
    
    # Loop over different USA states
    for(i in leaked_usa_state_names) {
      list_raw_data <- list_raw_data %>% list.append(
        generate_raw_data_usa_states(
          input_path_raw = input_path_raw,
          url_cases = url_usa_county_cases,
          url_deaths = url_usa_county_deaths,
          saved_data = saved_usa_county_data,
          input_path_data_dictionary = input_path_data_dictionary,
          states_dictionary_name = usa_states_dictionary_name,
          filtered_state_name = i
        )
      )
      
      if(verbose >= 1) {
        cat(paste0("Finish process USA state: ", i, "\n"))
      }
    }
    
    # Generation of all raw data
    df_raw_data <- bind_rows(list_raw_data)
    
    # Save all Covid data in processed data for not reprocess the information
    write.csv(
      df_raw_data,
      paste0(
        input_path_processed,
        "/df_covid_data_",
        gsub("-", "", input_date),
        ".csv"
      ),
      fileEncoding = "UTF-8",
      row.names = FALSE
    )
    
  } else {
    # Load all Covid data from processed data for not reprocess the information
    df_raw_data <- fread(
      paste0(
        input_path_processed,
        "/df_covid_data_",
        gsub("-", "", input_date),
        ".csv"
      ),
      encoding = "UTF-8"
    ) %>%
      mutate(date = as.Date(date))
  }
  
  return(df_raw_data)
}
