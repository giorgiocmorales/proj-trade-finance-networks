#CUNTO SANTANA (20XX)

#2.0 DOWNLOAD DATA ------

# US GDP deflator ------

# Parameters
flowRef <- "WEO"
ref_area <- "USA"
indicator <- "NGDP_D"  # Nominal GDP deflator index
freq <- "A"
start_year <- 1977
end_year <- 2024

# Build SDMX key
key <- paste0(ref_area, ".", indicator, ".", freq)

# Download
sdmx_raw <- readSDMX(
  providerId = "IMF_DATA",
  resource = "data",
  flowRef = flowRef,
  key = key,
  start = start_year,
  end = end_year
)

# Clean and normalize
usa_gdp_deflator <- as.data.frame(sdmx_raw) %>%
  mutate(
    year = as.numeric(TIME_PERIOD),
    value = as.numeric(OBS_VALUE),
    deflator = value / value[year == end_year] * 100
  ) %>%
  select(year, value, deflator)

# Preview
print(head(usa_gdp_deflator))
rm(sdmx_raw)
gc()

# IMTS Exports ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "XG_FOB_USD"
freq <- "A"
counterpart <- ""
start_year <- 1990
end_year <- 2024

# Initialize
imts_exports_base <- tibble()
countries_leftout <- c()
total_countries <- length(reporters)

# Progress bar
pb <- txtProgressBar(min = 0, max = total_countries, style = 3)

# Loop
for (i in seq_along(reporters)) {
  reporter <- reporters[i]
  key <- paste(c(reporter, indicator, counterpart , freq), collapse = ".")
  
  tryCatch({
    sdmx_raw <- readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = "IMTS",
      key = key,
      start = start_year,
      end = end_year
    )
    
    df <- as.data.frame(sdmx_raw)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          SCALE = as.numeric(SCALE),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      imts_exports_base <- bind_rows(imts_exports_base, df)
    }
  }, error = function(e) {
    warning(sprintf("Failed to retrieve: %s — %s", reporter, e$message))
    countries_leftout <- c(countries_leftout, reporter)
  })
  
  # Cleanup
  rm(sdmx_raw, df)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)
gc()

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# IMTS Imports ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "MG_CIF_USD"
freq <- "A"
counterpart <- ""
start_year <- 1990
end_year <- 2024

# Initialize
imts_imports_base <- tibble()
countries_leftout <- c()
total_countries <- length(reporters)

# Progress bar
pb <- txtProgressBar(min = 0, max = total_countries, style = 3)

# Loop
for (i in seq_along(reporters)) {
  reporter <- reporters[i]
  key <- paste(c(reporter, indicator, counterpart , freq), collapse = ".")
  
  tryCatch({
    sdmx_raw <- readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = "IMTS",
      key = key,
      start = start_year,
      end = end_year
    )
    
    df <- as.data.frame(sdmx_raw)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          SCALE = as.numeric(SCALE),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      imts_imports_base <- bind_rows(imts_imports_base, df)
    }
  }, error = function(e) {
    warning(sprintf("Failed to retrieve: %s — %s", reporter, e$message))
    countries_leftout <- c(countries_leftout, reporter)
  })
  
  # Cleanup
  rm(sdmx_raw, df)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)
gc()

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# DIP Inward ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "INWD_D_NETLA_FALL_ALL"
freq <- "A"
derived <- ""
counterpart <- ""
start_year <- 1990
end_year <- 2024

# Initialize
dip_inwd_base <- tibble()
countries_leftout <- c()
total_countries <- length(reporters)

# Progress bar
pb <- txtProgressBar(min = 0, max = total_countries, style = 3)

# Loop
for (i in seq_along(reporters)) {
  reporter <- reporters[i]
  key <- paste(c(reporter, derived, indicator, counterpart, freq), collapse = ".")
  
  tryCatch({
    sdmx_raw <- readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = "DIP",
      key = key,
      start = start_year,
      end = end_year
    )
    
    df <- as.data.frame(sdmx_raw)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          SCALE = as.numeric(SCALE),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      dip_inwd_base <- bind_rows(dip_inwd_base, df)
    }
  }, error = function(e) {
    warning(sprintf("Failed to retrieve: %s — %s", reporter, e$message))
    countries_leftout <- c(countries_leftout, reporter)
  })
  
  # Cleanup
  rm(sdmx_raw, df)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)
gc()

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)


# DIP Outward ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "OTWD_D_NETAL_FALL_ALL"
freq <- "A"
derived <- ""
counterpart <- ""
start_year <- 1990
end_year <- 2024

# Initialize
dip_otwd_base <- tibble()
countries_leftout <- c()
total_countries <- length(reporters)

# Progress bar
pb <- txtProgressBar(min = 0, max = total_countries, style = 3)

# Loop
for (i in seq_along(reporters)) {
  reporter <- reporters[i]
  key <- paste(c(reporter, derived, indicator, counterpart, freq), collapse = ".")
  
  tryCatch({
    sdmx_raw <- readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = "DIP",
      key = key,
      start = start_year,
      end = end_year
    )
    
    df <- as.data.frame(sdmx_raw)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          SCALE = as.numeric(SCALE),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      dip_otwd_base <- bind_rows(dip_otwd_base, df)
    }
  }, error = function(e) {
    warning(sprintf("Failed to retrieve: %s — %s", reporter, e$message))
    countries_leftout <- c(countries_leftout, reporter)
  })
  
  # Cleanup
  rm(sdmx_raw, df)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)
gc()

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# PIP Assets ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "P_TOTINV_P_USD"
sector <- "S1"
sector_counterpart <- "S1"
freq <- "A"
counterpart <- ""
accounting <- ""
start_year <- 1990
end_year <- 2024

# Initialize
pip_asset_base <- tibble()
countries_leftout <- c()
total_countries <- length(reporters)

# Progress bar
pb <- txtProgressBar(min = 0, max = total_countries, style = 3)

# Loop
for (i in seq_along(reporters)) {
  reporter <- reporters[i]
  key <- paste(c(reporter, accounting, indicator, sector, sector_counterpart, counterpart, freq), collapse = ".")
  
  tryCatch({
    sdmx_raw <- readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = "PIP",
      key = key,
      start = start_year,
      end = end_year
    )
    
    df <- as.data.frame(sdmx_raw)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          SCALE = as.numeric(SCALE),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      pip_asset_base <- bind_rows(pip_asset_base, df)
    }
  }, error = function(e) {
    warning(sprintf("Failed to retrieve: %s — %s", reporter, e$message))
    countries_leftout <- c(countries_leftout, reporter)
  })
  
  # Cleanup
  rm(sdmx_raw, df)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)
gc()


# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# PIP Liabilities ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "P_TOTINV_P_SCC_USD"
sector <- "S1"
sector_counterpart <- "S1"
freq <- "A"
counterpart <- ""
accounting <- ""
start_year <- 1990
end_year <- 2024

# Initialize
pip_liabilities_base <- tibble()
countries_leftout <- c()
total_countries <- length(reporters)

# Progress bar
pb <- txtProgressBar(min = 0, max = total_countries, style = 3)

# Loop
for (i in seq_along(reporters)) {
  reporter <- reporters[i]
  key <- paste(c(reporter, accounting, indicator, sector, sector_counterpart, counterpart, freq), collapse = ".")
  
  tryCatch({
    sdmx_raw <- readSDMX(
      providerId = "IMF_DATA",
      resource = "data",
      flowRef = "PIP",
      key = key,
      start = start_year,
      end = end_year
    )
    
    df <- as.data.frame(sdmx_raw)
    
    if (nrow(df) > 0) {
      df <- df %>%
        mutate(
          SCALE = as.numeric(SCALE),
          OBS_VALUE = as.numeric(OBS_VALUE)
        )
      pip_liabilities_base <- bind_rows(pip_liabilities_base, df)
    }
  }, error = function(e) {
    warning(sprintf("Failed to retrieve: %s — %s", reporter, e$message))
    countries_leftout <- c(countries_leftout, reporter)
  })
  
  # Cleanup
  rm(sdmx_raw, df)
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)
gc()


# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# Clean up
rm(imts_freq_values, imts_indicator_values)
rm(dip_direction_values, dip_entity_values, dip_freq_values, dip_type_values, dip_indicator_values)
rm(pip_ac_entry_values, pip_acconting_values, pip_counterpart_sector_values, pip_currency_den_values, pip_currency_values, pip_freq_values, pip_indicator_values, pip_instrument_values, pip_reporting_sector_values)

# BIS LBS -----

# Set timeout to 10 minutes for large files
options(timeout = 600)

#Download Locational Banking Statistics
lbs_base <- fetch_dataset(dest.dir = tempdir(),
                          bis.url = "https://data.bis.org/static/bulk/",
                          dataset = "WS_LBS_D_PUB_csv_col.zip",
                          exdir = tempdir(),
                          header = TRUE,
                          sep = ',',
                          stringsAsFactors = FALSE,
                          check.names = FALSE,
                          na.strings = "",
                          quote = "\"",
                          fill = TRUE)

# Release memory
gc()

# Tibble of parent country
lbs_parent_country <- lbs_base %>%
  distinct(L_PARENT_CTY, `Parent country`) %>%
  as_tibble() %>%
  rename(country_code = L_PARENT_CTY, country_name = `Parent country`)

# Check not in country_set
lbs_parent_country %>% 
  filter(!(country_code %in% country_set$iso2c)) %>%
  print(n = nrow(.))

# Tibble of reporting countries 
lbs_reporting_country <- lbs_base %>%
  distinct(L_REP_CTY, `Reporting country`) %>%
  as_tibble() %>%
  rename(country_code = L_REP_CTY, country_name = `Reporting country`)

# Check not in country_set
lbs_reporting_country %>% 
  filter(!(country_code %in% country_set$iso2c)) %>%
  print(n = nrow(.))

# Tibble of counterpart countries
lbs_counterpart_country <- lbs_base %>%
  distinct(L_CP_COUNTRY, `Counterparty country`) %>%
  as_tibble() %>%
  rename(country_code = L_CP_COUNTRY, country_name = `Counterparty country`)

# Check not in country_set
lbs_counterpart_country %>% 
  filter(!(country_code %in% country_set$iso2c)) %>%
  print(n = nrow(.))

# Filter for processing 
lbs_base_filtered <- tibble()

# Filter for processing 
lbs_base_filtered <- lbs_base %>%
  # Fix Curacaso
  mutate(L_PARENT_CTY = case_when(L_PARENT_CTY == "CW" ~ "1C_355", TRUE ~ L_PARENT_CTY),
         L_REP_CTY = case_when(L_REP_CTY == "CW" ~ "1C_355", TRUE ~ L_REP_CTY),
         L_CP_COUNTRY = case_when(L_CP_COUNTRY == "CW" ~ "1C_355", TRUE ~ L_CP_COUNTRY)) %>%
  # Only counterpart countries in country set
  filter(L_CP_COUNTRY %in% c(country_set$iso2c),
         # Only reporting countries in country set
         L_REP_CTY %in% c(country_set$iso2c),
         # Parent country all
         L_PARENT_CTY == "5J",
         # Ammounts outstanding/stocks
         L_MEASURE == "S",
         # All instruments
         L_INSTR == "A",
         # All currencies
         L_DENOM == "TO1",
         # All currencies
         L_CURR_TYPE == "A",
         # All types of institutions
         L_REP_BANK_TYPE == "A",
         # All counterpart sectors
         L_CP_SECTOR == "A",
         # Cross border positions
         L_POS_TYPE == "N")

# Clean up
rm(lbs_counterpart_country, lbs_parent_country, lbs_reporting_country)

# Pivot
lbs_base_filtered <- lbs_base_filtered  %>%
  pivot_longer(cols = -c(1:31),
               names_to = "date",
               values_to = "position")

# Filter by year
lbs_base_filtered <- lbs_base_filtered %>%
  mutate(year = as.numeric(substr(date, 1, 4)),  # Extracts the first four characters as the year
         quarter = substr(date, 6, 8)) %>%  # Extracts the last part as the quarter
  filter(quarter == "Q4", #Filter to retain only entries from the fourth quarter
         year <= end_year) #Filter to only get data up until final year

# Split
lbs_claims_base <- tibble()

lbs_claims_base <- lbs_base_filtered %>%
  filter(L_POSITION == "C")

lbs_liabilities_base <- tibble()

lbs_liabilities_base <- lbs_base_filtered %>%
  filter(L_POSITION == "L")

# Clean up
rm(lbs_base_filtered)

# COMBINED DATASET  -----
dataset_base <- bind_rows(
  
  # IMTS Exports
  imts_exports_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = COUNTRY,
      counterpart_iso3 = COUNTERPART_COUNTRY,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "flow",
      dv_type = DERIVATION_TYPE,
      source = "IMTS Exports"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # IMTS Imports
  imts_imports_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = COUNTRY,
      counterpart_iso3 = COUNTERPART_COUNTRY,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "flow",
      dv_type = DERIVATION_TYPE,
      source = "IMTS Imports"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # DIP Inward
  dip_inwd_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = COUNTRY,
      counterpart_iso3 = COUNTERPART_COUNTRY,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DV_TYPE,
      source = "DIP Inward"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # DIP Outward
  dip_otwd_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = COUNTRY,
      counterpart_iso3 = COUNTERPART_COUNTRY,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DV_TYPE,
      source = "DIP Outward"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # PIP Assets
  pip_asset_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = COUNTRY,
      counterpart_iso3 = COUNTERPART_COUNTRY,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DERIVATION_TYPE,
      source = "PIP Assets"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # PIP Liabilities
  pip_liabilities_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = COUNTRY,
      counterpart_iso3 = COUNTERPART_COUNTRY,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DERIVATION_TYPE,
      source = "PIP Liabilities"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # LBS Claims
  lbs_claims_base %>%
    left_join(usa_gdp_deflator, by = "year") %>%
    mutate(
      jurisdiction_iso3 = L_REP_CTY,
      counterpart_iso3 = L_CP_COUNTRY,
      value = position,
      value_adj = value,
      value_adj_cons = value_adj / deflator * 100,
      indicator = "LBS_CLAIMS",
      obs_status = NA_character_,
      value_type = "position",
      dv_type = NA_character_,
      source = "LBS Claims"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # LBS Liabilities
  lbs_liabilities_base %>%
    left_join(usa_gdp_deflator, by = "year") %>%
    mutate(
      jurisdiction_iso3 = L_REP_CTY,
      counterpart_iso3 = L_CP_COUNTRY,
      value = position,
      value_adj = value,
      value_adj_cons = value_adj / deflator * 100,
      indicator = "LBS_LIABILITIES",
      obs_status = NA_character_,
      value_type = "position",
      dv_type = NA_character_,
      source = "LBS Liabilities"
    ) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source)
)