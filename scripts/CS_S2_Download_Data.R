#CUNTO SANTANA (20XX)

# DOWNLOAD DATA -----

# 2.0 ------

# 2.1 US GDP deflator ------

# Parameters
flowRef <- "WEO"
ref_area <- "USA"
indicator <- "NGDP_D"  # Nominal GDP deflator index
freq <- "A"
start_year <- 1977
end_year <- 2024

# Build SDMX key
key <- paste(c(ref_area, indicator, freq), collapse = ".")

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
rm(ref_area)
gc()

# 2.1 IMTS Exports ------

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

# 2.2 IMTS Imports ------

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

# 2.3 DIP Inward ------

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


# 2.4 DIP Outward ------

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

# 2.5 PIP Assets ------

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

# 2.6 PIP Liabilities ------

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
rm(accounting, counterpart, countries_leftout, derived, flowRef, freq, i, indicator, key, reporter, reporters, sector, sector_counterpart)

# 2.7 BIS LBS -----

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

# 2.7.1 Split -----
lbs_claims_base <- tibble()

lbs_claims_base <- lbs_base_filtered %>%
  filter(L_POSITION == "C")

lbs_liabilities_base <- tibble()

lbs_liabilities_base <- lbs_base_filtered %>%
  filter(L_POSITION == "L")

# Clean up
rm(lbs_base_filtered)

# 2.8 Database Check -----

# 2.8.1 IMTS
bind_rows(
  imts_exports_base %>% mutate(source = "IMTS Exports"),
  imts_imports_base %>% mutate(source = "IMTS Imports")
) %>%
  group_by(source) %>%
  summarize(
    total_obs = n(),
    na_obs = sum(is.na(OBS_VALUE)),
    na_share = mean(is.na(OBS_VALUE)) * 100,
    zero_obs = sum(OBS_VALUE == 0, na.rm = TRUE),
    zero_share = mean(OBS_VALUE == 0, na.rm = TRUE) * 100,
    negative = sum(OBS_VALUE < 0, na.rm = TRUE),
    negative_share = mean(OBS_VALUE < 0, na.rm = TRUE) * 100,
    estimated = sum(STATUS == "e", na.rm = TRUE),
    estimatedl_share = mean(STATUS == "e", na.rm = TRUE) * 100
  )

# 2.8.2 DIP
bind_rows(
  dip_inwd_base %>% mutate(source = "DIP Inward"),
  dip_otwd_base %>% mutate(source = "DIP Outward")
) %>%
  group_by(source) %>%
  summarize(
    total_obs = n(),
    na_obs = sum(is.na(OBS_VALUE)),
    na_share = mean(is.na(OBS_VALUE)) * 100,
    zero_obs = sum(OBS_VALUE == 0, na.rm = TRUE),
    zero_share = mean(OBS_VALUE == 0, na.rm = TRUE) * 100,
    negative = sum(OBS_VALUE < 0, na.rm = TRUE),
    negative_share = mean(OBS_VALUE < 0, na.rm = TRUE) * 100,
    confidential = sum(STATUS == "C", na.rm = TRUE),
    confidential_share = mean(STATUS == "C", na.rm = TRUE) * 100
  )

# 2.8.3 PIP
bind_rows(
  pip_asset_base %>% mutate(source = "PIP Assets"),
  pip_liabilities_base %>% mutate(source = "PIP Liabilities")
) %>%
  group_by(source) %>%
  summarize(
    total_obs = n(),
    na_obs = sum(is.na(OBS_VALUE)),
    na_share = mean(is.na(OBS_VALUE)) * 100,
    zero_obs = sum(OBS_VALUE == 0, na.rm = TRUE),
    zero_share = mean(OBS_VALUE == 0, na.rm = TRUE) * 100,
    negative = sum(OBS_VALUE < 0, na.rm = TRUE),
    negative_share = mean(OBS_VALUE < 0, na.rm = TRUE) * 100,
    confidential = sum(STATUS == "C", na.rm = TRUE),
    confidential_share = mean(STATUS == "C", na.rm = TRUE) * 100
  )

# 2.8.4 LBS

bind_rows(
  lbs_claims_base %>% mutate(source = "LBS Claims"),
  lbs_liabilities_base %>% mutate(source = "LBS Liabilities")
) %>%
  group_by(source) %>%
  summarize(
    total_obs = n(),
    na_obs = sum(is.na(position) | is.nan(position)),
    na_share = mean(is.na(position) | is.nan(position)) * 100,
    zero_obs = sum(position == 0, na.rm = TRUE),
    zero_share = mean(position == 0, na.rm = TRUE) * 100,
    negative = sum(position < 0, na.rm = TRUE),
    negative_share = mean(position < 0, na.rm = TRUE) * 100
  )
  


# 2.9 COMBINED DATASET  -----
dataset_base <- bind_rows(
  
  # IMTS Exports
  imts_exports_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTRY" = "sdmx_code")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTERPART_COUNTRY" = "sdmx_code")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = OBS_VALUE,
      value_adj = value * 10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "flow",
      dv_type = DERIVATION_TYPE,
      source = "IMTS Exports"
    ) %>%
    filter(!is.na(value)) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # IMTS Imports
  imts_imports_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTRY" = "sdmx_code")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTERPART_COUNTRY" = "sdmx_code")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = OBS_VALUE,
      value_adj = value*10^SCALE,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "flow",
      dv_type = DERIVATION_TYPE,
      source = "IMTS Imports"
    ) %>%
    filter(!is.na(value)) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # DIP Inward
  dip_inwd_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTRY" = "sdmx_code")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTERPART_COUNTRY" = "sdmx_code")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = OBS_VALUE,
      value_adj = value,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DV_TYPE,
      source = "DIP Inward"
    ) %>%
    filter(!(is.na(value) & obs_status != "C")) %>%
    filter(!(value == 0 & obs_status != "C")) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # DIP Outward
  dip_otwd_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTRY" = "sdmx_code")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTERPART_COUNTRY" = "sdmx_code")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = OBS_VALUE,
      value_adj = value,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DV_TYPE,
      source = "DIP Outward"
    ) %>%
    filter(!(is.na(value) & obs_status != "C")) %>%
    filter(!(value == 0 & obs_status != "C")) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # PIP Assets
  pip_asset_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTRY" = "sdmx_code")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTERPART_COUNTRY" = "sdmx_code")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = OBS_VALUE,
      value_adj = value,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DERIVATION_TYPE,
      source = "PIP Assets"
    ) %>%
    filter(!(is.na(value) & obs_status != "C")) %>%
    filter(!(value == 0 & obs_status != "C")) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # PIP Liabilities
  pip_liabilities_base %>%
    mutate(TIME_PERIOD = as.numeric(TIME_PERIOD)) %>%
    left_join(usa_gdp_deflator, by = c("TIME_PERIOD" = "year")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTRY" = "sdmx_code")) %>%
    left_join(country_set %>% distinct(sdmx_code, .keep_all = TRUE) %>% select(iso3c, sdmx_code), by = c("COUNTERPART_COUNTRY" = "sdmx_code")) %>%
    mutate(
      year = TIME_PERIOD,
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = OBS_VALUE,
      value_adj = value,
      value_adj_cons = value_adj / deflator * 100,
      indicator = INDICATOR,
      obs_status = STATUS,
      value_type = "position",
      dv_type = DERIVATION_TYPE,
      source = "PIP Liabilities"
    ) %>%
    filter(!(is.na(value) & obs_status != "C")) %>%
    filter(!(value == 0 & obs_status != "C")) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # LBS Claims
  lbs_claims_base %>%
    left_join(usa_gdp_deflator, by = "year") %>%
    left_join(country_set %>% distinct(iso2c, .keep_all = TRUE) %>% select(iso3c, iso2c), by = c("L_REP_CTY" = "iso2c")) %>%
    left_join(country_set %>% distinct(iso2c, .keep_all = TRUE) %>% select(iso3c, iso2c), by = c("L_CP_COUNTRY" = "iso2c")) %>%
    mutate(
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = position,
      value_adj = value*10^6,
      value_adj_cons = value_adj / deflator * 100,
      indicator = "LBS_CLAIMS",
      obs_status = NA_character_,
      value_type = "position",
      dv_type = NA_character_,
      source = "LBS Claims"
    ) %>%
    filter(!(value == 0)) %>%
    filter(!(is.na(value) | is.nan(value))) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source),
  
  # LBS Liabilities
  lbs_liabilities_base %>%
    left_join(usa_gdp_deflator, by = "year") %>%
    left_join(country_set %>% distinct(iso2c, .keep_all = TRUE) %>% select(iso3c, iso2c), by = c("L_REP_CTY" = "iso2c")) %>%
    left_join(country_set %>% distinct(iso2c, .keep_all = TRUE) %>% select(iso3c, iso2c), by = c("L_CP_COUNTRY" = "iso2c")) %>%
    mutate(
      jurisdiction_iso3 = iso3c.x,
      counterpart_iso3 = iso3c.y,
      value = position,
      value_adj = value*10^6,
      value_adj_cons = value_adj / deflator * 100,
      indicator = "LBS_LIABILITIES",
      obs_status = NA_character_,
      value_type = "position",
      dv_type = NA_character_,
      source = "LBS Liabilities"
    ) %>%
    filter(!(value == 0)) %>%
    filter(!(is.na(value) | is.nan(value))) %>%
    select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons,
           obs_status, value_type, indicator, dv_type, source)
) %>%
  filter(jurisdiction_iso3 %in% country_set$iso3c, counterpart_iso3 %in% country_set$iso3c) %>% #Only countries within the country set.
  mutate(date = as.Date(paste0(year, "-12-31")))

gc()

# 2.10 Combined Summary ----

# NAs and confidential check
dataset_base %>%
  group_by(source) %>%
  summarize(
    total_obs = n(),
    na_obs = sum(is.na(value)),
    na_share = mean(is.na(value))*100,
    zero_obs = sum(value == 0, na.rm = TRUE),
    zero_share = mean(value == 0, na.rm = TRUE)*100,
    negative = sum(value < 0, na.rm = TRUE),
    negative_share = mean(value < 0, na.rm = TRUE)*100,
    confidential = sum(obs_status == "C", na.rm = TRUE),
    confidential_share = sum(obs_status == "C", na.rm = TRUE)/n()*100
  )

# Summary by source (value nominal)
summary_by_source_nom <- dataset_base %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj, na.rm = TRUE),
    Max = max(value_adj, na.rm = TRUE),
    Mean = mean(value_adj, na.rm = TRUE),
    Median = median(value_adj, na.rm = TRUE),
    `Std Dev` = sd(value_adj, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_nom)

# Summary by source (value cons)
summary_by_source_cons <- dataset_base %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj_cons, na.rm = TRUE),
    Max = max(value_adj_cons, na.rm = TRUE),
    Mean = mean(value_adj_cons, na.rm = TRUE),
    Median = median(value_adj_cons, na.rm = TRUE),
    `Std Dev` = sd(value_adj_cons, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_cons)

# CHECKPOINT, save environment and data sets
save.image(file = file.path(data_dir, "session_checkpoint_S2.RData"))
gc()
file.remove("data/session_checkpoint_S1.RData")

# Add date to databse
dataset_base <- dataset_base %>%
  mutate(date = as.Date(paste0(year, "-12-31")))

# Save CSV of consolidated datase 
write.csv(dataset_base, file.path(data_dir, "trade_finance_network_data.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Save CSV of consolidated data compressed to upload into GitHub
write.csv(dataset_base, gzfile(file.path(data_dir, "trade_finance_network_data.csv.gz")), row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Save country set
write.csv(country_set, file.path(data_dir, "country_set.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Clean up
rm(dip_inwd_base, dip_otwd_base, 
   imts_exports_base, imts_imports_base, 
   pip_asset_base, pip_liabilities_base, 
   lbs_base, lbs_claims_base, lbs_liabilities_base,
   pb)

# 2.11 countries by year and source-----
countries_by_year <-
  dataset_base %>%
  select(year, jurisdiction_iso3, counterpart_iso3, source) %>%
  pivot_longer(cols = c(jurisdiction_iso3, counterpart_iso3), names_to = "type", values_to = "country_iso3") %>%
  distinct(year, country_iso3, source) %>%
  group_by(year, source) %>%
  summarize(countries = list(unique(country_iso3)), .groups = 'drop') %>%
  mutate(n_countries = lengths(countries)) #Lengths of lists

# 2.11.2 Graph of available countries by year
countries_by_year %>%
  ggplot(aes(x = year, y = n_countries, color = source)) +
  geom_line(linewidth = 1) +
  # facet_wrap(~source, ncol = 4, scales = "fixed") +
  theme_minimal()

# 2.12 Common countries by year and source----
common_countries_by_year <- countries_by_year %>%
  filter(source %in% c("IMTS Exports", "DIP Outward", "PIP Assets", "LBS Liabilities")) %>% #Filter for restricting to a subset of datasets
  group_by(year) %>%
  filter(n() > 1) %>%
  reframe(common = list(reduce(countries, intersect))) %>%
  mutate(n_common = lengths(common))

gc()

# 2.12.1 Graph of available countries by year
common_countries_by_year %>%
  ggplot(aes(x = year, y = n_common)) +
  geom_line(linewidth = 1) +
  theme_minimal()

# 2.13 Filtered base -----
dataset_base_filtered <- dataset_base %>%
  left_join(common_countries_by_year, by = "year") %>%
  rowwise() %>%
  filter(jurisdiction_iso3 %in% common, counterpart_iso3 %in% common) %>%
  ungroup() %>%
  select(-common, -n_common)

gc()

# Summary filtered base

# Summary by source (value nominal)
summary_by_source_nom_fil <- dataset_base_filtered  %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj, na.rm = TRUE),
    Max = max(value_adj, na.rm = TRUE),
    Mean = mean(value_adj, na.rm = TRUE),
    Median = median(value_adj, na.rm = TRUE),
    `Std Dev` = sd(value_adj, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_nom_fil)

# Summary by source (value cons)
summary_by_source_cons_fil <- dataset_base_filtered  %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj_cons, na.rm = TRUE),
    Max = max(value_adj_cons, na.rm = TRUE),
    Mean = mean(value_adj_cons, na.rm = TRUE),
    Median = median(value_adj_cons, na.rm = TRUE),
    `Std Dev` = sd(value_adj_cons, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_by_source_cons_fil)