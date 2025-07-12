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
  key <- paste0(reporter, ".", indicator, "..", freq)
  
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

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# IMTS Imports ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "MG_CIF_USD"
freq <- "A"
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
  key <- paste0(reporter, ".", indicator, "..", freq)
  
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

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# DIP Inward ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "INWD_D_NETLA_FALL_ALL"
freq <- "A"
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
  key <- paste0(reporter, "..", indicator, "..", freq)
  
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

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)


# DIP Outward ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "OTWD_D_NETAL_FALL_ALL"
freq <- "A"
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
  key <- paste0(reporter, "..", indicator, "..", freq)
  
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

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)

# PIP Outward ------

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "OTWD_D_NETAL_FALL_ALL"
freq <- "A"
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
  key <- paste0(reporter, "..", indicator, "..", freq)
  
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

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)
