#CUNTO SANTANA (20XX)

#2.0 DOWNLOAD DATA ------

# IMTS

# Parameters
reporters <- na.omit(unique(country_set$sdmx_code))
indicator <- "XG_FOB_USD"
freq <- "A"
start_year <- 1990
end_year <- 2023

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
  
  setTxtProgressBar(pb, i)
}

close(pb)

# Summary
cat("\nDownload complete.\n")
cat("Countries left out:", length(countries_leftout), "\n")
print(countries_leftout)