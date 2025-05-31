#CUNTO SANTANA (20XX)

#1 IMPORT DATASETS

#Clear data
rm(list = ls())

#1.0 Prepare country selection-----

#Explore data for DOTS
dots_codes <- imf_parameters("DOT")
dots_indicators <- as_tibble(dots_codes[[3]])
dots_areas <- as_tibble(dots_codes[[2]])
rm(dots_codes)

##Explore data of CDIS
cdis_codes <- imf_parameters("CDIS")
cdis_indicators <- as_tibble(cdis_codes[[3]])
cdis_areas <- as_tibble(cdis_codes[[2]])
rm(cdis_codes)

##Explore data of CPIS
cpis_codes <- imf_parameters("CPIS")
cpis_indicators <- as_tibble(cpis_codes[[3]])
cpis_areas <- as_tibble(cpis_codes[[2]])
rm(cpis_codes)

##Create country list from WEO----

#Add FMI-WEO groups

advanced_economies <- c(
  # Europe
  "AUT", "BEL", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC",
  "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "PRT", "SVK", 
  "SVN", "ESP", "SWE", "CHE", "GBR",
  
  # North America
  "CAN", "USA",
  
  # Asia and Oceania
  "AUS", "HKG", "ISL", "ISR", "JPN", "KOR", "NZL", "SGP", "TWN",
  
  # Other
  "MAC", "SMR", "PRI"  # Including territories like Macao and Puerto Rico
)

# Emerging Asia by Subregion
emerging_asia <- c(
  # South Asia
  "BGD", "BTN", "IND", "MDV", "NPL", "LKA",
  
  # Southeast Asia and East Asia
  "BRN", "KHM", "CHN", "FJI", "IDN", "LAO", "MYS", "MNG", "MMR", "PHL", "THA", "VNM",
  
  # Pacific Islands
  "KIR", "MHL", "FSM", "NRU", "PLW", "PNG", "WSM", "SLV", "TLS", "TON", "TUV", "VUT"
)

# Europe (Emerging)
emerging_europe <- c(
  "ALB", "BLR", "BIH", "BGR", "HRV", "HUN", "XKX", "MDA", "MNE", "MKD", 
  "POL", "ROU", "RUS", "SRB", "TUR", "UKR"
)

# Latin America and Caribbean (Emerging)
emerging_latam <- c(
  "ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL", 
  "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND", 
  "JAM", "MEX", "NIC", "PAN", "PRY", "PER", "KNA", "LCA", "VCT", "SUR", 
  "TTO", "URY", "VEN"
)

# Middle East and Central Asia (Emerging)
emerging_meca <- c(
  "AFG", "DZA", "ARM", "AZE", "BHR", "DJI", "EGY", "GEO", "IRN", "IRQ", 
  "JOR", "KAZ", "KWT", "KGZ", "LBN", "LBY", "MRT", "MAR", "OMN", "PAK", 
  "QAT", "SAU", "SOM", "SDN", "SYR", "TJK", "TUN", "TKM", "ARE", "UZB", 
  "PSE", "YEM"
)

# Sub-Saharan Africa (Emerging)
emerging_ssafrica <- c(
  "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", "COM", 
  "COG", "COD", "CIV", "GNQ", "ERI", "SWZ", "ETH", "GAB", "GMB", "GHA", 
  "GIN", "GNB", "KEN", "LSO", "LBR", "MDG", "MWI", "MLI", "MUS", "MOZ", 
  "NAM", "NER", "NGA", "RWA", "STP", "SEN", "SYC", "SLE", "ZAF", "SSD", 
  "TZA", "TGO", "UGA", "ZMB", "ZWE"
)

#Add higher level groups
emerging_economies <- c(emerging_asia, emerging_europe, emerging_latam, emerging_meca, emerging_ssafrica)
imf_weo_countries <- c(advanced_economies, emerging_economies)

#Create country list
data(codelist)

#Check for IMF monitored countries not in list
imf_weo_countries[c(which(imf_weo_countries %in% codelist$iso3c == FALSE))]

#Check for DOTS monitored countries not in list
dots_areas %>% 
  filter(!(input_code %in% codelist$iso2c)) %>%
  print(n = nrow(.))

#Check for CDIS monitored countries not in list
cdis_areas %>% 
  filter(!(input_code %in% codelist$iso2c)) %>%
  print(n = nrow(.))

#Check for CPIS monitored countries not in list
cpis_areas %>% 
  filter(!(input_code %in% codelist$iso2c)) %>%
  print(n = nrow(.))

##Create country set----
country_set <- codelist %>%
  select(country.name.en, iso2c, iso3c, imf, continent, region) %>%
  mutate(iso2c = if_else(country.name.en == "Kosovo", "XK", iso2c), #Correct for Kosovo
         iso3c = if_else(country.name.en == "Kosovo", "XKX", iso3c),
         continent = if_else(country.name.en == "Kosovo", "Europe", continent),
         iso2c = if_else(country.name.en == "Netherlands Antilles", "AN", iso2c), #Correct for Netherlands Antilles
         iso3c = if_else(country.name.en == "Netherlands Antilles", "ANT", iso3c),
         country.name.en = if_else(country.name.en == "Curaçao", "Curacao & St. Maarten", country.name.en),
         iso2c = if_else(country.name.en == "Curacao & St. Maarten", "1C_355", iso2c),
         iso3c = if_else(country.name.en == "Curacao & St. Maarten", "CUW", iso3c), 
         imf = if_else(country.name.en == "Curacao & St. Maarten", 355, imf),
         continent = if_else(country.name.en == "Bouvet Island", "Antartica", continent),
         continent = if_else(country.name.en == "British Indian Ocean Territory", "Asia", continent),
         continent = if_else(country.name.en == "French Southern Territories", "Africa", continent),
         region = if_else(country.name.en == "Bouvet Island", "Antartica", region),
         region = if_else(country.name.en == "British Indian Ocean Territory", "East Asia & Pacific", region),
         region = if_else(country.name.en == "French Southern Territories", "Sub-Saharan Africa", region),
         region = if_else(country.name.en == "Mayotte", "Sub-Saharan Africa", region),
         region = if_else(country.name.en == "Réunion", "Sub-Saharan Africa", region),
         region = if_else(country.name.en == "St. Helena", "Sub-Saharan Africa", region),
         region = if_else(country.name.en == "Wallis & Futuna", "East Asia & Pacific", region),
         region = if_else(country.name.en == "Western Sahara", "Middle East & North Africa", region)) %>%
  mutate(weo_group_mayor = case_when(iso3c %in% advanced_economies ~ "Advanced Economies",
                                     iso3c %in% emerging_economies ~ "Emerging Economies",
                                     !iso3c %in% imf_weo_countries ~ "WEO Not-Monitored"),
         weo_group_region = case_when(iso3c %in% advanced_economies ~ "Advanced Economies",
                                      iso3c %in% emerging_asia ~ "Emerging and Developing Asia",
                                      iso3c %in% emerging_europe ~ "Emerging and Developing Europe",
                                      iso3c %in% emerging_latam ~ "Latin America and the Caribbean",
                                      iso3c %in% emerging_meca ~ "Middle East and Central Asia",
                                      iso3c %in% emerging_ssafrica ~ "Sub-Saharan Africa",
                                      !iso3c %in% imf_weo_countries ~ "WEO Not-Monitored")) %>%
  mutate(status = "current") %>%
  filter(iso2c %in% cdis_areas$input_code)

#Remove intermediate vectors
rm(imf_weo_countries, advanced_economies, emerging_economies, emerging_asia, emerging_europe, emerging_latam, emerging_meca, emerging_ssafrica)
#rm(cdis_areas, cpis_areas, dots_areas)
rm(dots_indicators, cdis_indicators, cpis_indicators)
rm(codelist)

##Former countries----
former_countries <- tibble(country.name.en = c("Former Czechoslovakia", "East Germany", "Serbia and Montenegro", "Former U.S.S.R.", "Yemen Arab Rep.", "Yemen, P.D. Rep.", "Former Yugoslavia" ),
                           iso2c = c("CSH", "DE2", "CS", "SUH","1C_473", "1C_459", "YUC"),
                           iso3c = c("CSH", "DDD", "SCG", "SUN", "YAR", "YMD", "YUG"),
                           imf = c(200, 278, 891, 810, "NA", 720, 891),
                           continent = c("Europe", "Europe", "Europe", "Europe", "Asia", "Asia", "Europe"),
                           region = c("Europe and Central Asia", "Europe and Central Asia", "Europe and Central Asia", "Europe and Central Asia", "Middle East & North Africa", "Middle East & North Africa",  "Europe and Central Asia"),
                           weo_group_mayor = "Former Countries",
                           weo_group_region = "Former Countries") %>% 
  mutate(status = "former")

##Definitive country set-----
country_set <- rbind(country_set, former_countries)
rm(former_countries)

# Download Data----
## 1.0.1 Download GDP deflator for inflation adjustment----

#Set years for data import
start_year <-1977
end_year <- 2023

##Import USD GDP deflator
usa_gdp_deflator <- imf_dataset(database_id = "IFS",
                                indicator = "NGDP_D_IX",
                                ref_area = "US",
                                freq = "A",
                                start = start_year,
                                end = end_year) %>% as_tibble() %>% 
  mutate(`value` = as.numeric(`value`), date = as.numeric(date)) %>% 
  mutate(deflator = value/filter(., date == end_year)$value*100) %>% #Set last year as 100 for putting everything on current dollars
  rename(year = date) 

##1.1 DOTS Exports----

#Initialize empty dataset for DOTS exports
dots_exports_base <- tibble()

##Set years to import data
start_year <- 1990
end_year <- 2023

#Initialize vector to keep track of which countries have been processed
countries_leftout <- c()

#Prepare progress bar for download
total_countries <- length(setdiff(country_set$iso2c, countries_leftout) %>%
  intersect(dots_areas$input_code)) #Only valid DOTS codes

print(total_countries)

progress_bar <- txtProgressBar(min = 0, max = total_countries, style = 3)

### Download loop
for (i in seq_along(setdiff(country_set$iso2c, countries_leftout))) {
  country_code <- setdiff(country_set$iso2c, countries_leftout)[i]

  # Skip invalid countries dynamically
  if (!(country_code %in% dots_areas$input_code)) {
    warning(sprintf("Skipping invalid country: %s (not a valid DOTS area)", country_code))
    next
  }
  
  # Initialize 'dots' before the try block to ensure it exists
  dots <- tibble()
  
  tryCatch({
    dots <- imf_dataset(database_id = "DOT", 
                        indicator = "TXG_FOB_USD",
                        ref_area = country_code, 
                        freq = "A", 
                        start = start_year,
                        end = end_year,
                        return_raw = TRUE) %>% 
      filter(`@FREQ` == "A",
             `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>%
      mutate(Obs = map(Obs, as.data.frame)) %>%
      unnest(Obs) %>%
      mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>%
      replace_na(list(`@REF_AREA` = "NA")) %>%
      select(-matches("X.OBS_STATUS"), -matches("@OBS_STATUS"))  # Remove redundant columns
    
    if ("X.TIME_PERIOD" %in% colnames(dots) | "X.OBS_VALUE" %in% colnames(dots)) {
      dots <- dots %>% 
        unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE) %>%
        unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE)
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to retrieve data for country: %s. Error: %s", country_code, e$message))
  })
  
  # Append the downloaded data to the aggregate dataset
  if (!is.null(dots) && nrow(dots) > 0) {
    dots_exports_base <- bind_rows(dots_exports_base, dots)
  }
  rm(dots)
  
  # Update Progress Bar
  setTxtProgressBar(progress_bar, i)
}

# Close Progress Bar
close(progress_bar)

#Edit base for processing
dots_exports_base <- dots_exports_base %>% 
  mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), 
         `@UNIT_MULT` = as.numeric(`@UNIT_MULT`),
         `@OBS_VALUE_ADJUSTED` = `@OBS_VALUE` * 10^(`@UNIT_MULT`))

#Change column names for ease of use
dots_exports_base <- dots_exports_base %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(frequency = `@FREQ`,
         indicator = `@INDICATOR`,
         unit_mult = `@UNIT_MULT`,
         jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`, 
         flow = `@OBS_VALUE`,
         flow_adj = `@OBS_VALUE_ADJUSTED`) %>%
  filter(!(is.na(flow_adj)))

#Inflationary adjustment and final filter
dots_exports_base <- dots_exports_base %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(flow_adj_cons = flow_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, flow, flow_adj, flow_adj_cons, jurisdiction_iso3, counterpart_iso3, `indicator.x`) %>%
  rename(indicator = `indicator.x`) %>%
  filter(flow != 0) #Get rid of 0 values

##1.2 DOTS Imports----

#Initialize empty dataset for DOTS imports
dots_imports_base <- tibble()

##Set years to import data
start_year <- 1990
end_year <- 2023

#Initialize vector to keep track of which countries have been processed
countries_leftout <- c()

#Prepare progress bar for download
total_countries <- length(setdiff(country_set$iso2c, countries_leftout) %>%
                            intersect(dots_areas$input_code)) #Only valid DOTS codes

print(total_countries)

progress_bar <- txtProgressBar(min = 0, max = total_countries, style = 3)

### Download loop
for (i in seq_along(setdiff(country_set$iso2c, countries_leftout))) {
  country_code <- setdiff(country_set$iso2c, countries_leftout)[i]
  
  # Skip invalid countries dynamically
  if (!(country_code %in% dots_areas$input_code)) {
    warning(sprintf("Skipping invalid country: %s (not a valid DOTS area)", country_code))
    next
  }
  
  # Initialize 'dots' before the try block to ensure it exists
  dots <- tibble()
  
  tryCatch({
    dots <- imf_dataset(database_id = "DOT", 
                        indicator = "TMG_CIF_USD",
                        ref_area = country_code, 
                        freq = "A", 
                        start = start_year,
                        end = end_year,
                        return_raw = TRUE) %>% 
      filter(`@FREQ` == "A",
             `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>%
      mutate(Obs = map(Obs, as.data.frame)) %>%
      unnest(Obs) %>%
      mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>%
      replace_na(list(`@REF_AREA` = "NA")) %>%
      select(-matches("X.OBS_STATUS"), -matches("@OBS_STATUS"))  # Remove redundant columns
    
    if ("X.TIME_PERIOD" %in% colnames(dots) | "X.OBS_VALUE" %in% colnames(dots)) {
      dots <- dots %>% 
        unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE) %>%
        unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE)
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to retrieve data for country: %s. Error: %s", country_code, e$message))
  })
  
  # Append the downloaded data to the aggregate dataset
  if (!is.null(dots) && nrow(dots) > 0) {
    dots_imports_base <- bind_rows(dots_imports_base, dots)
  }
  rm(dots)
  
  # Update Progress Bar
  setTxtProgressBar(progress_bar, i)
}

# Close Progress Bar
close(progress_bar)

#Edit base for processing
dots_imports_base <- dots_imports_base %>% 
  mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), 
         `@UNIT_MULT` = as.numeric(`@UNIT_MULT`),
         `@OBS_VALUE_ADJUSTED` = `@OBS_VALUE` * 10^(`@UNIT_MULT`))

#Change column names for ease of use
dots_imports_base <- dots_imports_base %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(frequency = `@FREQ`,
         indicator = `@INDICATOR`,
         unit_mult = `@UNIT_MULT`,
         jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`, 
         flow = `@OBS_VALUE`,
         flow_adj = `@OBS_VALUE_ADJUSTED`) %>%
  filter(!(is.na(flow_adj)))

#Inflationary adjustment and final filter
dots_imports_base <- dots_imports_base %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(flow_adj_cons = flow_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, flow, flow_adj, flow_adj_cons, jurisdiction_iso3, counterpart_iso3, `indicator.x`) %>%
  rename(indicator = `indicator.x`) %>%
  filter(flow != 0) #Get rid of 0 values

##1.3 CDIS Inward-----

#Initialize empty dataset for CDIS Inward
cdis_inward_base <- tibble()

#Set years for data import
start_year <- 2009
end_year <- 2023

#Initialize vector to keep track of which countries have been processed
countries_leftout <- c()

#Prepare progress bar for download
total_countries <- length(setdiff(country_set$iso2c, countries_leftout) %>% 
                            intersect(cdis_areas$input_code))

print(total_countries)
progress_bar <- txtProgressBar(min = 0, max = total_countries, style = 3)

### Download loop
for (i in seq_along(setdiff(country_set$iso2c, countries_leftout))) {
  country_code <- setdiff(country_set$iso2c, countries_leftout)[i]
  
  # Initialize 'cdis' before the try block to ensure it exists
  cdis <- tibble()
  
  tryCatch({
    cdis <- imf_dataset(database_id = "CDIS", 
                        indicator = "IIW_BP6_USD",
                        ref_area = country_code, 
                        freq = "A", 
                        start = start_year,
                        end = end_year,
                        return_raw = TRUE) %>% 
      filter(`@FREQ` == "A",
             `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>%
      mutate(Obs = map(Obs, as.data.frame)) %>%
      unnest(Obs) %>%
      mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>%
      replace_na(list(`@REF_AREA` = "NA"))
    
    if ("X.TIME_PERIOD" %in% colnames(cdis) | "X.OBS_VALUE" %in% colnames(cdis)) {
      cdis <- cdis %>% 
        unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE) %>%
        unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE)
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to retrieve data for country: %s. Error: %s", country_code, e$message))
  })
  
  # Append the downloaded data to the aggregate dataset
  if (nrow(cdis) > 0) {
    cdis_inward_base <- bind_rows(cdis_inward_base, cdis)
  }
  rm(cdis)
  # Update Progress Bar
  setTxtProgressBar(progress_bar, i)
}

# Close Progress Bar
close(progress_bar)

#Edit base for processing
cdis_inward_base <- cdis_inward_base %>% 
  mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), 
         `@UNIT_MULT` = as.numeric(`@UNIT_MULT`),
         `@OBS_VALUE_ADJUSTED` = `@OBS_VALUE` * 10^(`@UNIT_MULT`))

#Change column names for ease of use
cdis_inward_base <- cdis_inward_base %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(frequency = `@FREQ`,
         indicator = `@INDICATOR`,
         unit_mult = `@UNIT_MULT`,
         jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`, 
         position = `@OBS_VALUE`,
         position_adj = `@OBS_VALUE_ADJUSTED`,
         obs_status = `@OBS_STATUS`) %>%
  filter(!(is.na(position_adj) & is.na(obs_status)))

#Check how many confidential registries are
sum(cdis_inward_base$obs_status == "C", na.rm = TRUE)

#Negative registries
sum(cdis_inward_base$position < 0, na.rm = TRUE)
sum(cdis_inward_base$position < 0, na.rm = TRUE)/length(cdis_inward_base$position)*100

#Zero registries
sum(cdis_inward_base$position == 0, na.rm = TRUE)
sum(cdis_inward_base$position == 0, na.rm = TRUE)/length(cdis_inward_base$position)*100

#Inflationary adjustment and final filter
cdis_inward_base <- cdis_inward_base %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(position_adj_cons = position_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, obs_status, position, position_adj, position_adj_cons, jurisdiction_iso3, counterpart_iso3, `indicator.x`) %>%
  rename(indicator = `indicator.x`) %>%
  filter(position != 0 | obs_status == "C") #Get rid of 0 values while preserving confidential

##1.4 CDIS Outward ------

#Initialize empty dataset for CDIS Outward
cdis_outward_base <- tibble()

#Set years for data import
start_year <- 2009
end_year <- 2023

#Initialize vector to keep track of which countries have been processed
countries_leftout <- c()

#Prepare progress bar for download
total_countries <- length(setdiff(country_set$iso2c, countries_leftout) %>%
                            intersect(cdis_areas$input_code))
print(total_countries)
progress_bar <- txtProgressBar(min = 0, max = total_countries, style = 3)

### Download loop
for (i in seq_along(setdiff(country_set$iso2c, countries_leftout))) {
  country_code <- setdiff(country_set$iso2c, countries_leftout)[i]
  
  # Initialize 'cdis' before the try block to ensure it exists
  cdis <- tibble()
  
  tryCatch({
    cdis <- imf_dataset(database_id = "CDIS", 
                        indicator = "IOW_BP6_USD",
                        ref_area = country_code, 
                        freq = "A", 
                        start = start_year,
                        end = end_year,
                        return_raw = TRUE) %>% 
      filter(`@FREQ` == "A",
             `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>%
      mutate(Obs = map(Obs, as.data.frame)) %>%
      unnest(Obs) %>%
      mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>%
      replace_na(list(`@REF_AREA` = "NA"))
    
    if ("X.TIME_PERIOD" %in% colnames(cdis) | "X.OBS_VALUE" %in% colnames(cdis)) {
      cdis <- cdis %>% 
        unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE) %>%
        unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE)
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to retrieve data for country: %s. Error: %s", country_code, e$message))
  })
  
  # Append the downloaded data to the aggregate dataset
  if (nrow(cdis) > 0) {
    cdis_outward_base <- bind_rows(cdis_outward_base, cdis)
  }
  rm(cdis)
  # Update Progress Bar
  setTxtProgressBar(progress_bar, i)
}

# Close Progress Bar
close(progress_bar)

#Edit base for processing
cdis_outward_base <- cdis_outward_base %>% 
  mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), 
         `@UNIT_MULT` = as.numeric(`@UNIT_MULT`),
         `@OBS_VALUE_ADJUSTED` = `@OBS_VALUE` * 10^(`@UNIT_MULT`))

#Change column names for ease of use
cdis_outward_base <- cdis_outward_base %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(frequency = `@FREQ`,
         indicator = `@INDICATOR`,
         unit_mult = `@UNIT_MULT`,
         jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`, 
         position = `@OBS_VALUE`,
         position_adj = `@OBS_VALUE_ADJUSTED`,
         obs_status = `@OBS_STATUS`) %>%
  filter(!(is.na(position_adj) & is.na(obs_status)))

#Check for confidential
sum(cdis_outward_base$obs_status == "C", na.rm = TRUE)

#Check for negative
sum(cdis_outward_base$position < 0, na.rm = TRUE)
sum(cdis_outward_base$position < 0, na.rm = TRUE)/length(cdis_outward_base$position)*100

#Check for zero
sum(cdis_outward_base$position == 0, na.rm = TRUE)
sum(cdis_outward_base$position == 0, na.rm = TRUE)/length(cdis_outward_base$position)*100

#Inflationary adjustment
cdis_outward_base <- cdis_outward_base %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(position_adj_cons = position_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, obs_status, position, position_adj, position_adj_cons, jurisdiction_iso3, counterpart_iso3, `indicator.x`) %>%
  rename(indicator = `indicator.x`) %>% 
  filter(position != 0 | obs_status == "C") #Get rid of 0 values while preserving confidential

##1.5 CPIS Assets -----

#Initialize empty dataset for CPIS Assets
cpis_assets_base <- tibble()

#Set years for data import
start_year <- 2001
end_year <- 2023

#Initialize vector to keep track of which countries have been processed
countries_leftout <- c()

#Prepare progress bar for download
total_countries <- length(setdiff(country_set$iso2c, countries_leftout) %>%
                            intersect(cpis_areas$input_code))

print(total_countries)

progress_bar <- txtProgressBar(min = 0, max = total_countries, style = 3)

### Download loop
for (i in seq_along(setdiff(country_set$iso2c, countries_leftout))) {
  country_code <- setdiff(country_set$iso2c, countries_leftout)[i]
  
  # Initialize 'cpis' before the try block to ensure it exists
  cpis <- tibble()
  
  tryCatch({
    cpis <- imf_dataset(database_id = "CPIS", 
                        indicator = "I_A_T_T_T_BP6_USD",
                        ref_area = country_code, 
                        freq = "A", 
                        start = start_year,
                        end = end_year,
                        ref_sector = "T",
                        counterpart_sector = "T",
                        return_raw = TRUE) %>% 
      filter(`@FREQ` == "A",
             `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>%
      mutate(Obs = map(Obs, as.data.frame)) %>%
      unnest(Obs) %>%
      mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>%
      replace_na(list(`@REF_AREA` = "NA"))
    
    if ("X.TIME_PERIOD" %in% colnames(cpis)) {
      cpis <- cpis %>% 
        unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE)
    }
    
    if ("X.OBS_VALUE" %in% colnames(cpis)) {
      cpis <- cpis %>% 
        unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE)
    }
    
    if ("X.OBS_STATUS" %in% colnames(cpis)) {
      cpis <- cpis %>% 
        unite("@OBS_STATUS", c("@OBS_STATUS", "X.OBS_STATUS"), na.rm = TRUE)
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to retrieve data for country: %s. Error: %s", country_code, e$message))
  })
  
  # Append the downloaded data to the aggregate dataset
  if (nrow(cpis) > 0) {
    cpis_assets_base <- bind_rows(cpis_assets_base, cpis)
  }
  
  #Remove cpis to reset
  rm(cpis)
  
  # Update Progress Bar
  setTxtProgressBar(progress_bar, i)
}

# Close Progress Bar
close(progress_bar)

#Edit base for processing
cpis_assets_base <- cpis_assets_base %>% 
  mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), 
         `@UNIT_MULT` = as.numeric(`@UNIT_MULT`),
         `@OBS_VALUE_ADJUSTED` = `@OBS_VALUE` * 10^(`@UNIT_MULT`))

#Change column names for ease of use
cpis_assets_base <- cpis_assets_base %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(frequency = `@FREQ`,
         indicator = `@INDICATOR`,
         unit_mult = `@UNIT_MULT`,
         jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`, 
         position = `@OBS_VALUE`,
         position_adj = `@OBS_VALUE_ADJUSTED`,
         obs_status = `@OBS_STATUS`) %>%
  filter(!(is.na(position_adj) & is.na(obs_status)))

#Check for confidential
sum(cpis_assets_base$obs_status == "C", na.rm = TRUE)

#Check for negative values
sum(cpis_assets_base$position < 0, na.rm = TRUE)
sum(cpis_assets_base$position < 0, na.rm = TRUE)/length(cpis_assets_base$position)*100

#Check for 0 values
sum(cpis_assets_base$position == 0, na.rm = TRUE)
sum(cpis_assets_base$position == 0, na.rm = TRUE)/length(cpis_assets_base$position)*100

#Inflationary adjustment
cpis_assets_base <- cpis_assets_base %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(position_adj_cons = position_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, obs_status, position, position_adj, position_adj_cons, jurisdiction_iso3, counterpart_iso3, `indicator.x`) %>%
  rename(indicator = `indicator.x`) %>%
  filter(position != 0 | obs_status == "C") #Get rid of 0 values while preserving conficential

##1.6 CPIS Liabilities-----

#Initialize empty datasets for CPIS
cpis_liabilities_base <- tibble()

start_year <- 2001
end_year <- 2023

#Initialize vector to keep track of which countries have been processed
countries_leftout <- c()

#Prepare progress bar for download
total_countries <- length(setdiff(country_set$iso2c, countries_leftout) %>%
                            intersect(cpis_areas$input_code))
                            
print(total_countries)

progress_bar <- txtProgressBar(min = 0, max = total_countries, style = 3)

### Download loop
for (i in seq_along(setdiff(country_set$iso2c, countries_leftout))) {
  country_code <- setdiff(country_set$iso2c, countries_leftout)[i]
  
  # Initialize 'cpis' before the try block to ensure it exists
  cpis <- tibble()
  
  tryCatch({
    cpis <- imf_dataset(database_id = "CPIS", 
                        indicator = "I_L_T_T_T_BP6_USD",
                        ref_area = country_code, 
                        freq = "A", 
                        start = start_year,
                        end = end_year,
                        ref_sector = "T",
                        counterpart_sector = "T",
                        return_raw = TRUE) %>% 
      filter(`@FREQ` == "A",
             `@COUNTERPART_AREA` %in% c(country_set$iso2c)) %>%
      mutate(Obs = map(Obs, as.data.frame)) %>%
      unnest(Obs) %>%
      mutate(`@REF_AREA` = as.character(`@REF_AREA`)) %>%
      replace_na(list(`@REF_AREA` = "NA"))
    
    if ("X.TIME_PERIOD" %in% colnames(cpis)) {
      cpis <- cpis %>% 
        unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE)
    }
    
    if ("X.OBS_VALUE" %in% colnames(cpis)) {
      cpis <- cpis %>% 
        unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE)
    }
    
    if ("X.OBS_STATUS" %in% colnames(cpis)) {
      cpis <- cpis %>% 
        unite("@OBS_STATUS", c("@OBS_STATUS", "X.OBS_STATUS"), na.rm = TRUE)
    }
    
  }, error = function(e) {
    warning(sprintf("Failed to retrieve data for country: %s. Error: %s", country_code, e$message))
  })
  
  # Append the downloaded data to the aggregate dataset
  if (nrow(cpis) > 0) {
    cpis_liabilities_base <- bind_rows(cpis_liabilities_base, cpis)
  }
  
  #Remove cpis to reset
  rm(cpis)
  
  # Update Progress Bar
  setTxtProgressBar(progress_bar, i)
}

# Close Progress Bar
close(progress_bar)

#Edit base for processing
cpis_liabilities_base <- cpis_liabilities_base %>%
  unite("@OBS_VALUE", c("@OBS_VALUE", "X.OBS_VALUE"), na.rm = TRUE) %>%
  unite("@TIME_PERIOD", c("@TIME_PERIOD", "X.TIME_PERIOD"), na.rm = TRUE) %>%
  mutate(`@OBS_VALUE` = as.numeric(`@OBS_VALUE`), 
         `@UNIT_MULT` = as.numeric(`@UNIT_MULT`),
         `@OBS_VALUE_ADJUSTED` = `@OBS_VALUE` * 10^(`@UNIT_MULT`))

#Change column names for ease of use
cpis_liabilities_base <- cpis_liabilities_base %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@REF_AREA` = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c(`@COUNTERPART_AREA` = "iso2c")) %>%
  rename(frequency = `@FREQ`,
         indicator = `@INDICATOR`,
         unit_mult = `@UNIT_MULT`,
         jurisdiction_iso3 = iso3c.x,
         jurisdiction_iso2 = `@REF_AREA`,
         counterpart_iso3 = iso3c.y,
         counterpart_iso2 = `@COUNTERPART_AREA`,
         year = `@TIME_PERIOD`, 
         position = `@OBS_VALUE`,
         position_adj = `@OBS_VALUE_ADJUSTED`,
         obs_status = `@OBS_STATUS`) %>%
  filter(!(is.na(position_adj) & is.na(obs_status)))

#Inflationary adjustment
cpis_liabilities_base <- cpis_liabilities_base %>%
  mutate(year = as.numeric(year)) %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(position_adj_cons = position_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, obs_status, position, position_adj, position_adj_cons, jurisdiction_iso3, counterpart_iso3, `indicator.x`) %>%
  rename(indicator = `indicator.x`) %>%
  filter(position != 0 | obs_status == "C") #Get rid of 0 values while preserving conficential

#Check for confidential
sum(cpis_liabilities_base$obs_status == "C", na.rm = TRUE)

#Check for negative values
sum(cpis_liabilities_base$position < 0, na.rm = TRUE)
sum(cpis_liabilities_base$position < 0, na.rm = TRUE)/length(cpis_liabilities_base$position)*100

#Check for zero values
sum(cpis_liabilities_base$position == 0, na.rm = TRUE)
sum(cpis_liabilities_base$position == 0, na.rm = TRUE)/length(cpis_liabilities_base$position)*100

##1.7 BIS LBS -----

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
#Arrange database

#Tibble of parent country
lbs_parent_country <- lbs_base %>%
  distinct(L_PARENT_CTY, `Parent country`) %>%
  as_tibble() %>%
  rename(country_code = L_PARENT_CTY, country_name = `Parent country`)

#Check not in country_set
lbs_parent_country %>% 
  filter(!(country_code %in% country_set$iso2c)) %>%
  print(n = nrow(.))

#Tibble of reporting countries 
lbs_reporting_country <- lbs_base %>%
  distinct(L_REP_CTY, `Reporting country`) %>%
  as_tibble() %>%
  rename(country_code = L_REP_CTY, country_name = `Reporting country`)

#Check not in country_set
lbs_reporting_country %>% 
  filter(!(country_code %in% country_set$iso2c)) %>%
  print(n = nrow(.))

#Tibble of counterpart countries
lbs_counterpart_country <- lbs_base %>%
  distinct(L_CP_COUNTRY, `Counterparty country`) %>%
  as_tibble() %>%
  rename(country_code = L_CP_COUNTRY, country_name = `Counterparty country`)

#Check not in country_set
lbs_counterpart_country %>% 
  filter(!(country_code %in% country_set$iso2c)) %>%
  print(n = nrow(.))

#Filter for processing 
lbs_base_filtered <- tibble()

lbs_base_filtered <- lbs_base %>%
  #Fix Curacaso
  mutate(L_PARENT_CTY = case_when(L_PARENT_CTY == "CW" ~ "1C_355", TRUE ~ L_PARENT_CTY),
         L_REP_CTY = case_when(L_REP_CTY == "CW" ~ "1C_355", TRUE ~ L_REP_CTY),
         L_CP_COUNTRY = case_when(L_CP_COUNTRY == "CW" ~ "1C_355", TRUE ~ L_CP_COUNTRY)) %>%
  #Filters:
  #Only counterpart countries in country set
  filter(L_CP_COUNTRY %in% c(country_set$iso2c),
         #Only reporting countries in country set
         L_REP_CTY %in% c(country_set$iso2c),
         #Parent country all
         L_PARENT_CTY == "5J",
         #Ammounts outstanding/stocks
         L_MEASURE == "S",
         #All instruments
         L_INSTR == "A",
         #All currencies
         L_DENOM == "TO1",
         #All currencies
         L_CURR_TYPE == "A",
         #All types of intitutions
         L_REP_BANK_TYPE == "A",
         #All counterpart sectors
         L_CP_SECTOR == "A",
         #Cross border positions
         L_POS_TYPE == "N")

rm(lbs_counterpart_country, lbs_parent_country, lbs_reporting_country)

#Pivot 
lbs_base_filtered <- lbs_base_filtered  %>%
  pivot_longer(cols = -c(1:31),
               names_to = "date",
               values_to = "position")

#Filter by year
lbs_base_filtered <- lbs_base_filtered %>%
  mutate(year = as.numeric(substr(date, 1, 4)),  # Extracts the first four characters as the year
         quarter = substr(date, 6, 8)) %>%  # Extracts the last part as the quarter
  filter(quarter == "Q4", #Filter to retain only entries from the fourth quarter
         year <= end_year) #Filter to only get data up until final year

#Count NAs
sum(lbs_base_filtered$position == "NaN", na.rm = TRUE)
sum(is.na(lbs_base_filtered$position))

#Count zeroes
sum(lbs_base_filtered$position == 0, na.rm = TRUE)

#Arrange
lbs_base_filtered <- lbs_base_filtered %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c("L_REP_CTY" = "iso2c")) %>%
  left_join(country_set[, c("iso2c", "iso3c")], by = c("L_CP_COUNTRY" = "iso2c")) %>%
  rename(jurisdiction_iso2 = L_REP_CTY,
         jurisdiction_iso3 = iso3c.x,
         counterpart_iso2 =  L_CP_COUNTRY,
         counterpart_iso3 = iso3c.y,
         indicator = Series) %>%
  mutate(across(position, ~replace(., is.nan(.), NA)),
         position_adj = position*10^6) %>%
  filter(position != 0) %>%
  drop_na(position)


#Split
lbs_claims_base <- tibble()

lbs_claims_base <- lbs_base_filtered %>%
  filter(L_POSITION == "C")

lbs_liabilities_base <- tibble()

lbs_liabilities_base <- lbs_base_filtered %>%
  filter(L_POSITION == "L")

#Inflationary adjustment
lbs_claims_base <- lbs_claims_base %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(position_adj_cons = position_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, position, position_adj, position_adj_cons, jurisdiction_iso3, counterpart_iso3, indicator.x) %>%
  rename(indicator = indicator.x)

lbs_liabilities_base <- lbs_liabilities_base %>%
  left_join(., usa_gdp_deflator, by = "year") %>%
  mutate(position_adj_cons = position_adj/deflator*100) %>%
  select(year, jurisdiction_iso2, counterpart_iso2, position, position_adj, position_adj_cons, jurisdiction_iso3, counterpart_iso3, indicator.x) %>%
  rename(indicator = indicator.x)

#Sum instances of negative positions
sum(lbs_claims_base$position < 0, na.rm = TRUE)
sum(lbs_liabilities_base$position < 0, na.rm = TRUE)

#Remove redundant bases
rm(lbs_base, lbs_base_filtered)

#1.8 Join all data sets----

# Standardize and merge datasets
dataset_base <- bind_rows(
  dots_exports_base %>%
    mutate(source = "DOTS Exports", 
           value = flow, 
           value_adj = flow_adj,
           value_adj_cons = flow_adj_cons,
           value_type = "flow",
           obs_status = NA_character_),  # Add obs_status as NA
  
  dots_imports_base %>%
    mutate(source = "DOTS Imports", 
           value = flow, 
           value_adj = flow_adj,
           value_adj_cons = flow_adj_cons,
           value_type = "flow",
           obs_status = NA_character_),  # Add obs_status as NA
  
  cdis_inward_base %>%
    mutate(source = "CDIS Inward", 
           value = position, 
           value_adj = position_adj,
           value_adj_cons = position_adj_cons,
           value_type = "position"),  # Keep obs_status (already exists)
  
  cdis_outward_base %>%
    mutate(source = "CDIS Outward", 
           value = position, 
           value_adj = position_adj,
           value_adj_cons = position_adj_cons,
           value_type = "position"),  # Keep obs_status (already exists)
  
  cpis_assets_base %>%
    mutate(source = "CPIS Assets", 
           value = position, 
           value_adj = position_adj,
           value_adj_cons = position_adj_cons,
           value_type = "position"),  # Keep obs_status (already exists)
  
  cpis_liabilities_base %>%
    mutate(source = "CPIS Liabilities", 
           value = position, 
           value_adj = position_adj,
           value_adj_cons = position_adj_cons,
           value_type = "position"),  # Keep obs_status (already exists)
  
  lbs_claims_base %>%
    mutate(source = "LBS Claims", 
           value = position, 
           value_adj = position_adj,
           value_adj_cons = position_adj_cons,
           value_type = "position",
           obs_status = NA_character_),  # Add obs_status as NA
  
  lbs_liabilities_base %>%
    mutate(source = "LBS Liabilities", 
           value = position, 
           value_adj = position_adj,
           value_adj_cons = position_adj_cons,
           value_type = "position",
           obs_status = NA_character_)  # Add obs_status as NA
) %>%
  select(year, jurisdiction_iso3, counterpart_iso3, value, value_adj, value_adj_cons, obs_status,
         value_type, indicator, source)  # Ensure correct column order

##1.8.1 Save merged dataset--------

# Define data directory
data_dir <- file.path(system("git rev-parse --show-toplevel", intern = TRUE), "data")

#Create CSV and savefile
write.csv(dataset_base, file.path(data_dir, "trade_finance_network_data.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")

## Save additional datasets for further use
write.csv(country_set, file.path(data_dir, "country_set.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")

#Save environment
save.image(file = file.path(data_dir, "session_backup.RData"))

#1.9 Generate summary statistics ------

summary_by_source <- dataset_base %>%
  group_by(source) %>%
  summarise(
    Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
    Areas = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
    Links = n(),
    Confidential = sum(obs_status == "C", na.rm = TRUE),
    Min = min(value_adj_cons / 1000, na.rm = TRUE),
    Max = max(value_adj_cons / 1000, na.rm = TRUE),
    Mean = mean(value_adj_cons / 1000, na.rm = TRUE),
    Median = median(value_adj_cons / 1000, na.rm = TRUE),
    `Std Dev` = sd(value_adj_cons / 1000, na.rm = TRUE),
    .groups = 'drop'
  )

summary_by_source

# 1.10 Common countries per year by source  -------

# Countries by year
countries_by_year <-
  dataset_base %>%
    select(year, jurisdiction_iso3, counterpart_iso3, source) %>%
    pivot_longer(cols = c(jurisdiction_iso3, counterpart_iso3), names_to = "type", values_to = "country_iso3") %>%
    distinct(year, country_iso3, source) %>%
    group_by(year, source) %>%
    summarize(countries = list(unique(country_iso3)), .groups = 'drop')

#1.10 Find common countries and generate summaries-----

# Define the function to extract unique countries by year from a dataset, considering both jurisdiction and counterpart ISO3 codes
extract_countries_by_year <- function(data) {
  data %>%
    select(year, jurisdiction_iso3, counterpart_iso3) %>%
    pivot_longer(cols = c(jurisdiction_iso3, counterpart_iso3), names_to = "type", values_to = "country_iso3") %>%
    distinct(year, country_iso3) %>%
    group_by(year) %>%
    summarize(countries = list(unique(country_iso3)), .groups = 'drop')
}

# Define the function to find common countries across multiple datasets by year
find_common_countries <- function(list_of_dfs) {
  # Extract countries by year for each dataset
  countries_by_year <- lapply(list_of_dfs, extract_countries_by_year)
  
  # Initialize the common_countries data frame with years from the first dataset
  common_countries <- countries_by_year[[1]]
  
  # Iterate through the rest of the datasets to find the intersection
  for (i in 2:length(countries_by_year)) {
    common_countries <- common_countries %>%
      left_join(countries_by_year[[i]], by = "year", suffix = c("", ".y")) %>%
      mutate(countries = map2(countries, countries.y, intersect)) %>%
      select(-countries.y)
  }
  
  common_countries
}

# Define the function to generate descriptive summary
generate_summary <- function(data, confidential_col = NULL) {
  data %>%
    summarise(
      Period = paste(min(year, na.rm = TRUE), max(year, na.rm = TRUE), sep = "-"),
      `Areas` = n_distinct(c(jurisdiction_iso3, counterpart_iso3)),
      `Links` = n(),
      `Confidential` = if (!is.null(confidential_col) && confidential_col %in% names(data)) sum(data[[confidential_col]] == "C", na.rm = TRUE) else NA,
      `Min` = min(position/1000, na.rm = TRUE),
      `Max` = max(position/1000, na.rm = TRUE),
      `Mean` = mean(position/1000, na.rm = TRUE),
      `Median` = median(position/1000, na.rm = TRUE),
      `Std Dev` = sd(position/1000, na.rm = TRUE)
    )
}

# Load datasets (ensure they are available in the environment)
datasets <- list(
  `DOTS Exports` = dots_exports_base,
  `DOTS Imports` = dots_imports_base,
  `CDIS Inward` = cdis_inward_base,
  `CDIS Outward` = cdis_outward_base,
  `CPIS Assets` = cpis_assets_base,
  `CPIS Liabilities` = cpis_liabilities_base,
  `LBS Claims` = lbs_claims_base,
  `LBS Liabilities` = lbs_liabilities_base
)

datasets_assets <- list(
  `CDIS Outward` = cdis_outward_base,
  `CPIS Assets` = cpis_assets_base,
  `LBS Claims` = lbs_claims_base
)

# Generate summaries for all datasets
summary_all <- lapply(datasets, function(data) generate_summary(data, confidential_col = "obs_status"))
summary_table_all <- bind_rows(summary_all, .id = "Dataset")

# Ensure the common countries are correctly identified for assets, claims, and outward datasets
common_countries_assets <- find_common_countries(datasets_assets)
common_countries_assets <- common_countries_assets %>% pull(countries) %>% unlist() %>% unique()

cdis_outward_filtered <- filter(cdis_outward_base, jurisdiction_iso3 %in% common_countries_assets  & counterpart_iso3 %in% common_countries_assets) %>% filter(year >= 2009)
cpis_assets_filtered <- filter(cpis_assets_base, jurisdiction_iso3 %in% common_countries_assets  & counterpart_iso3 %in% common_countries_assets) %>% filter(year >= 2009)
lbs_claims_filtered <- filter(lbs_claims_base, jurisdiction_iso3 %in% common_countries_assets  & counterpart_iso3 %in% common_countries_assets) %>% filter(year >= 2009)

summary_assets <- list(
  `CDIS Outward` = generate_summary(cdis_outward_filtered, confidential_col = "obs_status"),
  `CPIS Assets` = generate_summary(cpis_assets_filtered, confidential_col = "obs_status"),
  `LBS Claims` = generate_summary(lbs_claims_filtered)
)

summary_table_assets <- bind_rows(summary_assets, .id = "Dataset")
