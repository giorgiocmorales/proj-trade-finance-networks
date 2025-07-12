# CUNTO SANTANA (20XX)

# 1.0 RETRIEVE AND INSPECT AVAILABLE IMF DATASETS ------------------------------

# 1.0.1 Load all dataflows (available IMF datasets)
imf_dataflows <- readSDMX(providerId = "IMF_DATA", resource = "dataflow")
print("IMF dataflows successfully retrieved")

dataset_table <- do.call(rbind, lapply(slot(imf_dataflows, "dataflows"), function(df) {
  data.frame(
    id          = slot(df, "id"),
    agency      = slot(df, "agencyID"),
    version     = slot(df, "version"),
    name        = slot(df, "Name")[["en"]],
    description = slot(df, "Description")[["en"]],
    dsdRef      = slot(df, "dsdRef"),
    stringsAsFactors = FALSE
  )
}))
print("Available datasets:")
print(head(dataset_table, 10))

rm(imf_dataflows)
gc()

# 1.1 IMTS METADATA EXTRACTION --------------------------------------------------

# 1.1.1 Load structure definition
imts_structure <- readSDMX(providerId = "IMF_DATA", resource = "datastructure", flowRef = "IMTS")
print("IMTS structure loaded")

# 1.1.2 Isolate DSD
imts_structure_list <- imts_structure@datastructures@datastructures
imts_structure_ids <- sapply(imts_structure_list, function(x) x@id)
imts_structure_index <- which(imts_structure_ids == "DSD_IMTS")
if (length(imts_structure_index) == 1) {
  imts_dsd <- imts_structure_list[[imts_structure_index]]
  print(paste("Using IMTS structure ID:", imts_structure_ids[imts_structure_index]))
} else {
  stop("IMTS structure ID 'DSD_IMTS' not found or ambiguous.")
}

# 1.1.3 Extract dimensions
imts_dimensions <- imts_dsd@Components@Dimensions
imts_dimension_ids <- sapply(imts_dimensions, function(x) x@conceptRef)
print("IMTS dimensions found:")
print(imts_dimension_ids)

# 1.1.4 Extract codelists
imts_codelists <- imts_structure@codelists@codelists
imts_codelist_ids <- as.data.frame(sapply(imts_codelists, function(cl) cl@id))
print("IMTS codelist IDs:")
print(head(imts_codelist_ids, 10))

# 1.1.5 Helper function
extract_imts_codelist_df <- function(id) {
  idx <- which(imts_codelist_ids == id)
  if (length(idx) == 0) return(NULL)
  codes <- imts_codelists[[idx]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

# 1.1.6 Extract key dimensions
imts_country_values   <- extract_imts_codelist_df("CL_IMTS_COUNTRY")
imts_indicator_values <- extract_imts_codelist_df("CL_IMTS_INDICATOR")
imts_freq_values      <- extract_imts_codelist_df("CL_FREQ")

# 1.1.7 Clean up
rm(imts_structure, imts_structure_list, imts_structure_ids, imts_structure_index, imts_dsd, imts_dimensions,imts_dimension_ids,imts_codelists, imts_codelist_ids)
rm(extract_imts_codelist_df)

gc()

# 1.2 DIP METADATA EXTRACTION ---------------------------------------------------

# 1.2.1 Load structure
dip_structure <- readSDMX(providerId = "IMF_DATA", resource = "datastructure", flowRef = "DIP")
print("DIP structure loaded")

# 1.2.2 Isolate DSD
dip_structure_list <- dip_structure@datastructures@datastructures
dip_structure_ids <- sapply(dip_structure_list, function(x) x@id)
dip_structure_index <- which(dip_structure_ids == "DSD_DIP")
if (length(dip_structure_index) == 1) {
  dip_dsd <- dip_structure_list[[dip_structure_index]]
  print(paste("Using DIP structure ID:", dip_structure_ids[dip_structure_index]))
} else {
  stop("DSD_DIP not found or multiple matches.")
}

# 1.2.3 Extract dimensions
dip_dimensions <- dip_dsd@Components@Dimensions
dip_dimension_ids <- sapply(dip_dimensions, function(x) x@conceptRef)
print("DIP dimension conceptRefs:")
print(dip_dimension_ids)

# 1.2.4 Extract codelists
dip_codelists <- dip_structure@codelists@codelists
dip_codelist_ids <- as.data.frame(sapply(dip_codelists, function(cl) cl@id))
print("Available DIP codelist IDs:")
print(dip_codelist_ids)

# 1.2.5 Helper function
extract_dip_codelist_df <- function(id) {
  idx <- which(dip_codelist_ids == id)
  if (length(idx) == 0) return(NULL)
  codes <- dip_codelists[[idx]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

# 1.2.6 Extract key values
dip_country_values     <- extract_dip_codelist_df("CL_DIP_COUNTRY")
dip_indicator_values   <- extract_dip_codelist_df("CL_DIP_INDICATOR")
dip_freq_values        <- extract_dip_codelist_df("CL_FREQ")
dip_type_values        <- extract_dip_codelist_df("CL_DIP_DV_TYPE")
dip_direction_values   <- extract_dip_codelist_df("CL_DIP_DI_DIRECTION")
dip_entity_values      <- extract_dip_codelist_df("CL_DIP_DI_ENTITY")

# 1.2.7 Clean up
rm(dip_structure, dip_structure_list, dip_structure_ids, dip_structure_index, dip_dsd, dip_dimensions, dip_dimension_ids, dip_codelists, dip_codelist_ids)
rm(extract_dip_codelist_df)
gc()

# 1.3 PIP METADATA EXTRACTION ---------------------------------------------------

# 1.3.1 Load structure
pip_structure <- readSDMX(providerId = "IMF_DATA", resource = "datastructure", flowRef = "PIP")
print("PIP structure loaded")

# 1.3.2 Isolate DSD
pip_structure_list <- pip_structure@datastructures@datastructures
pip_structure_ids <- sapply(pip_structure_list, function(x) x@id)
pip_structure_index <- which(pip_structure_ids == "DSD_PIP")
if (length(pip_structure_index) == 1) {
  pip_dsd <- pip_structure_list[[pip_structure_index]]
  print(paste("Using PIP structure ID:", pip_structure_ids[pip_structure_index]))
} else {
  stop("DSD_PIP not found or multiple matches.")
}

# 1.3.3 Extract dimensions
pip_dimensions <- pip_dsd@Components@Dimensions
pip_dimension_ids <- sapply(pip_dimensions, function(x) x@conceptRef)
print("PIP dimension conceptRefs:")
print(pip_dimension_ids)

# 1.3.4 Extract codelists
pip_codelists <- pip_structure@codelists@codelists
pip_codelist_ids <- as.data.frame(sapply(pip_codelists, function(cl) cl@id))
print("Available PIP codelist IDs:")
print(pip_codelist_ids)

# 1.3.5 Helper function
extract_pip_codelist_df <- function(id) {
  idx <- which(pip_codelist_ids == id)
  if (length(idx) == 0) return(NULL)
  codes <- pip_codelists[[idx]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

# 1.3.6 Extract key values
pip_country_values     <- extract_pip_codelist_df("CL_PIP_COUNTRY")
pip_indicator_values   <- extract_pip_codelist_df("CL_PIP_INDICATOR")
pip_currency_values    <- extract_pip_codelist_df("CL_PIP_CURRENCY")
pip_currency_den_values <- extract_pip_codelist_df("CL_PIP_CURRENCY_DENOMINATION")
pip_freq_values        <- extract_pip_codelist_df("CL_FREQ")
pip_instrument_values  <- extract_pip_codelist_df("CL_PIP_DV_TYPE")

# 1.3.7 Extract sector disaggregation
sector_indices <- which(pip_codelist_ids == "CL_SECTOR")

pip_reporting_sector_values <- {
  codes <- pip_codelists[[sector_indices[1]]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

pip_counterpart_sector_values <- {
  codes <- pip_codelists[[sector_indices[2]]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

# 1.3.8 Extract accounting entry disaggregation
accounting_indices <- which(pip_codelist_ids == "CL_ACCOUNTING_ENTRY")

pip_acconting_values <- {
  codes <- pip_codelists[[accounting_indices[1]]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

pip_ac_entry_values <- {
  codes <- pip_codelists[[accounting_indices[2]]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

# 1.3.9 Clean up
rm(pip_structure, pip_structure_list, pip_structure_ids, pip_structure_index, pip_dsd, pip_dimensions, pip_dimension_ids, pip_codelists, pip_codelist_ids, 
   sector_indices, accounting_indices,
   codes)
rm(extract_pip_codelist_df)
gc()

# 1.4 WEO METADATA EXTRACTION ---------------------------------------------------

# 1.4.1 Load structure
weo_structure <- readSDMX(providerId = "IMF_DATA", resource = "datastructure", flowRef = "WEO")
print("WEO structure loaded")

# 1.4.2 Isolate DSD
weo_structure_list <- weo_structure@datastructures@datastructures
weo_structure_ids <- sapply(weo_structure_list, function(x) x@id)
weo_structure_index <- which(weo_structure_ids == "DSD_WEO")
if (length(weo_structure_index) == 1) {
  weo_dsd <- weo_structure_list[[weo_structure_index]]
  print(paste("Using weo structure ID:", weo_structure_ids[weo_structure_index]))
} else {
  stop("DSD_WEO not found or multiple matches.")
}

# 1.4.3 Extract dimensions
weo_dimensions <- weo_dsd@Components@Dimensions
weo_dimension_ids <- as.data.frame(sapply(weo_dimensions, function(x) x@conceptRef))
print("WEO dimension conceptRefs:")
print(weo_dimension_ids)

# 1.4.4 Extract codelists
weo_codelists <- weo_structure@codelists@codelists
weo_codelist_ids <- as.data.frame(sapply(weo_codelists, function(cl) cl@id))
print("Available weo codelist IDs:")
print(weo_codelist_ids)

# 1.4.5 Helper function
extract_weo_codelist_df <- function(id) {
  idx <- which(weo_codelist_ids == id)
  if (length(idx) == 0) return(NULL)
  codes <- weo_codelists[[idx]]@Code
  data.frame(
    id = sapply(codes, function(x) x@id),
    label = sapply(codes, function(x) x@name$en),
    stringsAsFactors = FALSE
  )
}

# 1.4.6 Extract key values
weo_country_values     <- extract_weo_codelist_df("CL_WEO_COUNTRY")
weo_indicator_values   <- extract_weo_codelist_df("CL_WEO_INDICATOR")
weo_freq_values        <- extract_weo_codelist_df("CL_FREQ")

# 1.4.7 Clean up
rm(weo_structure, weo_structure_list, weo_structure_ids, weo_structure_index, weo_dsd, weo_dimensions, weo_dimension_ids, weo_codelists, weo_codelist_ids)
rm(extract_weo_codelist_df)
gc()

# 1.5 COUNTRY SET -----

# 1.5.1 Load codelist
data("codelist")

# 1.5.1 Add FMI-WEO groups

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

# 1.5.2 Define WEO group lists and full set
weo_group_lists <- list(
  advanced = advanced_economies,
  emerging_asia = emerging_asia,
  emerging_europe = emerging_europe,
  emerging_latam = emerging_latam,
  emerging_meca = emerging_meca,
  emerging_ssafrica = emerging_ssafrica
)

emerging_economies <- unlist(weo_group_lists[names(weo_group_lists) != "advanced"], use.names = FALSE)
imf_weo_countries <- c(weo_group_lists$advanced, emerging_economies)

# 1.5.3 Standardize valid ISO3 and names
valid_iso3c <- na.omit(unique(codelist$iso3c))
valid_names <- na.omit(unique(codelist$country.name.en))


# 1.5.4 Identify WEO countries not in codelist
imf_weo_countries %>%
  as_tibble() %>%
  filter(!(value %in% na.omit(codelist$iso3c)))

# 1.5.5 Join country values of  IMTS, DIP, and PIP
sdmx_country_values <- bind_rows(
  imts_country_values %>% mutate(IMTS = TRUE),
  dip_country_values  %>% mutate(DIP = TRUE),
  pip_country_values  %>% mutate(PIP = TRUE),
  weo_country_values  %>% mutate(WEO = TRUE)
) %>%
  group_by(id, label) %>%
  summarise(
    IMTS = any(IMTS, na.rm = TRUE),
    DIP  = any(DIP, na.rm = TRUE),
    PIP  = any(PIP, na.rm = TRUE),
    WEO  = any(WEO, na.rm = TRUE),
    .groups = "drop"
  )

# 1.5.6 Identify SDMX countries not in codelist via iso3 and names
sdmx_country_values %>%
  filter(!(id %in% valid_iso3c & id != "")) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

sdmx_country_values %>%
  filter(!(label %in% valid_names & id != "")) %>%
  tibble::as_tibble() %>%
  print(n = Inf)

# 1.5.7 Create manual overrides

# Codelist base for manual overrides
codelist_base <- codelist %>% 
  select(country.name.en, iso2c, iso3c, imf, continent, region) %>% 
  mutate(sdmx_code = iso3c, status = "current")

rm(codelist)

# Manual current country overrides with SDMX code
manual_overrides_current <- tibble::tribble(
  ~iso3c, ~iso2c, ~label,                 ~sdmx_code, ~continent, ~region, ~imf, ~replace_target,
  "XKX",  "XK",    "Kosovo, Republic of",               "KOS",    "Europe",   "Europe & Central Asia",      967, "Kosovo",
  "ANT",  "AN",    "Netherlands Antilles",              "ANT",    "Americas", "Latin America & Caribbean",  353, "Netherlands Antilles",
  "CUW",  "1C_355","Curacao & St. Maarten",             "CWX",    "Americas", "Latin America & Caribbean",  355, NA,
  "WBG",  NA,      "West Bank and Gaza",                "WBG",    "Asia",     "Middle East & North Africa", 487, NA, #Will replace Palestinian territories
  "JEY",  "JE",    "Jersey",                            "TX117",  "Europe",   "Europe & Central Asia",      NA,  "Jersey",
  "ESH",  "EH",     "Western Sahara",                   "TX793",  "Africas",   "Middle East & North Africa",NA,  "Western Sahara",
  "BVT",  "BT",     "Bouvet Island",                    "TX865",  "Antartica","Antartica",                  NA,  "Bouvet Island",
  "CCK",  "CC",     "Cocos (Keeling) Islands",          "TX871" , "Oceania",  "East Asia & Pacific",        NA,  "Cocos (Keeling) Islands",
  "CXR",  "CX",     "Christmas Island",                 "CXR",    "Oceania",  "East Asia & Pacific",        NA,  "Christmas Island", #TX814 alternative
  "HDM",  "HM",     "Heard Island and McDonald Islands","TX872",  "Oceania", "East Asia & Pacific",         NA,  "Heard & McDonald Islands",
  "REU",  "RE",     "Réunion",                          "REU",    "Africas",   "RegionSub-Saharan Africa",  696, "Réunion",
  "WLF",  "WF",     "Wallis and Futuna Islands",        "WLF",    "Oceania",  "East Asia & Pacific",        NA,  "Wallis & Futuna",
  "SHN",  "SH",     "St. Helena",                       "SHN",    "Africa",   "Sub-Saharan Africa",         856, "St. Helena",
  "MYT",  "YT",     "Mayotte",                          "MYT",    "Africa",   "Sub-Saharan Africa",         NA,  "Mayotte",
  "IOT",  "IO",     "British Indian Ocean Territory",   "MYT",    "Asia",     "East Asia & Pacific",        NA,  "British Indian Ocean Territory",
  "ATF",  "TF",     "French Southern Territories",      "ATF",    "Africa",   "Sub-Saharan Africa",         NA,  "French Southern Territories"
  ) %>% mutate(status = "current")

# Former countries block (IMF dataset-consistent names)
manual_overrides_former <- tibble::tribble(
  ~iso3c, ~iso2c, ~label,                             ~sdmx_code, ~continent, ~region, ~imf, ~replace_target,
  "CSK",  NA,     "Czechoslovakia",                             "CSK", "Europe",   "Europe & Central Asia",     934, "Czechoslovakia",
  "DDR",  NA,     "German Democratic Republic",                 "DDR", "Europe",   "Europe & Central Asia",     278, "German Democratic Republic",
  "SCG",  NA,     "Serbia and Montenegro",                      "SCG", "Europe",   "Europe & Central Asia",     891, "Serbia and Montenegro",
  "SUN",  NA,     "Union of Soviet Socialist Republics (USSR)", "SUN", "Europe",   "Europe & Central Asia",     810, NA,
  "VDR",  NA,     "Vietnam, Democratic Republic of",            "VDR", "Asia",     "East Asia & Pacific",        NA, NA,  
  "YAR",  NA,     "Yemen Arab Republic",                        "YAR", "Asia",     "Middle East & North Africa", NA, "Yemen Arab Republic",
  "YMD",  NA,     "Yemen, People's Democratic Republic of",     "YMD", "Asia",     "Middle East & North Africa", 720, "Yemen People's Republic",
  "YUG",  NA,     "Yugoslavia, Socialist Federal Republic of",  "YUG", "Europe",   "Europe & Central Asia",      188, "Yugoslavia"
) %>% mutate(status = "former",
             weo_group_major = "Former Countries",
             weo_group_region = "Former Countries")

# Combine manual overides
manual_combined <- bind_rows(manual_overrides_current, manual_overrides_former) %>%
  mutate(country_name = ifelse(is.na(replace_target), label, replace_target))

# 1.5.8 Apply manual overrides
codelist_adjusted <- codelist_base %>%
  filter(!(country.name.en %in% manual_combined$country_name)) %>% # Remove entries with manual override
  bind_rows(manual_combined %>%
              rename(country.name.en = country_name))     # Use manually defined names

# 1.5.9 Keep only countries with valid SDMX codes
codelist_adjusted <- codelist_adjusted %>%
  filter((sdmx_code %in% sdmx_country_values$id)) %>%
  left_join(sdmx_country_values, by = c("sdmx_code" = "id")) %>%
  select(country.name.en,
         iso2c,
         iso3c,
         imf,
         sdmx_code,
         continent,
         region,
         weo_group_major,
         weo_group_region,
         IMTS,
         DIP,
         PIP,
         WEO)


# Assign WEO groups
country_set <- codelist_adjusted %>%
  mutate(
    weo_group_major = case_when(
      !is.na(weo_group_major) ~ weo_group_major,
      iso3c %in% advanced_economies ~ "Advanced Economies",
      iso3c %in% emerging_economies ~ "Emerging Economies",
      TRUE ~ "WEO Not-Monitored"
    ),
    weo_group_region = case_when(
      !is.na(weo_group_region) ~ weo_group_region,
      iso3c %in% advanced_economies ~ "Advanced Economies",
      iso3c %in% emerging_asia      ~ "Emerging and Developing Asia",
      iso3c %in% emerging_europe    ~ "Emerging and Developing Europe",
      iso3c %in% emerging_latam     ~ "Latin America and the Caribbean",
      iso3c %in% emerging_meca      ~ "Middle East and Central Asia",
      iso3c %in% emerging_ssafrica  ~ "Sub-Saharan Africa",
      TRUE                          ~ "WEO Not-Monitored"
    )
  )

# 1.5.10 Clean up

rm(imts_country_values, dip_country_values, pip_country_values, weo_country_values)
rm(codelist_adjusted, codelist_base, valid_iso3c, valid_names, manual_combined, manual_overrides_current, manual_overrides_former)
gc()

# 1.5.10 Save environment

# Save country set
write.csv(country_set, file.path(data_dir, "country_set.csv"), row.names = FALSE, fileEncoding = "UTF-8", na = "")

# Save R environment
save.image(file = file.path(data_dir, "session_checkpoint.RData"))
