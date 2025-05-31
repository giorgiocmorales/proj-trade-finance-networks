#CUNTO SANTANA (20XX)

#1 IMPORT DATASETS

#Clear data
rm(list = ls())

#Available datasetsç

install.packages("httr") # Ensure HTTP requests work properly
install.packages("jsonlite") # Required for parsing JSON responses
install.packages("rsdmx") # For SDMX data retrieval
library(httr)
library(jsonlite)
library(rsdmx)

# Test connection
response <- GET("https://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow/IMF")

# Check response status
print(response$status_code)  # Should return 200 if successful
print(content(response, "text"))


