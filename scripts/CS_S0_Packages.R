#CUNTO SANTANA (20XX)

#0 INSTALL AND LOAD PACKAGES

# Install devtools if not installed
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
library(devtools)

# Install imfr package (GitHub, outside CRAN) if not installed
if (!requireNamespace("imfr", quietly = TRUE)) install_github("christophergandrud/imfr")
library(imfr)

# Function to check and load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

packages <- c("tidyverse", "BISdata", "countrycode", "pbapply", "purrr", "moments", #Data management packages
              "igraph", "BBmisc", #For network analysis
              "ggraph", "circlize", "cowplot", "gridExtra", "grid", "ggpubr", "gridGraphics", "ggplotify", "ggrepel", #For visualization
              "tinytex", "extrafont", "rmdwc", "knitr", "kableExtra", "apaTables", "tinylabels", #For R-Markdown formatting
              "papaja", "dplyr", "tidyr", "plm", "stargazer", "panelvar", "pwt10" #For performing P-VAR
              ) 

# Load all packages
load_packages(packages)
rm(packages)

# Set the path to import/save files based on the script’s directory
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setPath <- dirname(rstudioapi::getSourceEditorContext()$path) # Get the folder of the current script
  setwd(setPath)
  print(paste("Working directory set to:", getwd()))
} else {
  warning("rstudioapi package not found. Please set the working directory manually.")
}
