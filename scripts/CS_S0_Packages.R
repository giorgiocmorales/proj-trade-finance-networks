#CUNTO SANTANA (20XX)

#0 INSTALL AND LOAD PACKAGES

# 0.1 Function to check and load packages
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

packages <- c("rsdmx", "remotes", #Data download
              "tidyverse", "BISdata", "countrycode", "pbapply", "purrr", "moments", #Data management packages
              "igraph", "BBmisc", #For network analysis
              "ggraph", "circlize", "cowplot", "gridExtra", "grid", "ggpubr", "gridGraphics", "ggplotify", "ggrepel", #For visualization
              "tinytex", "extrafont", "rmdwc", "knitr", "kableExtra", "apaTables", "tinylabels", #For R-Markdown formatting
              "papaja", "dplyr", "tidyr", "plm", "stargazer", "panelvar", "pwt10" #For performing P-VAR
              ) 

# 0.2 Load all packages
load_packages(packages)
rm(packages)
rm(load_packages)

# 0.2

# Root directory of the Git project
project_dir <- system("git rev-parse --show-toplevel", intern = TRUE)

# Paths
data_dir <- file.path(project_dir, "data")
outputs_dir <- file.path(project_dir, "outputs")

# Ensure folders exist (optional but safe)
dir.create(data_dir, showWarnings = FALSE)
dir.create(outputs_dir, showWarnings = FALSE)


# 0.3 Set the path to import/save files based on the script’s directory
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setPath <- dirname(rstudioapi::getSourceEditorContext()$path) # Get the folder of the current script
  setwd(setPath)
  print(paste("Working directory set to:", getwd()))
} else {
  warning("rstudioapi package not found. Please set the working directory manually.")
}