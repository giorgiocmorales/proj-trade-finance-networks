#CUNTO SANTANA (20XX)

#0 INSTALL AND LOAD PACKAGES

#Install packages if not already

##Install imfr package (out of CRAN)-----
install.packages("devtools")
require(devtools)
install_github("christophergandrud/imfr")

##Load packages-----

#For data management
library(tidyverse) #For syntax ease
library(imfr) #Download imfr data
library(BISdata) #Download BIS data
library(countrycode) #Country data
library(pbapply) #For progress bar
library(purrr)
library(moments)

#For network analysis
library(igraph)
library(BBmisc) #Miscelanous functions for normalization

#For visualization
library(ggraph)
library(circlize)
library(cowplot)
library(gridExtra)
library(grid)
library(ggpubr)
library(gridGraphics)
library(ggplotify)
library(ggrepel)

#For markdown
library(tinytex)
library(extrafont)
library(rmdwc)
library(knitr)
library(kableExtra)
library(moments)
library(apaTables)
library(tinylabels)
library(papaja)
library(dplyr)
library(tidyr)

#PVAR
library(plm)
library(stargazer)
library(panelvar)
library(pwt10)

#Set path to import/save files
setPath <- dirname(rstudioapi::getSourceEditorContext()$path) #Gets the folder name where the directory of this script is located

#Set working directory on path
setwd(setPath)
getwd()