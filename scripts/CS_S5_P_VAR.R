#CUNTO SANTANA (20XX)

#5 PVAR

#Data

data <- read_excel("pwt1001.xlsx")
informal_economy_database <- read_excel("informal-economy-database.xlsx")

#Transform from wide to long format
long_data <- informal_economy_database %>%
  pivot_longer(
    cols = -c(Economy, Code) , 
    names_to = "Year",
    values_to = "Value"
  ) 

#Eliminte first column
long_data <- long_data[-1]

#Set names
colnames(long_data) = c("country", "year", "informal")

#Merged dataset
data <- merge(data, long_data, by = c("country", "year"))

#Drop NAs
data_panel <- drop_na(data)


#Panel Var with Interconnectedness Index

#Model 1: Panel with index, TOT and income
pvar_1 <- pvarfeols(dependent_vars = c("index", "ln_tot", "ln_rgdpnapc"), 
                    lags = 1, data = data_panel, panel_identifier = c("country", "year"))

#Model 2: Panel with informal sector, index, TOT and income
pvar_2 <- pvarfeols(dependent_vars = c("informal","index", "ln_tot",  "ln_rgdpnapc"), 
                    lags = 1, data = data_panel, panel_identifier = c("country", "year"))


summary(pvar_1) #Summary of model 1

summary(pvar_2) #Summary of model 2


irf_pvar_1 <- girf(pvar_1, 10, 20) #Generate GIRF for model 1

irf_pvar_2 <- girf(pvar_2, 10, 20) #Generate GIRF for model 2

#Generate Confidence Intervals for model 1
bootstrap_1 <- bootstrap_irf(pvar_1, typeof_irf = c("GIRF"),
                             n.ahead = 10,
                             nof_Nstar_draws = 20,
                             mc.cores = getOption("mc.cores", 4L), 
                             confidence.band = 0.95) 

#Generate Confidence Intervals for model 2
bootstrap_2 <- bootstrap_irf(pvar_2, typeof_irf = c("GIRF"),
                             n.ahead = 10,
                             nof_Nstar_draws = 20,
                             mc.cores = getOption("mc.cores", 4L), 
                             confidence.band = 0.95)

#Plot IRFs
irf_plot_1 <- plot(irf_pvar_1, bootstrap_1)
irf_plot_2 <- plot(irf_pvar_2, bootstrap_2)

se(pvar_1) #Standard errors of model 1
se(pvar_2) #Standard errors of model 2

panelvar::stability(pvar_1) #Stability of model 1
panelvar::stability(pvar_2) #Stability of model 2

fevd_orthogonal(pvar_1) #Variance decomposition of model 1 
fevd_orthogonal(pvar_2) #Variance decomposition of model 2


#Panel Var with Openness

#Model 1.2: Panel with Openness, TOT and income
pvar_1.2 <- pvarfeols(dependent_vars = c("open", "ln_tot", "ln_rgdpnapc"), 
                      lags = 1, data = data_panel, panel_identifier = c("country", "year"))

#Model 2.2: Panel with Informal sector, Openness, TOT and income
pvar_2.2 <- pvarfeols(dependent_vars = c("informal","open", "ln_tot",  "ln_rgdpnapc"), 
                      lags = 1, data = data_panel, panel_identifier = c("country", "year"))


summary(pvar_1.2) #Summary of model 1.2

summary(pvar_2.2) #Summary of model 2.2

irf_pvar_1.2 <- girf(pvar_1.2, 10, 20) #Generate GIRF for model 1.2

irf_pvar_2.2 <- girf(pvar_2.2, 10, 20) #Generate GIRF for model 2.2

#Generate Confidence Intervals for model 1.2
bootstrap_1.2 <- bootstrap_irf(pvar_1.2, typeof_irf = c("GIRF"),
                               n.ahead = 10,
                               nof_Nstar_draws = 20,
                               mc.cores = getOption("mc.cores", 4L), 
                               confidence.band = 0.95)

#Generate Confidence Intervals for model 2.2
bootstrap_2.2 <- bootstrap_irf(pvar_2.2, typeof_irf = c("GIRF"),
                               n.ahead = 10,
                               nof_Nstar_draws = 20,
                               mc.cores = getOption("mc.cores", 4L), 
                               confidence.band = 0.95)

#Plot IRFs
irf_plot_1.2 <- plot(irf_pvar_1.2, bootstrap_1.2)
irf_plot_2.2 <- plot(irf_pvar_2.2, bootstrap_2.2)


se(pvar_1.2) #Standard errors of model 1.2
se(pvar_2.2) #Standard errors of model 2.2

panelvar::stability(pvar_1.2) #Stability of model 1.2
panelvar::stability(pvar_2.2) #Stability of model 2.2

fevd_orthogonal(pvar_1.2) #Variance decomposition of model 1.2
fevd_orthogonal(pvar_2.2) #Variance decomposition of model 2.2

