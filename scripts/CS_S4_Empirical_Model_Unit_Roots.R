#CUNTO SANTANA (20XX)

# 4 EMPIRICAL MODEL UNIT ROOT TESTS

#Data----

data <- read_excel("pwt1001.xlsx")
informal_economy_database <- read_excel("informal-economy-database.xlsx")

#Transform from wide to long format----
long_data <- informal_economy_database %>%
  pivot_longer(
    cols = -c(Economy, Code) , 
    names_to = "Year",
    values_to = "Value"
  ) 

long_data <- long_data[-1]

colnames(long_data) = c("country", "year", "informal")

data <- merge(data, long_data, by = c("country", "year"))

data_panel <- drop_na(data)

#Unit root test data----

index_unitroot <- data_panel[,c("country", "year", "index")] %>% pivot_wider(names_from = country, values_from = index)
gdp_unitroot <- data_panel[,c("country", "year", "ln_rgdpnapc")]  %>% pivot_wider(names_from = country, values_from = ln_rgdpnapc)
tot_unitroot <- data_panel[,c("country", "year", "ln_tot")]  %>% pivot_wider(names_from = country, values_from = ln_tot)
IS_unitroot <- data_panel[,c("country", "year", "informal")]  %>% pivot_wider(names_from = country, values_from = informal)
open_unitroot <- data_panel[,c("country", "year", "ln_open")]  %>% pivot_wider(names_from = country, values_from = ln_open)

##Unit root tests----

#Madwu----

index_madwu <- purtest(index_unitroot, pmax = 4, exo = "intercept", test = "madwu") #Stationary 

gdp_madwu <- purtest(gdp_unitroot, pmax = 4, exo = "intercept", test = "madwu") #Stationary 

tot_madwu <- purtest(tot_unitroot, pmax = 4, exo = "intercept", test = "madwu") #Stationary 

IS_madwu <-purtest(IS_unitroot, pmax = 4, exo = "intercept", test = "madwu") #Stationary 

open_madwu <-purtest(open_unitroot, pmax = 4, exo = "intercept", test = "madwu") #Stationary 


madwu <- t(tibble(index_madwu$statistic[[6]], gdp_madwu$statistic[[6]], tot_madwu$statistic[[6]], IS_madwu$statistic[[6]]))

#Invnormal

index_invnormal <- purtest(index_unitroot, pmax = 4, exo = "intercept", test = "invnormal") #Stationary 

gdp_invnormal <- purtest(gdp_unitroot, pmax = 4, exo = "intercept", test = "invnormal") #Stationary 

tot_invnormal <- purtest(tot_unitroot, pmax = 4, exo = "intercept", test = "invnormal") #Stationary 

IS_invnormal <-purtest(IS_unitroot, pmax = 4, exo = "intercept", test = "invnormal") #Stationary

open_invnormal <-purtest(open_unitroot, pmax = 4, exo = "intercept", test = "invnormal") #Stationary

invnormal <- t(tibble(index_invnormal$statistic[[6]], gdp_invnormal$statistic[[6]], tot_invnormal$statistic[[6]], IS_invnormal$statistic[[6]]))

#Logit----

index_logit <- purtest(index_unitroot, pmax = 4, exo = "intercept", test = "logit") #Stationary 

gdp_logit <- purtest(gdp_unitroot, pmax = 4, exo = "intercept", test = "logit") #Stationary 

tot_logit <- purtest(tot_unitroot, pmax = 4, exo = "intercept", test = "logit") #Stationary 

IS_logit <-purtest(IS_unitroot, pmax = 4, exo = "intercept", test = "logit") #Stationary

open_logit <-purtest(open_unitroot, pmax = 4, exo = "intercept", test = "logit") #Stationary


logit <- t(tibble(index_logit$statistic[[6]], gdp_logit$statistic[[6]], tot_logit$statistic[[6]], IS_logit$statistic[[6]]))

#Pm----

index_Pm <- purtest(index_unitroot, pmax = 4, exo = "intercept", test = "Pm") #Stationary 

gdp_Pm <- purtest(gdp_unitroot, pmax = 4, exo = "intercept", test = "Pm") #Stationary 

tot_Pm <- purtest(tot_unitroot, pmax = 4, exo = "intercept", test = "Pm") #Stationary 

IS_Pm <-purtest(IS_unitroot, pmax = 4, exo = "intercept", test = "Pm") #Stationary

open_Pm <-purtest(open_unitroot, pmax = 4, exo = "intercept", test = "Pm") #Stationary


Pm <- t(tibble(index_Pm$statistic[[6]], gdp_Pm$statistic[[6]], tot_Pm$statistic[[6]], IS_Pm$statistic[[6]]))

#Hadri----

index_hadri <- purtest(index_unitroot, hadriax = 4, exo = "intercept", test = "hadri") #Stationary 

gdp_hadri <- purtest(gdp_unitroot, hadriax = 4, exo = "intercept", test = "hadri") #Stationary 

tot_hadri <- purtest(tot_unitroot, hadriax = 4, exo = "intercept", test = "hadri") #Stationary 

IS_hadri <-purtest(IS_unitroot, hadriax = 4, exo = "intercept", test = "hadri") #Stationary

open_hadri <-purtest(open_unitroot, hadriax = 4, exo = "intercept", test = "hadri") #Stationary

hadri <- t(tibble(index_hadri$statistic[[6]], gdp_hadri$statistic[[6]], tot_hadri$statistic[[6]], IS_hadri$statistic[[6]]))


#Table----
Unit_root_tests <- cbind(madwu, invnormal, logit, Pm, hadri)

stargazer(Unit_root_tests, summary = FALSE, digits = 4)