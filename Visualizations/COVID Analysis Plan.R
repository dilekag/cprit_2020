library(INLA)
library(sf)
library(rgdal)
library(readxl)
library(spdep)

setwd('C:/Users/Dilek/OneDrive/Documents/GitHub/cprit_2020/Visualizations/dashboard_1')
countySIRAll <- data.frame(read_excel("county_sir_data.xlsx", sheet = 1))
map_data <- readOGR(dsn = "Texas_Counties")

#user inputted 
data <- read_excel("covid_dashboard_data.xlsx", sheet = 2)

#Model 2: Based on SIR's
cancer_data_2015 <- subset(countySIRAll, Year == 2015)
tx_data_map <- merge(map_data,cancer_data_2015, by.x = 'FIPS_ST_CN', by.y = 'County_Code')
tx_data_map <- merge(tx_data_map, data, by.x = 'FIPS_ST_CN', by.y = 'County_Code')
tx_nb <- poly2nb(tx_data_map)
nb2INLA('tx_SIR_map.adj', tx_nb)
tx_g <- inla.read.graph(filename = 'tx_SIR_map.adj')
tx_data_map$idareau <- 1:nrow(tx_data_map@data)
tx_data_map$idareav <- 1:nrow(tx_data_map@data)
tx_data_map$Observed <- as.integer(tx_data_map$Observed)
formula <- Observed ~ Rate +
  f(idareau, model = "besag", graph = tx_g, scale.model = TRUE) +
  f(idareav, model = "iid")
res <- inla(formula, family = "poisson", data = tx_data_map@data, 
            E = Expected, control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), control.predictor = list(compute=TRUE))
res

