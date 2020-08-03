library(rgdal)
library(spdep)

#Leroux Model by methods outlined in chapter 7.2 of https://becarioprecario.bitbucket.io/inla-gitbook/ch-spatial.html#sec:lattice


setwd("C:/Users/Dilek/OneDrive/Documents/GitHub/cprit_2020/visualizations/dashboard_1")
county_SIRs <- read_excel("all_years_by_hist.xlsx", sheet = 1)
county_SIRs <- data.frame(county_SIRs)
tx_map_data <- readOGR(dsn = "Texas_Counties")

tx_nbrs <- poly2nb(tx_map_data) 

wide_SIRs <- reshape(county_SIRs,
                     timevar = "Year",
                     idvar = "County_Code",
                     direction = "wide")
tx_SIR_map <- merge(tx_map_data, wide_SIRs, by.x = "FIPS_ST_CN", by.y = "County_Code")  

county_SIRs <- data.frame(county_SIRs)
county_SIRs$County_Code <- as.factor(county_SIRs$County_Code)
county_SIRs$Observed <- as.integer(county_SIRs$Observed)

county_SIRs$idarea <- as.numeric(county_SIRs$County_Code)
county_SIRs$idarea1 <- county_SIRs$idarea
county_SIRs$idtime <- 1 + county_SIRs$Year- min(county_SIRs$Year)
county_SIRs$idtime1 <- county_SIRs$idtime

Q <- Diagonal(x = sapply(tx_g, length))
for(i in 2:nrow(tx_map_data)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}
C <- Diagonal(x = 1, n = nrow(tx_map_data)) - Q

formula5 <- Observed ~ 
  f(idarea, model = "generic1", Cmatrix = C) + 
  f(idarea1, idtime1, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res5 <- inla(formula5, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

