library(rgdal)
library(readxl)
library(INLA)
library(spdep)

setwd("C:/Users/Dilek/OneDrive/Documents/GitHub/cprit_2020/visualizations/dashboard_1")

countyAllYearsSIRAll <- read_excel("all_years_by_hist.xlsx", sheet = 1)
countyAllYearsSIRAdeno <- read_excel("all_years_by_hist.xlsx", sheet = 2)
countyAllYearsSIRSmall <- read_excel("all_years_by_hist.xlsx", sheet = 3)
countyAllYearsSIRSquamous <- read_excel("all_years_by_hist.xlsx", sheet = 4)
countyAllYearsSIROther <- read_excel("all_years_by_hist.xlsx", sheet = 5)

#Read mqp data and create a neighborhood/ adjacency matrix 
map_data <- readOGR(dsn = "Texas_Counties")
tx_nb <- poly2nb(map_data)

#Construct C matrix for Leroux Model
Q <- Diagonal(x = sapply(tx_nb, length))
for(i in 2:nrow(map_data)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}
C <- Diagonal(x = 1, n = nrow(map_data)) - Q

#Prep SIR data for INLA
county_SIRs <- countyAllYearsSIRAll
county_SIRs <- data.frame(county_SIRs)
county_SIRs$County_Code <- as.factor(county_SIRs$County_Code)
county_SIRs$Observed <- as.integer(county_SIRs$Observed)
county_SIRs$idarea <- as.numeric(county_SIRs$County_Code)
county_SIRs$idarea1 <- county_SIRs$idarea
county_SIRs$idtime <- 1 + county_SIRs$Year- min(county_SIRs$Year)
county_SIRs$idtime1 <- county_SIRs$idtime
county_SIRs$e <- 1:nrow(county_SIRs) 

#Model 1: BYM, seperating spatiotemporal effects 
formula1 <- Observed ~ 
  f(idarea, model = "bym", graph = tx_nb) +
  f(idarea1, idtime1, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res1 <- inla(formula1, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

#Model 2: BYM
formula2 <- Observed ~ 
  f(idarea, model = "bym", graph = tx_nb) + 
  f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res2 <- inla(formula2, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

#Model 3: Besag
formula3 <- Observed ~ 
  f(idarea, model = "besag", graph = tx_nb) + 
  f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res3 <- inla(formula3, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

#Model 4: Besag Proper
formula4 <- Observed ~ 
  f(idarea, model = "besagproper", graph = tx_nb) + 
  f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res4 <- inla(formula4, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

# Model 5: Leroux Model by methods outlined in chapter 7.2 of https://becarioprecario.bitbucket.io/inla-gitbook/ch-spatial.html#sec:lattice
formula5 <- Observed ~ 
  f(idarea, model = "generic1", Cmatrix = C) + 
  f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res5 <- inla(formula5, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

# Model 6: Time as a linear covariate 
formula6 <- Observed ~ idtime +
  f(idarea, model = "generic1", Cmatrix = C) + 
  f(e, model = "iid") + 
  f(idtime1, model = "iid", constr = T) 
res6 <- inla(formula6, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

# Model 7: Gaussian w/ log of the response variable 
formula7 <- (log(Observed+0.000005)) ~ 
  f(idarea, model = "generic1", Cmatrix = C) + 
  f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res7 <- inla(formula7, 
             family = "gaussian", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

res1$waic$waic
res1$dic$dic
-sum(log(res1$cpo$cpo))

res2$waic$waic
res2$dic$dic
-sum(log(res2$cpo$cpo))

res3$waic$waic
res3$dic$dic
-sum(log(res3$cpo$cpo))

res4$waic$waic
res4$dic$dic
-sum(log(res4$cpo$cpo))

res5$waic$waic
res5$dic$dic
-sum(log(res5$cpo$cpo))

res6$waic$waic
res6$dic$dic
-sum(log(res6$cpo$cpo))

res7$waic$waic
res7$dic$dic
-sum(log(res7$cpo$cpo))
