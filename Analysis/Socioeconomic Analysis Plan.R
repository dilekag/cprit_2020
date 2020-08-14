library(rgdal)
library(sf)
library(readxl)
library(ggplot2)
library(leaflet)
library(tidyr)
library(spdep)

#Set working directory to GitHub folder
setwd("GitHub/cprit_2020/visualizations/dashboard_1")


# Extract TX map data for plotting 
map_data <- readOGR(dsn = "Texas_Counties")
# Extract TX data regarding 1995 lung cancer SIR's
data <- read_excel("county_sir_data.xlsx", sheet = 1)
# sheet = 1 --> All histologic types
# sheet = 2 --> Adenocarcinoma
# sheet = 3 --> Small Cell Carcinoma
# sheet = 4 --> Squamous Cell Carcinoma
# sheet = 5 --> Other Non-small Cell Carcinoma
rurality <- read_excel("socioeconomic_data.xlsx", sheet = 1)
poverty <- read_excel("socioeconomic_data.xlsx", sheet = 2)

tx_nb <- poly2nb(map_data)
Q <- Diagonal(x = sapply(tx_nb, length))
for(i in 2:nrow(map_data)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}
C <- Diagonal(x = 1, n = nrow(map_data)) - Q

soci_data <- merge(data, rurality, by = c("County_Code","Year"))
soci_data <- merge(soci_data, poverty, by = c("County_Code","Year"))
soci_data <- data.frame(soci_data)
soci_data$County_Code <- as.factor(soci_data$County_Code)
soci_data$idarea <- as.numeric(soci_data$County_Code)
soci_data$e <- 1:nrow(soci_data)
soci_data$idtime <- 1 + soci_data$Year- min(soci_data$Year)
soci_data$Observed <- as.integer(soci_data$Observed)



formula <- Observed ~ Rurality_Score + Poverty_Rate +
  f(idarea, model = "generic1", Cmatrix = C) + #mat_c()
  f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 
res <- inla(formula, 
   family = "poisson", data = soci_data, E = Expected,
   control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))

marginal_data <- res$marginals.fixed$Poverty_Rate
ggplot(data.frame(inla.smarginal(marginal_data)), aes(x,y)) + geom_line() + theme_bw()
ggplot(data.frame(inla.smarginal(marginal_data)), aes(x,y)) + geom_line() + 
  geom_area(data = subset(data.frame(inla.smarginal(marginal_data)), x < 0), fill = "black") + theme_bw()
inla.dmarginal(-0.01, marginal_data)

#Preliminary plot before the slider with "1995" as an example input for the year 
county_SIRs_1995 <- data.frame(subset(county_SIRs, Year == "1995"))
county_SIRs_sf_1995 <- st_as_sf(merge(map_data, county_SIRs_1995, by.x = "FIPS_ST_CN", by.y = "County_Code"))
ggplot(county_SIRs_sf_1995) + geom_sf(aes(fill = Rurality.Score)) + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient("Rurality Score")

#Reshape to prep the data over time to be merged with the map data
wide_SIRs <- reshape(county_SIRs,
                     timevar = "Year",
                     idvar = "County_Code",
                     direction = "wide")

#Merge two datasets
tx_SIR_map <- merge(map_data, wide_SIRs, by.x = "FIPS_ST_CN", by.y = "County_Code")  

#Convert to sf object for plotting
tx_SIR_map_sf <- st_as_sf(tx_SIR_map)
tx_SIR_map_sf <- gather(tx_SIR_map_sf, Year, SIR, paste0("SIR.", c(1995, 2000, 2005, 2010, 2015)))
tx_SIR_map_sf$Year <- as.integer(substring(tx_SIR_map_sf$Year, 5, 8))

#### INLA MODELING ####
library(INLA)
library(spdep)
#Create a neighborhood matrix 
tx_nb <- poly2nb(tx_SIR_map)
#nb2INLA("tx_SIR_map.adj", tx_nb)
#tx_g <- inla.read.graph(filename = "tx_SIR_map.adj")

#Prep for INLA by completing the dataset
county_SIRs$County_Code <- as.factor(county_SIRs$County_Code)
county_SIRs$idarea <- as.numeric(county_SIRs$County_Code)
county_SIRs$idarea1 <- county_SIRs$idarea
county_SIRs$idtime <- 1 + county_SIRs$Year- min(county_SIRs$Year)
county_SIRs$idtime1 <- county_SIRs$idtime
county_SIRs$Y <- as.integer(county_SIRs$Y)

#Define the model
formula1 <- Y ~ Rurality.Score +
  f(idarea, model = "bym", graph = tx_nb) +
  f(idarea1, idtime1, model = "iid") + f(idtime, model = "rw1") 

#Run INLA
res1 <- inla(formula1, 
             family = "poisson", data = county_SIRs, E = E,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))  

#does this plot mean that the average relative risk has increased over time? 
plot(res1$summary.random$idtime$mean)
plot(res1$marginals.fixed$Rurality.Score)
res1$waic$waic
res1$dic$dic
res1$cpo$cpo

