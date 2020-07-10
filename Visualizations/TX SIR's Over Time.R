library(rgdal)
library(sf)
library(readxl)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidyr)

setwd("~/")
# Extract TX map data for plotting 
map_data <- readOGR(dsn = "GitHub/cprit_2020/Analysis/Shapefile Geographic Data/Texas_Counties")
# Extract TX data regarding 1995 lung cancer SIR's
county_SIRs <- read_excel("GitHub/cprit_2020/Analysis/Clean Data for Plots/County SIRs All Years.xlsx")
#Reshape to prep the data over time to be merged with the map data
county_SIRs <- data.frame(county_SIRs)
wide_SIRs <- reshape(county_SIRs,
             timevar = "Year",
             idvar = "County_Code",
             direction = "wide")
wide_SIRs[1:2,]
#Merge two datasets
tx_SIR_map <- merge(map_data, wide_SIRs, by.x = "FIPS_ST_CN", by.y = "County_Code")  
#Convert to sf object for plotting
tx_SIR_map_sf <- st_as_sf(tx_SIR_map)
tx_SIR_map_sf <- gather(tx_SIR_map_sf, Year, SIR, paste0("SIR.", c(1995, 2000, 2005, 2010, 2015)))
tx_SIR_map_sf$Year <- as.integer(substring(tx_SIR_map_sf$Year, 5, 8))

#### PLOTS ####

#SIR's over time across counties
plot1 <- ggplot(tx_SIR_map_sf) + geom_sf(aes(fill = SIR)) +
  facet_wrap(~Year, dir = "h", ncol = 3) +
  ggtitle("SIR's Over Time") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red")
plot1
#Time Plot of SIR's
plot2 <- ggplot(county_SIRs, aes(x= Year, y = SIR,
                                 group = County_Code, color = County_Code)) +
  geom_line() + geom_point(size = 2) + theme_bw() + theme(legend.position = "none")
plot2 <- ggplotly(plot2)
plot2

#### INLA MODELING ####
library(INLA)
library(spdep)
#Create a neighborhood matrix 
tx_nb <- poly2nb(tx_SIR_map)
nb2INLA("tx_SIR_map.adj", tx_nb)
tx_g <- inla.read.graph(filename = "tx_SIR_map.adj")

#Prep for INLA by completing the dataset
colnames(county_SIRs)[5] <- "Y"
colnames(county_SIRs)[4] <- "E"
county_SIRs$County_Code <- as.factor(county_SIRs$County_Code)
county_SIRs$idarea <- as.numeric(county_SIRs$County_Code)
county_SIRs$idarea1 <- county_SIRs$idarea
county_SIRs$idtime <- 1 + county_SIRs$Year- min(county_SIRs$Year)
county_SIRs$Y <- as.integer(county_SIRs$Y)
formula1 <- Y ~ f(idarea, model = "bym", graph = tx_g) +
  f(idarea1, idtime, model = "iid") + idtime
res1 <- inla(formula1, 
            family = "poisson", data = county_SIRs, E = E,
            control.predictor = list(compute = TRUE))  
county_SIRs$RR <- res1$summary.fitted.values[, "mean"]
county_SIRs$LL <- res1$summary.fitted.values[, "0.025quant"]
county_SIRs$UL <- res1$summary.fitted.values[, "0.975quant"]  
summary(res1)
### Plots After INLA ###
tx_SIR_map_sf <- merge(tx_SIR_map_sf, county_SIRs,
                       by.x = c("FIPS_ST_CN", "Year"),
                       by.y = c("County_Code", "Year"))

plot3 <- ggplot(tx_SIR_map_sf) + geom_sf(aes(fill = RR)) +
  facet_wrap(~Year, dir = "h", ncol = 3) +
  ggtitle("SIR's Over Time After INLA Smoothing") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red")
plot3

