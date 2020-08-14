library(rgdal)
library(sf)
library(readxl)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidyr)

#Set working directory to GitHub folder
setwd("C:/Users/Dilek/OneDrive/Documents/GitHub/cprit_2020")


# Extract TX map data for plotting 
map_data <- readOGR(dsn = "Analysis/Shapefile Geographic Data/Texas_Counties")
# Extract TX data regarding 1995 lung cancer SIR's
county_SIRs <- read_excel("Analysis/Clean Data for Plots/Every Year 1995-2015/All by Hist Type.xlsx", sheet = 1)
# sheet = 1 --> All histologic types
# sheet = 2 --> Adenocarcinoma
# sheet = 3 --> Small Cell Carcinoma
# sheet = 4 --> Squamous Cell Carcinoma
# sheet = 5 --> Other Non-small Cell Carcinoma

#Reshape to prep the data over time to be merged with the map data
county_SIRs <- data.frame(county_SIRs)
wide_SIRs <- reshape(county_SIRs,
                     timevar = "Year",
                     idvar = "County_Code",
                     direction = "wide")

#Merge two datasets
tx_SIR_map <- merge(map_data, wide_SIRs, by.x = "FIPS_ST_CN", by.y = "County_Code")  

#Convert to sf object for plotting
tx_SIR_map_sf <- st_as_sf(tx_SIR_map)
#tx_SIR_map_sf <- gather(tx_SIR_map_sf, Year, SIR, paste0("SIR.", c(1995, 2000, 2005, 2010, 2015)))
#tx_SIR_map_sf$Year <- as.integer(substring(tx_SIR_map_sf$Year, 5, 8))

#### PLOTS ####
"
#SIR's over time across counties
plot1 <- ggplot(tx_SIR_map_sf) + geom_sf(aes(fill = SIR)) +
  facet_wrap(~Year, dir ='h', ncol = 3) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(midpoint = 1, low = 'blue', mid = 'white', high = 'red')
plot1
"

#### INLA MODELING ####
library(INLA)
library(spdep)
#Create a neighborhood matrix 
tx_nb <- poly2nb(map_data)
nb2INLA("tx_SIR_map.adj", tx_nb)
tx_g <- inla.read.graph(filename = "tx_SIR_map.adj")

#Prep for INLA by completing the dataset
#colnames(county_SIRs)[5] <- "Y"
#colnames(county_SIRs)[4] <- "E"
county_SIRs$County_Code <- as.factor(county_SIRs$County_Code)
county_SIRs$idarea <- as.numeric(county_SIRs$County_Code)
county_SIRs$idarea1 <- county_SIRs$idarea
county_SIRs$idtime <- 1 + county_SIRs$Year- min(county_SIRs$Year)
county_SIRs$idtime1 <- county_SIRs$idtime
county_SIRs$Observed <- as.integer(county_SIRs$Observed)
county_SIRs$e <- 1:nrow(county_SIRs) 

#Define the model
formula1 <- Observed ~ f(idarea, model = "bym", graph = tx_g) + f(e, model = "iid") + 
  f(idtime, model = "rw2", constr = T) 


#Run INLA
res1 <- inla(formula1, 
             family = "poisson", data = county_SIRs, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))  

RR <- res1$summary.fitted.values
ind <- which(county_SIRs$idarea == 1)
RR1 <- RR[ind, 'mean']
plot(SIR1, type="b")
summary(RR)
res()$summary.fitted.values[vec, 'mean']
SIR1 <- county_SIRs$SIR[ind]
SIR1
points(RR1, col=2, pch=19)
e <- res1$summary.random$e 
time.random <- res1$summary.random$idtime 

tail(e)
range <- range(c(RR, county_SIRs$SIR))
plot(RR, county_SIRs$SIR, xlim=range, ylim=range)


interaction.random <- res1$summary.random$idtime[, 'mean'] 


#does this plot mean that the average relative risk has increased over time? 
plot(res1$summary.random$idtime$mean)
res1$waic$waic
res1$dic$dic
res1$cpo$cpo
#Explain intercept ?

ggplot(data.frame(inla.smarginal(res1$marginals.fixed$`(Intercept)`)),aes(x=x,y=y)) + geom_line() +theme_bw()



#Why is there 254 points for one and 508 for the other? Correspond to counties?
plot(res1$summary.random$idarea1$mean)
plot(res1$summary.random$idarea$mean)

#Drop down where user can select a county and get information about SIR, etc. there,
# how would I get the average random effect 

#Code for creating INLA plots given the reactive object
county_SIRs <- data.frame(county_SIRs)
county_SIRs$RR <- res1$summary.fitted.values[, "mean"]
wide_SIRs <- reshape(county_SIRs,
                     timevar = "Year",
                     idvar = "County_Code",
                     direction = "wide")
tx_SIR_map <- merge(map_data, wide_SIRs, by.x = "FIPS_ST_CN", by.y = "County_Code")  
#Convert to sf object for plotting
tx_SIR_map_sf <- st_as_sf(tx_SIR_map)
tx_RR_map_sf <- gather(tx_SIR_map_sf, Year, RR, paste0("RR.", c(1995:2015)))
tx_RR_map_sf$Year <- as.integer(substring(tx_RR_map_sf$Year, 4, 7))
ggplot(tx_RR_map_sf) + geom_sf(aes(fill=RR)) + facet_wrap(~Year, dir = "h", ncol = 7)

