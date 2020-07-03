library(rgdal)
library(sf)
library(readxl)
library(ggplot2)

# Extract TX map data for plotting 
map_data <- readOGR(dsn = "GitHub/cprit_2020/Analysis/Shapefile Geographic Data/Texas_Counties")
# Extract TX data regarding 1995 lung cancer SIR's
county_SIR_1995 <- read_excel("GitHub/cprit_2020/Analysis/Plots & Data Analysis/Clean Data for Plots/County SIRs 1995.xlsx")
