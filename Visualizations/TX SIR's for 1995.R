library(rgdal)
library(sf)
library(readxl)
library(ggplot2)
library(plotly)
library(leaflet)

setwd("~/")
# Extract TX map data for plotting 
map_data <- readOGR(dsn = "GitHub/cprit_2020/Analysis/Shapefile Geographic Data/Texas_Counties")
# Extract TX data regarding 1995 lung cancer SIR's
county_SIR_1995 <- read_excel("GitHub/cprit_2020/Analysis/Clean Data for Plots/County SIRs 1995.xlsx")
#Combine the two datasets by county FIPS code
full <- merge(map_data, county_SIR_1995, by.x = "FIPS_ST_CN", by.y = "County Code")
keeps <- c("CNTY_NM","SIR")
full <- full[keeps]

# Plot 1 (ggplot of SIR's by county):
full_sf <- st_as_sf(full)
plot1 <- ggplot(full_sf) + 
  geom_sf(aes(fill = SIR)) + 
  ggtitle("SIR") + theme_bw() + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  )
plot1

#Plot 2: More interactive version of plot 1
plot2 <- ggplotly(plot1)
plot2

#Plot 3: Using Leaflet
map <- spTransform(full,
            CRS("+proj=longlat +datum=WGS84 +no_defs"))
l <- leaflet(map) %>% addTiles()
pal <- colorNumeric(palette = "YlOrRd", domain = map$SIR)
l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 1
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 1,
    title = "SIR", position = "bottomright"
  )

