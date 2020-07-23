#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(rgdal)
library(sf)
library(tidyr)
library(INLA)
library(spdep)

# Map data
map_data <- readOGR(dsn = "Texas_Counties")
# Rates over time (temporal trend data for general page)
ratesDataAll <- read_excel("rates_data.xlsx", sheet = 1)
ratesDataAdeno <- read_excel("rates_data.xlsx", sheet = 2)
ratesDataSmall <- read_excel("rates_data.xlsx", sheet = 3)
ratesDataSquamous <- read_excel("rates_data.xlsx", sheet = 4)
ratesDataOtherNonSmall <- read_excel("rates_data.xlsx", sheet = 5)
# Rates for different counties (spatial trend for general page)
countyRatesAll <- read_excel("rates_county.xlsx", sheet = 1)
countyRatesAdeno <- read_excel("rates_county.xlsx", sheet = 2)
countyRatesSmall <- read_excel("rates_county.xlsx", sheet = 3)
countyRatesSquamous <- read_excel("rates_county.xlsx", sheet = 4)
countyRatesOtherNonSmall <- read_excel("rates_county.xlsx", sheet = 5)
#SIR data by county 
countySIRAll <- read_excel("county_sir_data.xlsx", sheet = 1)
countySIRAdeno <- read_excel("county_sir_data.xlsx", sheet = 2)
countySIRSmall <- read_excel("county_sir_data.xlsx", sheet = 3)
countySIRSquamous <- read_excel("county_sir_data.xlsx", sheet = 4)
countySIROther <- read_excel("county_sir_data.xlsx", sheet = 5)

# UI main dashboard
ui <- dashboardPage(
    dashboardHeader(
        title = "Spatiotemporal & Socioeconomic Lung Cancer Relationships in Texas Between 1995 and 2015",
        titleWidth = 900),
    # Sidebar Content
    dashboardSidebar(
        sidebarMenu(
            menuItem("General Trends", tabName ="general", icon = icon("general")),
            menuItem("SIR Plots & INLA Modeling", tabName ="sir", icon = icon("sir")),
            menuItem("Socioeconomic Associations", tabName ="covar", icon = icon("covar")),
            menuItem("COVID-19 and Lung Cancer", tabName ="covid", icon = icon("covid"))
        )
    ),
    # Main Body
    dashboardBody(
        tabItems(
            # First Tab
            tabItem(tabName = "general",
                      tags$h3(
                        "Currently, lung cancer is the most common type of cancer in the world, accounting for over 2 million cases worldwide in 2018.
                        In the United States, approximately 140,000 people are projected to 
                        die from the disease in 2020. The goal of this study was to model the 
                        spatial (across different counties), temporal (over time), and 
                        spatiotemporal relationships of lung cancer. In addition, COVID-19 and 
                        other socioeconomic factors were investigated to determine if there were 
                        associations. All cancer data was from the Texas Cancer Registry and 
                        processed via the SEER*Stat software. "
                      ),
                        tags$h5("There are multiple classifications of lung cancer called 'histologic types' based on
                        the appearance of the cancerous cell under a microscope. Each has a unique etiology, therefore 
                        affecting people differently, meaning each should be studied individually. The gray boxes 
                        in the diagram on the right show the divisions of and relative prevalence 
                        of the four histologic types included in this analysis."),
                        tags$img(align = "center", height = 150, width = 350, src = 'hist_types.png'),
                        tags$h5("On this 'General Trends' page of the dashboard, the first set of plots show the", tags$strong("spatial"), "relationships of lung cancer in order to visualize where there may be counties/ regions with abnormally high rates of specific lung cancers. The second set of plots show the", tags$strong("temporal"), "trends of different lung cancer types between 1995 and 2015. In addition, through the selection of inputs, the trends regarding specific age and/or gender groups can be investigated."),
                        tags$br(),
                    fluidRow(
                        tabBox(
                            title = "County-Level Lung Cancer Rates per 100k Across Texas by Histologic Type",
                            id = "generalplot", width = 12,
                            tabPanel("All", plotOutput("allCountyRatesPlot")),
                            tabPanel("Adenocarcinoma", plotOutput("adenoCountyRatesPlot")),
                            tabPanel("Small Cell Carcinoma", plotOutput("smallCountyRatesPlot")),
                            tabPanel("Squamous Carcinoma", plotOutput("squamousCountyRatesPlot")),
                            tabPanel("Non-Small Cell Carcinomas", plotOutput("nonsmallCountyRatesPlot"))                            
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Inputs for Plot Below", status = "warning", solidHeader = TRUE, width = 12,
                            checkboxGroupInput("gender", "Please Select Gender(s):", c("Male","Female")),
                            checkboxGroupInput("age", "Please Select Age Group(s):", c("<55 Years","55-74 Years", "75+ Years"))
                        )),
                    fluidRow(
                        tabBox(
                            title = "Lung Cancer Rates per 100k in Texas Over Time by Age, Gender & Histologic Type",
                            id = "generalplot", width = 12,
                            tabPanel("All", plotOutput("allRatesPlot")),
                            tabPanel("Adenocarcinoma", plotOutput("adenoRatesPlot")),
                            tabPanel("Small Cell Carcinoma", plotOutput("smallRatesPlot")),
                            tabPanel("Squamous Carcinoma", plotOutput("squamousRatesPlot")),
                            tabPanel("Non-Small Cell Carcinomas", plotOutput("nonsmallRatesPlot")),
                            tags$figcaption(tags$em("The blue line above is the best-fit line via the lowess() 
                                function given a span of 0.6 years"))
                        )
                    ),
                ),
            # Second Tab
            tabItem(tabName = "sir",
                    fluidRow(
                        box(title = "Inputs", status = "warning", solidHeader = TRUE, width = 12,
                            radioButtons("cancerType", "Please Select the Histologic Lung Cancer Type(s) to be Investigated:", c("All", "Adenocarcinoma", "Small Cell Carcinoma", "Squamous Cell Carcinoma", "Other Non-Small Cell Carcinomas")),
                            selectInput("dataType", "Please Select the Desired Metric for Analysis:", c("Expected", "Observed", "SIR"))
                        )),
                    fluidRow(
                        tabBox(
                            title = "Plot of County-Level Lung Cancer in Texas Between 1995 and 2015",
                            id = "sirs", width = 12,
                            tabPanel("County-level Relative Risk After INLA Smoothing via a Bernardinelli Model", plotOutput("inlaPlot")),
                            tabPanel("Data without Modeling/ Smoothing", plotOutput("rawPlot"))
                        )),
                    fluidRow(
                        box(
                            title = "County-Level Analysis Inputs", status = "warning", solidHeader = TRUE, width = 12,
                            #selectInput("countyChoice", "Please select a county to conduct a further analysis for that area")
                        ),
                        box(
                            plotOutput("timeResults")
                        )
                    )
                ),
            # Third Tab
            tabItem(tabName = "covar"),
            # Fourth Tab
            tabItem(tabName = "covid")
        )
    )
)
# Server
server <- function(input, output) {
# General Dashboard Page 1: Rates over Time for Each Histologic Type
    output$allRatesPlot <- renderPlot({
        y <- rep(0,21)
        for (j in input$gender){
            for (k in input$age){
                for (p in colnames(ratesDataAll)){
                    if (paste(j,k) == p){
                        y <- y + ratesDataAll[[p]]
                    }
                }
            }
        }
        x <- c(1995:2015)
        plot(x,y, xlab ="Year",ylab="Rate per 100k People in Texas",col="red")
        lines(lowess(y~x,f=0.6),col = "blue")

    })
    output$adenoRatesPlot <- renderPlot({
        y <- rep(0,21)
        for (j in input$gender){
            for (k in input$age){
                for (p in colnames(ratesDataAdeno)){
                    if (paste(j,k) == p){
                        y <- y + ratesDataAdeno[[p]]
                    }
                }
            }
        }
        x <- c(1995:2015)
        plot(x,y, xlab ="Year",ylab="Rate per 100k People in Texas",col="red")
        lines(lowess(y~x,f=0.6),col = "blue")
    })
    output$smallRatesPlot <- renderPlot({
        y <- rep(0,21)
        for (j in input$gender){
            for (k in input$age){
                for (p in colnames(ratesDataSmall)){
                    if (paste(j,k) == p){
                        y <- y + ratesDataSmall[[p]]
                    }
                }
            }
        }
        x <- c(1995:2015)
        plot(x,y, xlab ="Year",ylab="Rate per 100k People in Texas",col="red")
        lines(lowess(y~x,f=0.6),col = "blue")
    })
    output$squamousRatesPlot <- renderPlot({
        y <- rep(0,21)
        for (j in input$gender){
            for (k in input$age){
                for (p in colnames(ratesDataSquamous)){
                    if (paste(j,k) == p){
                        y <- y + ratesDataSquamous[[p]]
                    }
                }
            }
        }
        x <- c(1995:2015)
        plot(x,y, xlab ="Year",ylab="Rate per 100k People in Texas",col="red")
        lines(lowess(y~x,f=0.6),col = "blue")
    })
    output$nonsmallRatesPlot <- renderPlot({
        y <- rep(0,21)
        for (j in input$gender){
            for (k in input$age){
                for (p in colnames(ratesDataOtherNonSmall)){
                    if (paste(j,k) == p){
                        y <- y + ratesDataOtherNonSmall[[p]]
                    }
                }
            }
        }
        x <- c(1995:2015)
        plot(x,y, xlab ="Year",ylab="Rate per 100k People in Texas",col="red")
        lines(lowess(y~x,f=0.6),col = "blue")
    })
# General Dashboard Page 1: Rates Across Texas Counties for Each Histologic Type
    output$allCountyRatesPlot <- renderPlot({
        countyRatesAll <- data.frame(countyRatesAll)
        wide <- reshape(countyRatesAll,
                        timevar = "Year",
                        idvar = "County",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County")
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, Rate, paste0("Rate.", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, 6, 9))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = cut_number(Rate,8))) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_brewer("Rate per 100k People", palette = "OrRd")
    })
    output$adenoCountyRatesPlot <- renderPlot({
        countyRatesAdeno <- data.frame(countyRatesAdeno)
        wide <- reshape(countyRatesAdeno,
                        timevar = "Year",
                        idvar = "County",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County")
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, Rate, paste0("Rate.", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, 6, 9))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = cut_number(Rate,4))) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_brewer("Rate per 100k People", palette = "OrRd")
    })
    output$smallCountyRatesPlot <- renderPlot({
        countyRatesSmall <- data.frame(countyRatesSmall)
        wide <- reshape(countyRatesSmall,
                        timevar = "Year",
                        idvar = "County",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County")
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, Rate, paste0("Rate.", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, 6, 9))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = cut_number(Rate,3))) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_brewer("Rate per 100k People", palette = "OrRd")
    })
    output$squamousCountyRatesPlot <- renderPlot({
        countyRatesSquamous <- data.frame(countyRatesSquamous)
        wide <- reshape(countyRatesSquamous,
                        timevar = "Year",
                        idvar = "County",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County")
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, Rate, paste0("Rate.", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, 6, 9))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = cut_number(Rate,4))) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_brewer("Rate per 100k People", palette = "OrRd")
    })
    output$nonsmallCountyRatesPlot <- renderPlot({
        countyRatesOtherNonSmall <- data.frame(countyRatesOtherNonSmall)
        wide <- reshape(countyRatesOtherNonSmall,
                        timevar = "Year",
                        idvar = "County",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County")
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, Rate, paste0("Rate.", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, 6, 9))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = cut_number(Rate,3))) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_brewer("Rate per 100k People", palette = "OrRd")
    })
# SIR & INLA Dashboard Page 2: SIR data alone as well as with INLA smoothing
    output$rawPlot <- renderPlot({
        if (input$cancerType == "All"){data <- countySIRAll}
        if (input$cancerType == "Adenocarcinoma"){data <- countySIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countySIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countySIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countySIROther}
        county_data <- data.frame(data)
        inp <- toString(input$dataType)
        n <- nchar(inp)
        wide <- reshape(county_data,
                             timevar = "Year",
                             idvar = "County_Code",
                             direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County_Code")  
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, inp, paste0(inp,".", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, (n+2), (n+5)))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = cut_number(inp, 5))) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_brewer(inp, palette = "OrRd")
    })
    output$inlaPlot <- renderPlot({
        if (input$cancerType == "All"){data <- countySIRAll}
        if (input$cancerType == "Adenocarcinoma"){data <- countySIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countySIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countySIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countySIROther}
        county_data <- data.frame(data)
        inp <- toString(input$dataType)
        n <- nchar(inp)
        wide <- reshape(county_data,
                        timevar = "Year",
                        idvar = "County_Code",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County_Code")  
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, inp, paste0(inp,".", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, (n+2), (n+5)))
        
        #Create neighborhood matrix
        tx_nb <- poly2nb(tx_data_map)
        nb2INLA("tx_SIR_map.adj", tx_nb)
        tx_g <- inla.read.graph(filename = "tx_SIR_map.adj")
        
        #Prep data fro INLA & run
        colnames(county_data)[5] <- "Y"
        colnames(county_data)[4] <- "E"
        county_data$County_Code <- as.factor(county_data$County_Code)
        county_data$idarea <- as.numeric(county_data$County_Code)
        county_data$idarea1 <- county_data$idarea
        county_data$idtime <- 1 + county_data$Year- min(county_data$Year)
        county_data$Y <- as.integer(county_data$Y)
        # Define formula & run INLA
        formula <- Y ~ f(idarea, model = "bym", graph = tx_g) +
            f(idarea1, idtime, model = "iid") + f(idtime, model = "rw2")
        res <- inla(formula, 
                     family = "poisson", data = county_data, E = E,
                     control.predictor = list(compute = TRUE))
        county_data$RR <- res$summary.fitted.values[, "mean"]
        tx_data_map_sf <- merge(tx_data_map_sf, county_data,
                               by.x = c("FIPS_ST_CN", "Year"),
                               by.y = c("County_Code", "Year"))
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = RR)) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red")       
        
    }) 
    res <- reactive({
        if (input$cancerType == "All"){data <- countySIRAll}
        if (input$cancerType == "Adenocarcinoma"){data <- countySIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countySIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countySIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countySIROther}
        county_data <- data.frame(data)
        inp <- toString(input$dataType)
        n <- nchar(inp)
        wide <- reshape(county_data,
                        timevar = "Year",
                        idvar = "County_Code",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County_Code")  
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, inp, paste0(inp,".", c(1995, 2000, 2005, 2010, 2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year, (n+2), (n+5)))
        
        #Create neighborhood matrix
        tx_nb <- poly2nb(tx_data_map)
        nb2INLA("tx_SIR_map.adj", tx_nb)
        tx_g <- inla.read.graph(filename = "tx_SIR_map.adj")
        
        #Prep data fro INLA & run
        colnames(county_data)[5] <- "Y"
        colnames(county_data)[4] <- "E"
        county_data$County_Code <- as.factor(county_data$County_Code)
        county_data$idarea <- as.numeric(county_data$County_Code)
        county_data$idarea1 <- county_data$idarea
        county_data$idtime <- 1 + county_data$Year- min(county_data$Year)
        county_data$Y <- as.integer(county_data$Y)
        # Define formula & run INLA
        formula <- Y ~ f(idarea, model = "bym", graph = tx_g) +
            f(idarea1, idtime, model = "iid") + f(idtime, model = "rw2")
        inla(formula, 
            family = "poisson", data = county_data, E = E,
            control.predictor = list(compute = TRUE))
    })
    output$timeResults <- renderPlot({
        plot(res()$summary.random$idtime$mean)
    })
    #res()$marginals.random$idarea$index. [FIPS code]
}

# Run the application 
shinyApp(ui = ui, server = server)
