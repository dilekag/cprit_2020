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
map_data <- readOGR(dsn = "Texas_Counties")
ratesDataAll <- read_excel("rates_data.xlsx", sheet = 1)
ratesDataAdeno <- read_excel("rates_data.xlsx", sheet = 2)
ratesDataSmall <- read_excel("rates_data.xlsx", sheet = 3)
ratesDataSquamous <- read_excel("rates_data.xlsx", sheet = 4)
ratesDataOtherNonSmall <- read_excel("rates_data.xlsx", sheet = 5)
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
                    fluidRow(
                        tags$em("There are multiple classifications of lung cancer called 'histologic types' based on
                                the appearance of the cell under a microscope. The two major histologic types of lung cancer are 'small-cell' and 'non-small cell'
                                carcinomas. There are multiple sub-classifications under 'non-small cell' carcinomas including adenocarcinoma,
                                squamous cell carcinoma, and others."),
                        tags$hr()
                    ),
                    fluidRow(
                        box(
                            title = "Inputs", status = "warning", solidHeader = TRUE, width = 12,
                            checkboxGroupInput("gender", "Please Select Gender(s):", c("Male","Female")),
                            checkboxGroupInput("age", "Please Select Age Group(s):", c("<55 Years","55-74 Years", "75+ Years"))
                        )),
                    fluidRow(
                        tabBox(
                            title = "Lung Cancer Rates per 100k in Texas by Age, Gender & Histologic Type",
                            id = "generalplot", width = 12,
                            tabPanel("All", plotOutput("allRatesPlot")),
                            tabPanel("Adenocarcinoma", plotOutput("adenoRatesPlot")),
                            tabPanel("Small Cell Carcinoma", plotOutput("smallRatesPlot")),
                            tabPanel("Squamous Carcinoma", plotOutput("squamousRatesPlot")),
                            tabPanel("Non-Small Cell Carcinomas", plotOutput("nonsmallRatesPlot"))
                        )
                    )
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
                            tabPanel("Raw Data without Modeling/ Smoothing", plotOutput("rawPlot")),
                            tabPanel("Data After INLA Smoothing via a Bernardinelli Model", plotOutput("inlaPlot"))
                        ))
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
        plot(c(1995:2015),y)
        abline()
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
        plot(c(1995:2015),y)        
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
        plot(c(1995:2015),y)        
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
        plot(c(1995:2015),y)        
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
        plot(c(1995:2015),y)        
    })
    output$rawPlot <- renderPlot({
        if (input$cancerType == "All"){data <- countySIRAll}
        if (input$cancerType == "Adenocarcinoma"){data <- countySIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countySIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countySIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countySIROther}
        county_SIRs <- data.frame(data)
        wide_SIRs <- reshape(county_SIRs,
                             timevar = "Year",
                             idvar = "County_Code",
                             direction = "wide")
        tx_SIR_map <- merge(map_data, wide_SIRs, by.x = "FIPS_ST_CN", by.y = "County_Code")  
        tx_SIR_map_sf <- st_as_sf(tx_SIR_map)
        tx_SIR_map_sf <- gather(tx_SIR_map_sf, Year, SIR, paste0("SIR.", c(1995, 2000, 2005, 2010, 2015)))
        tx_SIR_map_sf$Year <- as.integer(substring(tx_SIR_map_sf$Year, 5, 8))
        ggplot(tx_SIR_map_sf) + geom_sf(aes(fill = SIR)) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank()
            ) +
            scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red")
    })
    output$inlaPlot <- renderPlot({
        
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
