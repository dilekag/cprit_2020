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
ratesDataAll <- read_excel("rates_data.xlsx", sheet = 1)
ratesDataAdeno <- read_excel("rates_data.xlsx", sheet = 2)
ratesDataSmall <- read_excel("rates_data.xlsx", sheet = 3)
ratesDataSquamous <- read_excel("rates_data.xlsx", sheet = 4)
ratesDataOtherNonSmall <- read_excel("rates_data.xlsx", sheet = 5)

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
                            checkboxGroupInput("gender", "Select Gender(s):", c("Male","Female")),
                            checkboxGroupInput("age", "Select Age Group(s):", c("<55 Years","55-74 Years", "75+ Years"))
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
            tabItem(tabName = "sir"),
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
}

# Run the application 
shinyApp(ui = ui, server = server)
