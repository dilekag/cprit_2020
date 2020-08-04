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
library(leaflet)
library(dashboardthemes)

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
# Rates for different counties for ALL years (county-level analysis on INLA page)
#check_SIRAll <- county_rates_all_years
#SIR data by county (5-Year Intervals)
countySIRAll <- read_excel("county_sir_data.xlsx", sheet = 1)
countySIRAdeno <- read_excel("county_sir_data.xlsx", sheet = 2)
countySIRSmall <- read_excel("county_sir_data.xlsx", sheet = 3)
countySIRSquamous <- read_excel("county_sir_data.xlsx", sheet = 4)
countySIROther <- read_excel("county_sir_data.xlsx", sheet = 5)
#SIR data by county for all years
countyAllYearsSIRAll <- read_excel("all_years_by_hist.xlsx", sheet = 1)
countyAllYearsSIRAdeno <- read_excel("all_years_by_hist.xlsx", sheet = 2)
countyAllYearsSIRSmall <- read_excel("all_years_by_hist.xlsx", sheet = 3)
countyAllYearsSIRSquamous <- read_excel("all_years_by_hist.xlsx", sheet = 4)
countyAllYearsSIROther <- read_excel("all_years_by_hist.xlsx", sheet = 5)
#Socioeconomic Data
rurality <- read_excel("socioeconomic_data.xlsx", sheet = 1)
poverty <- read_excel("socioeconomic_data.xlsx", sheet = 2)

# UI main dashboard
ui <- dashboardPage(skin = "red", 
    dashboardHeader(title = "Lung Cancer in Texas"),
    # Sidebar Content
    dashboardSidebar(
        sidebarMenu(
            tags$img(src = "ut_logo.png", width = "100%"),
            tags$br(),
            menuItem(" About", tabName = "intro", icon = icon("info-circle", lib = "font-awesome")),
            menuItem(" General Trends", tabName ="general", icon = icon("chart-bar", lib = "font-awesome")),
            menuItem(" SIR Plots & INLA Modeling", tabName ="sir", icon = icon("map-marked-alt", lib = "font-awesome")),
            menuItem(" Socioeconomic Associations", tabName ="covar", icon = icon("user-friends", lib = "font-awesome")),
            menuItem(" COVID-19 and Lung Cancer", tabName ="covid", icon = icon("lungs-virus", lib = "font-awesome"))
            )
    ),
    # Main Body
    dashboardBody(
        tabItems(
            # Info Tab
            tabItem(tabName = "intro",
                    tags$h2("Spatiotemporal & Socioeconomic Lung Cancer Relationships in Texas Between 1995 and 2015"),
                    fluidRow(tags$img(height = 2, width = 1500, src = 'black_line.png')),
                    tags$h5(
                        "Lung cancer is the leading cause of cancer mortality in the world, with many people dying due to 
                        late-stage diagnoses.", tags$sup("1"), "As a result, it has become increasingly important to determine sub-populations and 
                        area-types that have an increased risk for the disease. In the United States alone, approximately 228,280 people are 
                        projected to be diagnosed with lung cancer in 2020.", tags$sup("2")
                    ),
                    tags$h4(tags$b("Project Overview")),
                    tags$h5    
                        ("The goal of this study was to model the 
                        spatial (across different counties), temporal (over time), and 
                        spatiotemporal relationships of lung cancer. In addition, COVID-19 and 
                        other socioeconomic factors were investigated alongside lung cancer to determine if there were 
                        associations. All cancer data was from the Texas Cancer Registry and 
                        processed via the SEER*Stat software.", tags$sup("3")
                    ),
                    tags$h4(tags$b("Explanation of Histologic Types")),
                    tags$h5("There are multiple classifications of lung cancer called 'histologic types' based on the appearance 
                            of the cancerous cell under a microscope. Only pathologists have the expertise to identify the specific 
                            visual differences between histologic cell types. Each has a unique etiology, therefore affecting people 
                            differently, meaning each should be studied individually. The grey boxes in the following diagram show the 
                            divisions of and relative prevalence", tags$sup("4"), "of the four histologic types included in this analysis."),
                    tags$br(),
                    HTML('<center><img src="hist_types.png" width="550" height="200"></center>'),
                    tags$br(),
                    tags$h5("As shown above, carcinomas (cancer forms in the skin/ tissue cells lining internal organs) are far more common than 
                            sarcomas (forms in connective tissue cells like fat and blood vessels). Most carcinomas are non-small cell; adenocarcinomas 
                            and squamous cell carcinomas are the two most common histologic types classified under this category."),
                    tags$br(),
                    tags$h4(tags$b("Using the Dashboard")),
                    tags$h5("The sidebar on the left can be used to navigate through the various sections of the dashboard. 
                            The top of each page contains a blurb detailing its contents. 
                            There are multiple maps being constructed in addition to thorough sampling procedures, so please be patient 
                            with the page as it may need 1-2 minutes to load the appropriate results once a selection is made."),
                    fluidRow(tags$img(height = 2, width = 1500, src = 'black_line.png')),
                    tags$h5(tags$sup("1"), "Li, J., Guo, W., Ran, J. et al.", 
                            a("Five-year lung cancer mortality risk analysis and topography in Xuan Wei: a spatiotemporal correlation analysis.", 
                              href = "https://doi.org/10.1186/s12889-019-6490-1"), "BMC Public Health 19, 173 (2019)."),
                    tags$h5(tags$sup("2"), "Siegel RL, Miller, KD, Jemal A.", 
                            a("Cancer statistics, 2020.", 
                              href = "https://doi.org/10.3322/caac.21590"), "CA: A  Cancer Journal for Clinicians. Vol 70; 1. January 8, 2020."),
                    tags$h5(tags$sup("3"),  
                            a("Texas Cancer Registry", 
                              href = "https://www.dshs.state.tx.us/tcr/"), "SEER*Stat Database, Limited_Use 1995-2017 Incidence, Texas statewide, Texas Department of State Health Services, 
                            created December 2019, based on NPCR-CSS Submission, cut-off 11/07/19."),
                    tags$h5(tags$sup("4"), "Howlader N, Noone AM, Krapcho M, Miller D, Brest A, Yu M, Ruhl J, Tatalovich Z, Mariotto A, Lewis DR, Chen HS, Feuer EJ, Cronin KA (eds).", 
                            a("SEER Cancer Statistics Review",
                              href = "https://seer.cancer.gov/csr/1975_2017/"), "1975-2017, National Cancer Institute. Bethesda, MD, based on November 2019 SEER data submission, posted to the SEER web site, April 2020."
                    )
                    ),
            # First Tab
            tabItem(tabName = "general",
                        tags$h5("This", tags$strong("General Trends"), "page of the dashboard contains two sets of plots. The first shows the", tags$strong("spatial"), "relationships of lung cancer in order to visualize where there may be counties/ regions with abnormally high rates of specific lung cancers. The second set of plots show the", tags$strong("temporal"), "trends of different lung cancer types between 1995 and 2015. In addition, through the selection of inputs, the trends regarding specific age and/or gender groups can be investigated."),
                    fluidRow(tags$img(height = 2, width = 1500, src = 'black_line.png')),
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
                            title = "Inputs for Temporal Plots Below", status = "warning", solidHeader = TRUE, width = 12,
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
                    tags$h5(tags$em("Disclaimer: Starting in 2001, a new lung cancer called 'non-small cell carcinoma' was added under the classification of 'non-small cell carcinoma', which may be the cause of the increase in cases during this time."))
                ),
            # Second Tab
            tabItem(tabName = "sir",
                    tags$h5("This ", tags$strong("SIR Plots & INLA Modeling "), "page of the dashboard visualizes the spatiotemporal 
                            relationships of both the", tags$strong("observed and modeled"), "lung cancer data for the four histologic types for the following three metrics:"),
                    tags$ol(
                        tags$li(tags$strong("Observed"),"(true) number of lung cancer cases"), 
                        tags$li(tags$strong("Expected"), "number of lung cancer cases (calculated using census data)", tags$sup("1")), 
                        fluidRow(tags$h5("Texas' population was divided into 30 demographic groups by gender, race, and age. Each was assumed to have its own unique rate of lung cancer. 
                                         2 Genders (Male & Female) x 5 Race Groups (White, Black, Asian/ Pacific Islander, American Indian & Hispanic) x 3 Age Groups (<54 years, 55-74 years, 75+ years) = 30 Demographic Groups")),
                        fluidRow(withMathJax("Assuming these 30 demographic groups in Texas, expected cases are calculated by the following formulas: $$lcr_{ij} \\text{ (Lung Cancer Rate for Demographic i in Year j)} = \\frac{\\text{Number of People of Demographic i Diagnosed with Lung Cancer in Year j in All TX}}{\\text{Number of People of Demographic i Living in TX in Year j}} $$")),
                        fluidRow(withMathJax("$$\\text{Expected Cases in County A in Year j} = \\sum_{i=1}^{30} \\text{Number of People of Demographic i Living in County A in Year j} * lcr_{ij} $$")),
                        tags$li(tags$strong("Standardized Incidence Ratio (SIR)"), "for each county")
                    ),
                    fluidRow(withMathJax("$$\\text{SIR for County A in Year j} =\\frac{\\text{Observed Cases in County A in Year j (1)}}{\\text{Expected Cases in County A in Year j (2)}}$$")),
                    tags$h5("SIRs are useful because they have a straightforward interpretation. A value greater than 1 indicates a potential high incidence county or 'hot spot' that may be at particular risk for lung cancer. A value less than 1, on the other hand, indicates a 'cold spot'."),
                    tags$h5(tags$strong("What is the importance of modeling data?"), "Independence between each possible pair of observations (in this case, an observation is the SIR or rate of lung cancer in a county for some year) is an assumption made when analyzing data. 
                            However, this is an unfair assumption because there exist ", tags$strong("three sources of correlations and variation"), "that must be considered in order to accurately understand lung cancer trends:"),
                    tags$ol(
                        tags$li(tags$strong("Spatial"), ": Counties that are close to one another often share socioeconomic traits and topographies."),
                        tags$li(tags$strong("Temporal"), ": Various events/ anomalies happen in certain years that are unrelated to lung cancer but still influence the counties' rates"),
                        tags$li(tags$strong("Spatiotemporal"), ": The two effects above may interact with one another and contribute to additional variation")
                    ),
                    tags$h5("By getting rid of uncertainty and unnecessary noise, ", tags$strong("model-based relative risk (RR)"), " provides a smoothed version of SIR, is less vulnerable to abnormalities, and is generally considered more accurate. 
                            For this project's analysis, a combination of the Bernardinelli Model", tags$sup("2"), " and Leroux Model", tags$sup("3"), "(below) was used to model the relative risk of lung cancer. 
                            It was implemented through the R-INLA", tags$sup("4"), "software by the methods outlined in both Moraga", tags$sup("5"), " and Rubio-Gomez V", tags$sup("6"), "."),
                    tags$h5("The data was modeled by a ", tags$strong("Poisson Distribution"), " (below), which requires one parameter,  ", HTML('&#955'), ". "),
                    HTML('<center><img src="poisson.png" width="250" height="175"></center>'),
                    withMathJax("For County i in Year j: $$\\text{Observed LC Cases}_{ij} \\sim \\text{Poisson}(\\text{Expected LC Cases}_{ij}*\\text{RR}_{ij})$$"),
                    withMathJax("$$ log(\\text{RR}_{ij}) = \\alpha + s_i + t_j + \\delta_{ij}  $$"),
                    tags$h5("where ", HTML('&#945') ," is the intercept; s", tags$sub("i"), " represents the spatial effects via a neighborhood matrix and Leroux parameter to determine the spatial dependency of the data; 
                            t", tags$sub("j"), " represents the temporal effects via a markovian random-walk model of order two; ",
                            HTML('&#948'), tags$sub("ij"), " represents the spatiotemporal effects via a completely random, independent and identically distributed model."),
                    fluidRow(tags$img(height = 2, width = 1500, src = 'black_line.png')),
                    tags$br(),
                    fluidRow(
                        box(
                            width = 12,
                            radioButtons("cancerType", "Please Select the Histologic Lung Cancer Type(s) to be Investigated:", c("All", "Adenocarcinoma", "Small Cell Carcinoma", "Squamous Cell Carcinoma", "Other Non-Small Cell Carcinomas")),
                            selectInput("dataType", "Please Select the Desired Metric for Analysis:", c("Expected", "Observed", "SIR"))
                        )),
                    fluidRow(
                        tabBox(
                            title = "Plots of Modeled Risk to and Observed Rates of Lung Cancer in Texas Between 1995 and 2015",
                            id = "sirs", width = 12,
                            tabPanel("County-level Relative Risk (based on SIR) After INLA Smoothing via a Bernardinelli Model", plotOutput("inlaPlot")),
                            tabPanel("Observed, Expected, and SIR Data without Modeling/ Smoothing", plotOutput("rawPlot"))
                        )),
                    fluidRow(
                        box(
                            title = "County-Level Analysis", status = "warning", solidHeader = TRUE, width = 12,
                            selectInput("countyChoice", "Please select a county to conduct a further analysis for that area", map_data$CNTY_NM),
                            sliderInput("year", "Please select a year of data to focus the analysis in", value = 2000, min = 1995, max = 2015, sep = "")
                        )),
                    fluidRow(
                        box(
                            plotOutput("map")),
                       box(
                            plotOutput("timeResults"))
                    ),
                    fluidRow(tags$img(height = 2, width = 1500, src = 'black_line.png')),
                    tags$h5(tags$sup("1"), "Population Estimates Program, Population Division, U.S. Census Bureau. Intercensal Estimates of the Resident Population by Five-Year Age Groups, Sex, Race, and Hispanic Origin for Counties: ",
                            a("July 1, 1990 to July 1, 1999", href = "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/counties/asrh/casrh48.txt"), "; ",
                            a("April 1, 2000 to July 1, 2010", href = "https://www.census.gov/data/datasets/time-series/demo/popest/intercensal-2000-2010-counties.html"), "; ",
                            a("April 1, 2010 to July 1, 2019", href = "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-detail.html"), "; Washington, DC."),
                    tags$h5(tags$sup("2"), "Bernardinelli L, Clayton DG, Pascutto C, Montomoli C, Ghislandi M, Songini M. (1995).",
                            a("Bayesian analysis of space-time variation in disease risk.", href = "https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.4780142112"),
                            tags$i("Statistics in Medicine"), "14: 2433-43."),
                    tags$h5(tags$sup("3"), "Leroux B, Lei X, Breslow N. (1999). ",
                            a("Estimation of Disease Rates in Small Areas: A New Mixed Model for Spatial Dependence. ", href = "https://link.springer.com/chapter/10.1007/978-1-4612-1284-3_4"),
                            tags$i("Statistical Models in Epidemiology, the Environment and Clinical Trials, "), "edited by M Halloran and D Berry, 135-78. New York: Springer-Verlag."),
                    tags$h5(tags$sup("4"), "Rue H, Martino S, Chopin N. (2009) ",
                            a("Approximate Bayesian inference for latent Gaussian models using integrated nested Laplace approximations (with discussion). ", href = "http://www.r-inla.org/"),
                            "Journal of the Royal Statistical Society, Series B, 71(2):319{392}."),
                    tags$h5(tags$sup("5"), "Moraga P. (2020). ",
                            a(tags$i("Geospatial Health Data: Modeling and Visualization with R-INLA and Shiny. "), href = "https://www.paulamoraga.com/book-geospatial/index.html"),
                            "CRC Press. ISBN: 978-0367357955"),
                    tags$h5(tags$sup("6"), "Rubio-Gomez V. (2020). ",
                            a(tags$i("Bayesian Inference with INLA. "), href = "https://becarioprecario.bitbucket.io/inla-gitbook/index.html"),
                            "CRC Press. ISBN: 978-1138039872")
                ),
            # Third Tab
            tabItem(tabName = "covar",
                fluidRow(
                    box(width = 12,
                    sliderInput(inputId = "sociYear", label = NULL, value = 2000, min = 1995, max = 2015, step = 5, sep = ""))
                ),
                fluidRow(
                    box(width = 7, plotOutput("rurality")),
                    box(width = 5, plotOutput("poverty"))
                ),
                fluidRow(
                    box(width = 12, 
                        selectInput("cancerType2", "Please Select the Histologic Lung Cancer Type(s) to be Investigated:", c("All", "Adenocarcinoma", "Small Cell Carcinoma", "Squamous Cell Carcinoma", "Other Non-Small Cell Carcinomas")))
                ),
                fluidRow(
                    box(
                        dataTableOutput("sociResults")
                    )
                )
                
                ),
            # Fourth Tab
            tabItem(tabName = "covid",
                tags$h5("This", tags$strong("COVID-19 and Lung Cancer"), "page contains side-by-side plots comparing the relative rates of lung cancer and the recent COVID-19 epidemic that has been significantly straining Texas hospital resources. It is hypothesized that COVID-19 can have adverse effects on peoples' lungs, so when determining", tags$strong("health funding"), ", it's essential to consider these two diseases in relation to one another."),
                fluidRow(tags$img(height = 2, width = 1500, src = 'black_line.png')),
                tags$br(),
                fluidRow(
                    box(title = "COVID-19 Variable of Interest", status = "warning", solidHeader = TRUE, width = 6,
                        radioButtons("covid",NULL, c("Cumulative COVID-19 Cases since 3/4/20 per 100k Texans"="cases","Rate of Cumulative Confirmed COVID-19 Patients who Died"="mort")),
                        tags$figcaption(tags$em("COVID-19 Data from TX DSHS", tags$sup("1"), " as of August 1st, 2020"))
                        ),
                    box(title = "Lung Cancer Variable of Interest", status = "warning", solidHeader = TRUE, width = 6,
                        radioButtons("lung_cancer",NULL,c("Lung Cancer Diagnoses per 100k Texans in 2017"="cases","Deaths from Lung Cancer per 100k Texans for Sum of Yearly Populations between 1995 and 2017"="mort"))
                    )
                ),
                fluidRow(
                    box(width = 6, leafletOutput("covid")),
                    box(width = 6, leafletOutput("lung_cancer"))
                ),
                tags$h5("A subject of interest would be to determine whether there is a relationship between how counties (specifically their health systems) are affected by COVID-19 and how they are affected by lung cancer. In order to investigate the potential associations, two spatial models were constructed:"),
                tags$strong("Model 1: Lung Cancer Death Rates per 100k as Response Variable"),
                withMathJax("For County i in Year j: $$\\text{Observed LC Cases}_{ij} \\sim \\text{Poisson}(\\text{Expected LC Cases}_{ij}*\\text{RR}_{ij})$$"),
                withMathJax("$$ log(\\text{RR}_{ij}) = \\alpha + s_i + t_j + \\delta_{ij}  $$"),
                tags$h5("where ", HTML('&#945') ," is the intercept; s", tags$sub("i"), " represents the spatial effects via a neighborhood matrix and Leroux parameter to determine the spatial dependency of the data; 
                            t", tags$sub("j"), " represents the temporal effects via a markovian random-walk model of order two; ",
                        HTML('&#948'), tags$sub("ij"), " represents the spatiotemporal effects via a complely random, idependent and identically distributed model."),
                fluidRow(
                    plotOutput("model1"),
                    plotOutput("model2")
                )
            )
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
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
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
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
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
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
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
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
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
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
            ) +
            scale_fill_brewer("Rate per 100k People", palette = "OrRd")
    })
# SIR & INLA Dashboard Page 2: SIR data alone as well as with INLA smoothing
    #Matrix required for Leroux model
    mat_c <- reactive({
        tx_nb <- poly2nb(map_data)
        Q <- Diagonal(x = sapply(tx_nb, length))
        for(i in 2:nrow(map_data)) {
            Q[i - 1, i] <- -1
            Q[i, i - 1] <- -1
        }
        Diagonal(x = 1, n = nrow(map_data)) - Q
    })
    #Plot of SIR, Observed cases, or Expected cases across the state of Texas
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
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
            ) +
            scale_fill_brewer(inp, palette = "OrRd")
    })
    # Reactive results objects from running INLA model on 21 data points
    res <- reactive({
        if (input$cancerType == "All"){data <- countyAllYearsSIRAll} 
        if (input$cancerType == "Adenocarcinoma"){data <- countyAllYearsSIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countyAllYearsSIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countyAllYearsSIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countyAllYearsSIROther}
        county_data <- data.frame(data)
        #Prep data fro INLA & run
        county_data$County_Code <- as.factor(county_data$County_Code)
        county_data$idarea <- as.numeric(county_data$County_Code)
        county_data$idtime <- 1 + county_data$Year- min(county_data$Year)
        county_data$e <- 1:nrow(county_data) 
        county_data$Observed <- as.integer(county_data$Observed)
        # Define formula & run INLA
        formula <- Observed ~ 
            f(idarea, model = "generic1", Cmatrix = mat_c()) + 
            f(e, model = "iid") + 
            f(idtime, model = "rw2", constr = T) 
        inla(formula, 
             family = "poisson", data = county_data, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
     })
    output$inlaPlot <- renderPlot({
        if (input$cancerType == "All"){data <- countyAllYearsSIRAll} 
        if (input$cancerType == "Adenocarcinoma"){data <- countyAllYearsSIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countyAllYearsSIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countyAllYearsSIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countyAllYearsSIROther}
        county_data <- data.frame(data)
        county_data$RR <- res()$summary.fitted.values[, "mean"]
        wide <- reshape(county_data,
                        timevar = "Year",
                        idvar = "County_Code",
                        direction = "wide")
        tx_data_map <- merge(map_data, wide, by.x = "FIPS_ST_CN", by.y = "County_Code")  
        tx_data_map_sf <- st_as_sf(tx_data_map)
        tx_data_map_sf <- gather(tx_data_map_sf, Year, RR, paste0("RR.", c(1995:2015)))
        tx_data_map_sf$Year <- as.integer(substring(tx_data_map_sf$Year,4,7))
        tx_data_map_sf <- subset(tx_data_map_sf, Year == 1995 | Year == 2000 | Year == 2005 | Year == 2010 | Year == 2015)
        ggplot(tx_data_map_sf) + geom_sf(aes(fill = RR)) +
            facet_wrap(~Year, dir = "h", ncol = 3) +
            theme_bw() +
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
            ) +
            scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red")       
    }) 
    output$timeResults <- renderPlot({
        if (input$cancerType == "All"){data <- countyAllYearsSIRAll} 
        if (input$cancerType == "Adenocarcinoma"){data <- countyAllYearsSIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countyAllYearsSIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countyAllYearsSIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countyAllYearsSIROther}
        county_data <- data.frame(data)
        code <- as.integer(subset(map_data@data, CNTY_NM == input$countyChoice)["FIPS_ST_CN"])
        step_size <- (code-48000+1)*0.5
        index <- seq(step_size,(20*254+step_size),254)
        rr <- res()$summary.fitted.values[index, 'mean']
        sir <- county_data$SIR[index]
        plot(1995:2015, sir, col = "blue", type = "b", pch = 19, xlab = "Year", ylab = "SIR and Relative Risk")
        points(1995:2015, rr, col = "red", type = "b", pch=19)
        legend("topright", c("Observed SIR", "Modeled Relative Risk"), fill = c("blue", "red"))
    })
    output$map <- renderPlot({
        if (input$cancerType == "All"){data <- countyAllYearsSIRAll} 
        if (input$cancerType == "Adenocarcinoma"){data <- countyAllYearsSIRAdeno}
        if (input$cancerType == "Small Cell Carcinoma"){data <- countyAllYearsSIRSmall}
        if (input$cancerType == "Squamous Cell Carcinoma"){data <- countyAllYearsSIRSquamous}
        if (input$cancerType == "Other Non-Small Cell Carcinomas"){data <- countyAllYearsSIROther}
        county_data <- data.frame(data)
        map_data_sf <- st_as_sf(map_data)
        code <- as.integer(subset(map_data@data, CNTY_NM == input$countyChoice)["FIPS_ST_CN"])
        county_year_data <- subset(county_data, Year == as.integer(input$year) & County_Code == code)
        expect <- round(county_year_data["Expected"], 1)
        observed <- round(county_year_data["Observed"], 0)
        sir <- round(county_year_data["SIR"], 2)
        ggplot(data = map_data_sf) + geom_sf(aes(fill = (CNTY_NM == input$countyChoice), color = "red")) +theme_void() + theme(legend.position = "none") + 
            labs(title = paste(input$countyChoice, "County in", input$year), subtitle = paste("Observed Cases =", observed,"|   Expected Cases =", expect, "|   SIR =", sir)) + theme(plot.title = element_text(size = 20, face = "bold"))
    })
   
#Socioeconomic Associations Dashboard Page 3: Spatial plots of covariates & model analysis 
    output$rurality <- renderPlot({
        rurality_data <- data.frame(subset(rurality, Year == input$sociYear))
        rurality_data$Rurality_Score <- as.factor(rurality_data$Rurality_Score)
        rurality_data_sf <- st_as_sf(merge(map_data, rurality_data, by.x = "FIPS_ST_CN", by.y = "County_Code"))
        ggplot(rurality_data_sf) + geom_sf(aes(fill = Rurality_Score)) + theme_bw() +
            theme(line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.background = element_blank()
            ) +
            scale_fill_brewer("Rurality Score", type = "seq", palette = 1, direction = -1, labels = c("Metro area (Population > 1 million)","Metro area (1 million > Population > 250,000)","Metro area (250,000 > Population)","Urban area adjacent to metro area (Population > 20,000)","Urban area NOT adjacent to metro area (Population > 20,000)","Urban area adjacent to metro area (19,999 > Population > 2,500)","Urban area NOT adjacent to metro area (19,999 > Population > 2,500)","Rural area OR (2,500 > Population) adjacent to metro area","Rural area OR (2,500 > Population) NOT adjacent to metro area"))
    })
    output$poverty <- renderPlot({
        poverty_data <- data.frame(subset(poverty, Year == input$sociYear))
        poverty_data_sf <- st_as_sf(merge(map_data, poverty_data, by.x = "FIPS_ST_CN", by.y = "County_Code"))
        ggplot(poverty_data_sf) + geom_sf(aes(fill = cut_number(Poverty_Rate,5))) + theme_bw() +
            theme(line = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_blank()
            ) +
            scale_fill_brewer("Poverty Rate", palette = "RdYlGn", direction = -1)
    })
   # soci_res <- reactive({
    #})
    output$sociResults <- renderDataTable({
        if (input$cancerType2 == "All"){data <- countySIRAll}
        if (input$cancerType2 == "Adenocarcinoma"){data <- countySIRAdeno}
        if (input$cancerType2 == "Small Cell Carcinoma"){data <- countySIRSmall}
        if (input$cancerType2 == "Squamous Cell Carcinoma"){data <- countySIRSquamous}
        if (input$cancerType2 == "Other Non-Small Cell Carcinomas"){data <- countySIROther}
        soci_data <- merge(data, rurality, by = c("County_Code","Year"))
        soci_data <- merge(soci_data, poverty, by = c("County_Code","Year"))
        soci_data <- data.frame(soci_data)
        soci_data$County_Code <- as.factor(soci_data$County_Code)
        soci_data$idarea <- as.numeric(soci_data$County_Code)
        soci_data$e <- 1:nrow(soci_data)
        soci_data$idtime <- 1 + soci_data$Year- min(soci_data$Year)
        soci_data$Observed <- as.integer(soci_data$Observed)
        formula <- Observed ~ Rurality_Score + Poverty_Rate +
            f(idarea, model = "generic1", Cmatrix = mat_c()) + 
            f(e, model = "iid") + 
            f(idtime, model = "rw2", constr = T) 
        soci_res <- inla(formula, 
             family = "poisson", data = soci_data, E = Expected,
             control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
        d <- soci_res$summary.fixed[c("Rurality_Score", "Poverty_Rate"),c("0.025quant", "0.975quant")]
        d <- cbind(d, c("Rurality Parameter", "Poverty Parameter"))
        d
    })
# COVID-19 Dashboard Page 4: Leaflet Plots & Model to Determine Presence of an Association
    output$covid <- renderLeaflet({
        if (input$covid == "cases"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 1)}
        if (input$covid == "mort"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 2)}
        map <- merge(map_data, data, by.x = "FIPS_ST_CN", by.y = "County_Code")
        l <- leaflet(map) %>% addTiles()
        pal <- colorNumeric(palette = "YlOrRd", domain=map$Rate)
        labels <- sprintf("<strong> %s </strong> <br/>
                  Rate: %s", map$CNTY_NM, round(map$Rate,4)) %>% lapply(htmltools::HTML)
        l %>% 
            addPolygons(
                color = "grey", weight = 1,
                fillColor = ~ pal(Rate), fillOpacity = 0.5,
                highlightOptions = highlightOptions(weight = 4),
                label = labels,
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px"
                    ),
                    textsize = "15px", direction = "auto"
                )    
            ) %>%
            addLegend(
                pal = pal, values = ~Rate, opacity = 0.5,
                title = "Rate", position = "bottomright"
            )
    })
    output$lung_cancer <- renderLeaflet({
        if (input$lung_cancer == "cases"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 3)}
        if (input$lung_cancer == "mort"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 4)}        
        map <- merge(map_data, data, by.x = "FIPS_ST_CN", by.y = "County_Code")
        l <- leaflet(map) %>% addTiles()
        pal <- colorNumeric(palette = "YlOrRd", domain=map$Rate)
        labels <- sprintf("<strong> %s </strong> <br/>
                  Rate: %s", map$CNTY_NM, round(map$Rate,4)) %>% lapply(htmltools::HTML)
        l %>% 
            addPolygons(
                color = "grey", weight = 1,
                fillColor = ~ pal(Rate), fillOpacity = 0.5,
                highlightOptions = highlightOptions(weight = 4),
                label = labels,
                labelOptions = labelOptions(
                    style = list(
                        "font-weight" = "normal",
                        padding = "3px 8px"
                    ),
                    textsize = "15px", direction = "auto"
                )    
            ) %>%
            addLegend(
                pal = pal, values = ~Rate, opacity = 0.5,
                title = "Rate", position = "bottomright"
            )
        })
    output$model1 <- renderPlot({
        # COVID data, which depends on what the user selects
        if (input$covid == "cases"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 1)}
        if (input$covid == "mort"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 2)}
        # Lung cancer death data, which is the response variable for model 1
        lc_deaths <- read_excel("covid_dashboard_data.xlsx", sheet = 3)
        colnames(lc_deaths) <- c("Lung_Cancer_Death_Rate", "County_Code")
        lc_deaths <- merge(lc_deaths, data, by.x = "County_Code", by.y = "County_Code")
        # Prep for INLA
        lc_deaths$idareau <- 1:nrow(lc_deaths)
        lc_deaths$idareav <- 1:nrow(lc_deaths)
        lc_deaths$Lung_Cancer_Death_Rate <- lc_deaths$Lung_Cancer_Death_Rate*1000
        lc_deaths$Lung_Cancer_Death_Rate <- round(lc_deaths$Lung_Cancer_Death_Rate, digits = 0)
        res <- inla(Lung_Cancer_Death_Rate ~ Rate + 
                    f(idareau, model = "generic1", Cmatrix = mat_c()) +
                    f(idareav, model = "iid"),
                    family = "poisson", data = lc_deaths, control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
        marginal <- data.frame(inla.smarginal(res$marginals.fixed$Rate))
        ggplot(marginal, aes (x=x,y=y)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +geom_vline(xintercept = 0, col = "black") +theme_bw()
    })
    output$model2 <- renderPlot({
        if (input$covid == "cases"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 1)}
        if (input$covid == "mort"){data <- read_excel("covid_dashboard_data.xlsx", sheet = 2)}
        cancer_data_2015 <- subset(countySIRAll, Year == 2015)
        data1 <- merge(cancer_data_2015, data, by.x = "County_Code", by.y = "County_Code")
        data1$idareau <- 1:nrow(data1)
        data1$idareav <- 1:nrow(data1)
        data1$Observed <- as.integer(data1$Observed)
        data1$County_Code <- as.factor(data1$County_Code)
        formula <- Observed ~ Rate +
            f(idareau, model = "generic1", Cmatrix = mat_c()) +
            f(idareav, model = "iid")
        res <- inla(formula, family = "poisson", data = data1, 
            E = Expected, control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE), 
            control.predictor = list(compute=TRUE))
        marginal <- data.frame(inla.smarginal(res$marginals.fixed$Rate))
        ggplot(marginal, aes (x=x,y=y)) + geom_line() + labs(x = expression(beta[1]), y = "Density") +geom_vline(xintercept = 0, col = "black") +theme_bw()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
