#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(rmarkdown)

projection <- "EPSG:6423"
census_api_key("e59695f18b5f5959947fd9098feba458ca285cc5", install=TRUE, overwrite=TRUE)

countiesOfInterest <- c('Los Angeles')

variablesOfInterest <- c("B25026_001E","B02001_002E","B15001_050E",
                         "B15001_009E","B19013_001E","B25058_001E",
                         "B06012_002E")

getCACensusTractDataByYear <- function(year) {
    get_acs(geography = "tract", variables = variablesOfInterest, 
            year=year, state='CA', county=countiesOfInterest, geometry=T, output="wide") %>%
        st_transform(projection) %>%
        rename(TotalPop = "B25026_001E",
               White = "B02001_002E",
               Female1824Bachelor = "B15001_050E",
               Male1824Bachelor = "B15001_009E",
               MedianHouseholdIncome = "B19013_001E",
               MedianRent = "B25058_001E",
               Poverty = "B06012_002E"
        ) %>%
        dplyr::select(-NAME, -starts_with("B")) %>%
        mutate(pctWhite = ifelse(TotalPop > 0, White / TotalPop,0),
               pctBachelors = ifelse(TotalPop > 0, ((Female1824Bachelor + Male1824Bachelor) / TotalPop),0),
               pctPoverty = ifelse(TotalPop > 0, Poverty / TotalPop, 0),
               year=as.character(year))
}

tracts09 <- getCACensusTractDataByYear(2009)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Emergency Readiness Kits in Southern California"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Budget:",
                        min = 100000,
                        max = 1000000,
                        value = 500000,
                        step = 100000,
                        width = '100%'),
            sliderInput("kits",
                        "Cost of kits:",
                        min = 25,
                        max = 250,
                        value = 40,
                        step = 5,
                        width = '100%'),
            sliderInput("risk_weight",
                        "Weighting fire risk score (fire likelihood vs. high-need population)",
                        min = 0,
                        max = 100,
                        value = 50,
                        step = 10,
                        post = '%',
                        width = '100%')
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        ggplot() + 
            geom_sf(data=tracts09)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
