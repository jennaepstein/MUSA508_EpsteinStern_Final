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
                         "B06012_002E", "B01001_048E", "B01001_049E",
                         "B01001_024E", "B01001_025E", "B07013_002E")

getCACensusTractDataByYear <- function(year) {
    get_acs(geography = "tract", variables = variablesOfInterest, 
            year=year, state='CA', county=countiesOfInterest, geometry=T, output="wide") %>%
        st_transform(projection) %>%
        rename(TotalPop = "B25026_001E",
               White = "B02001_002E",
               Female1824Bachelor = "B15001_050E",
               Male1824Bachelor = "B15001_009E",
               Female80to84 = "B01001_048E",
               Female85orMore = "B01001_049E",
               Male80to84 = "B01001_024E",
               Male85orMore = "B01001_025E", 
               MedianHouseholdIncome = "B19013_001E",
               MedianRent = "B25058_001E",
               Poverty = "B06012_002E",
               HousingUnits = "B07013_002E"
        ) %>%
        dplyr::select(-NAME, -starts_with("B")) %>%
        mutate(pctNonWhite = ifelse(TotalPop < White, 0, ifelse(TotalPop > 0, 1 -(White / TotalPop),0)),
               pctBachelors = ifelse(TotalPop > 0, ((Female1824Bachelor + Male1824Bachelor) / TotalPop),0),
               pctPoverty = ifelse(TotalPop > 0, Poverty / TotalPop, 0),
               pctOver80 = ifelse(TotalPop> 0, ((Female80to84 + Female85orMore + Male80to84 + Male85orMore) / TotalPop), 0),
               year=as.character(year))
}

tracts17 <- getCACensusTractDataByYear(2017) %>%
    filter(GEOID != '06037599100') %>%
    filter(GEOID != '06037599000')

tracts17_prioritized <- tracts17 %>%
    mutate(priorityScore = pctNonWhite) %>%
    arrange(desc(priorityScore))

tracts17_prioritized <- tracts17_prioritized %>%
    mutate(isReceivingKits = 0)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Allocating Emergency Readiness Kits in L.A. County (funded by REI)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("budget",
                        "Budget:",
                        min = 100000,
                        max = 10000000,
                        value = 500000,
                        step = 100000,
                        width = '100%'),
            sliderInput("costOfKit",
                        "Cost of kits:",
                        min = 10,
                        max = 250,
                        value = 40,
                        step = 5,
                        width = '100%'),
            sliderInput("risk_weight",
                        "Weighting algorithm (fire risk vs. vulnerable population)",
                        min = 0,
                        max = 100,
                        value = 50,
                        step = 10,
                        post = '%',
                        width = '100%'),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("allocationMap")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$allocationMap <- renderPlot ({
        budget <- input$budget
        costOfKit <- input$costOfKit
        pctOfTract <- 1.0
        numOfKits <- (budget / costOfKit)
        
        budgetRemaining <- budget
        tract.i <- 1
        totalHousingUnits <- 0
        
        addComma <- function(num){ format(num, big.mark=",")}
        
        while(budgetRemaining > costOfKit) {
            
            housingUnits <- tracts17_prioritized[tract.i,]$HousingUnits
            costForTract <- housingUnits*costOfKit
            print(paste("Tract #",tracts17_prioritized[tract.i,]$GEOID, " is priority #", tract.i, " and has ", addComma(housingUnits), " housing units, Cost of kits: $",addComma(costForTract), sep=""))
            if(costForTract <= budgetRemaining-costForTract) {
                print(paste("Budget remaining: $", addComma(budgetRemaining),sep=""))
                totalHousingUnits <- totalHousingUnits + housingUnits
            } else {
                possibleHousingUnits <- floor(budgetRemaining/costOfKit)
                totalHousingUnits <- totalHousingUnits + possibleHousingUnits
                print(paste("Cost for whole tract exceeds remaining budget, partial distribution to",addComma(floor(possibleHousingUnits))," housing units possible"))
            }
            budgetRemaining <- budgetRemaining - costForTract
            tract.i <- tract.i + 1  
            tracts17_prioritized[tract.i,]$isReceivingKits <- 1
            #print("...")
        }
        
     ggplot(tracts17_prioritized) +
            geom_sf(data = st_union(tracts17_prioritized))+
            geom_sf(aes(fill = isReceivingKits),lwd = 0) +
            labs(subtitle = "Algorithm is evenly weighted between fire risk and vulnerable populations", title = paste("Budget: $",addComma(budget)," - Kit cost: $",addComma(costOfKit),sep="")) +
            theme(plot.title = element_text(size=22)) +
            theme(legend.position = "none")
        })
    }

# Run the application 
shinyApp(ui = ui, server = server)
