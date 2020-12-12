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

shinyData <- st_read("finalDataSet-forShiny.shp") %>%
    st_transform(projection)

addComma <- function(num){ format(num, big.mark=",")}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Allocating Emergency Readiness Kits in L.A. County"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("budget",
                        "Budget:",
                        min = 100000,
                        max = 10000000,
                        value = 2500000,
                        step = 100000,
                        width = '100%'),
            sliderInput("costOfKit",
                        "Cost of kits:",
                        min = 10,
                        max = 250,
                        value = 20,
                        step = 5,
                        width = '100%'),
            sliderInput("pctOfTract",
                        "Percent of households in tract to receive kit",
                        min = 5,
                        max = 100,
                        value = 80,
                        step = 5,
                        post = '%',
                        width = '100%'),
            sliderInput("risk_weight",
                        "Algorithm weight (100% means only prioritize based on fire probability)",
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

server <- function(input, output, session) {
    output$allocationMap <- renderPlot ({
            getPrioritizedCensusTracts <- function(budget = 250000, costOfKit = 20, pctOfTract = 1.0, weight = 0.5){
                 d <- shinyData %>%
                    mutate(isReceivingKits = 0) %>%
                    mutate(totalKitsToSend = 0) %>%
                    mutate(priorityRanking = FrPrbNr*weight + RPL_THEMES*(1-weight)) %>%
                    arrange(desc(priorityRanking))
                
                numOfKits <- (budget / costOfKit)
                
                budgetRemaining <- budget
                tract.i <- 1
                totalHousingUnits <- 0
                
                while(budgetRemaining > costOfKit) {
                    housingUnits <- floor(d[tract.i,]$HsngUnt * pctOfTract)
                    if (housingUnits == 0) { tract.i <- tract.i+1; next; }
                    costForTract <- housingUnits*costOfKit
                    if(costForTract <= budgetRemaining) {
                        kitsSent <- housingUnits
                    } else {
                        possibleHousingUnits <- floor(budgetRemaining/costOfKit)
                        kitsSent <- possibleHousingUnits
                    }
                    budgetRemaining <- budgetRemaining - costForTract
                    d[tract.i,]$isReceivingKits <- 1
                    d[tract.i,]$totalKitsToSend <- kitsSent
                    tract.i <- tract.i + 1  
                }
                attr(d, "budget") <- budget
                attr(d, "costOfKit") <- costOfKit
                attr(d, "pctOfTract") <- pctOfTract
                attr(d, "weight") <- weight
                return(d)
            }
            
            getServedTractsPlot <- function(df, ttl, st){
                allocationMap <- ggplot(df) +
                    geom_sf(data = st_union(df))+
                    geom_sf(aes(fill = isReceivingKits),lwd = 0) +
                    labs(subtitle = st, title = ttl) +
                    theme(plot.title = element_text(size=22)) +
                    theme(plot.subtitle = element_text(size=14)) +
                    theme(legend.position = "none")
                
                allocationMap
            }
            dt <- getPrioritizedCensusTracts(budget=input$budget, costOfKit = input$costOfKit, pctOfTract = (input$pctOfTract/100), weight= input$risk_weight)
            print(head(dt))
            t <- paste("Sending",addComma(sum(dt$totalKitsToSend)),"kits to",addComma(sum(dt$isReceivingKits)),"LA county census tracts")
            st <- paste("Budget: $",addComma(input$budget),", Kit cost: $",input$costOfKit,sep="")
            getServedTractsPlot(dt,t,st)
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
