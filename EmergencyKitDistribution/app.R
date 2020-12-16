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

options(scipen = 999)
projection <- "EPSG:6423"

shinyData <- st_read("finalDataSet-forShiny.shp") %>%
    st_transform(projection)

addComma <- function(num){ format(num, big.mark=",")}

mapTheme <- function(base_size = 12) {
    theme(
        text = element_text( color = "black"),
        plot.title = element_text(size = 14,colour = "black"),
        plot.subtitle=element_text(face="italic"),
        plot.caption=element_text(hjust=0),
        axis.ticks = element_blank(),
        panel.background = element_blank(),axis.title = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
    theme(
        text = element_text( color = "black"),
        plot.title = element_text(size = 14,colour = "black"),
        plot.subtitle = element_text(face="italic"),
        plot.caption = element_text(hjust=0),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line("grey80", size = 0.1),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(fill = "grey80", color = "white"),
        strip.text = element_text(size=12),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.title = element_text(colour = "black", face = "italic"),
        legend.text = element_text(colour = "black", face = "italic"),
        strip.text.x = element_text(size = 14)
    )
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CalKit — Allocating emergency supply kits in L.A. county"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("budget",
                        "Budget:",
                        min = 100000,
                        max = 5000000,
                        value = 500000,
                        step = 100000,
                        width = '100%'),
            sliderInput("costOfKit",
                        "Cost of kits:",
                        min = 10,
                        max = 250,
                        value = 50,
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
            p("After making changes, please wait up to 30 seconds for the output to refresh.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("allocationMap"),
            h3("Tracts receiving kits"),
            DT::dataTableOutput('table')
        )
    )
)

server <- function(input, output, session) {

    ##  This debounce code keeps the data from being refreshed too quickly while the slider is being used    
    d_budget <- reactive({
        input$budget
    }) %>% debounce(1000)
    d_costOfKit <- reactive({
        input$costOfKit
    }) %>% debounce(1000)
    d_pctOfTract <- reactive({
        input$pctOfTract
    }) %>% debounce(1000)
    d_risk_weight <- reactive({
        input$risk_weight
    }) %>% debounce(1000)
    print(d_risk_weight)
    
    getPrioritizedCensusTracts <- reactive({
        getPrioritizedCensusTracts <- function(budget = 250000, costOfKit = 20, pctOfTract = 1.0, weight = 0.5){
            d <- shinyData %>%
                mutate(isReceivingKits = 0) %>%
                mutate(totalKitsToSend = 0) %>%
                mutate(totalCost = 0) %>%
                mutate(priorityRanking = (FrPrbNr*weight + RPL_THEMES*(1-weight)) * 100) %>%
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
                d[tract.i,]$totalCost <- kitsSent * costOfKit
                tract.i <- tract.i + 1  
            }
            attr(d, "budget") <- budget
            attr(d, "costOfKit") <- costOfKit
            attr(d, "pctOfTract") <- pctOfTract
            attr(d, "weight") <- weight
            return(d)
        }
        dt <- getPrioritizedCensusTracts(budget=d_budget(), costOfKit = d_costOfKit(), pctOfTract = (d_pctOfTract()/100), weight= (d_risk_weight()/100))
        })
    
    output$allocationMap <- renderPlot ({
            dt <- getPrioritizedCensusTracts()
            getServedTractsPlot <- function(df, ttl, st){
                allocationMap <- ggplot(df) +
                    geom_sf(data = st_union(df))+
                    geom_sf(aes(fill = isReceivingKits),lwd = 0.1) +
                    labs(subtitle = st, title = ttl) +
                    theme(plot.title = element_text(size=22)) +
                    theme(plot.subtitle = element_text(size=14)) +
                    theme(legend.position = "none")

                allocationMap
            }
            t <- paste("Sending",addComma(sum(dt$totalKitsToSend)),"kits to",addComma(sum(dt$isReceivingKits)),"L.A. County census tracts")
            st <- paste("Budget: $",addComma(d_budget()),", Kit cost: $",d_costOfKit(),sep="")
            getServedTractsPlot(dt,t,st)
    })
    
    output$table <- DT::renderDataTable({
        table <- getPrioritizedCensusTracts() %>%
            filter(isReceivingKits==1) %>%  
            select(FIPS, LOCATIO, priorityRanking, HsngUnt, totalKitsToSend, totalCost) %>%
            rename(Census_Tract_ID = "FIPS",
                   Census_Tract = "LOCATIO",
                   Total_Housing_Units = "HsngUnt",
                   Priority_Score = "priorityRanking",
                   Total_Kits_To_Send = "totalKitsToSend",
                   Total_Cost = "totalCost") %>%
            st_drop_geometry()
        DT::datatable(table, options = list(pageLength = 10))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
