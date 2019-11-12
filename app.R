#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### PACKAGES

library("shiny")
library("shinyjs")
library("tidyverse")
library("plotly")


### TRANFORMATION OF DATA

currentValueData <- read_csv("data/currentValueData/propertyECDataWithPresentValue.csv")

propertyECData <- list.files(path = "data", pattern = "*.csv") %>%
    str_c("data/", .) %>%
    map_df(~read_csv(., col_types = cols("Completion Date" = col_character()))) %>%
    select(., -c("No. of Units", "Property Type", "Type of Area"))

names(propertyECData) <- str_replace_all(names(propertyECData), "[ ($)]*", "")

propertyECData$SaleYear <- substr(propertyECData$SaleDate, nchar(propertyECData$SaleDate)-3, nchar(propertyECData$SaleDate))

minYear <- as.Date(min(propertyECData$SaleYear), format = "%Y")
maxYear <- as.Date(max(propertyECData$SaleYear), format = "%Y")
allYears <- sort(unique(propertyECData$SaleYear))

selectPAList <- sort(unique(propertyECData$PlanningArea))



ui <- fluidPage(
    
    titlePanel("EC Inspector Dashboard"),
    
    fluidRow(
        column(3,
               selectInput(inputId = "PlanningAreas",
                           label = "Select planning areas to compare",
                           choices = selectPAList,
                           selected = selectPAList[1:2],
                           multiple = TRUE)
        ),
        column(5,
               sliderInput(inputId = "Years",
                           label = "Select a year range",
                           step = 1,
                           min = minYear,
                           max = maxYear,
                           value = c(minYear, maxYear),
                           dragRange=TRUE,
                           timeFormat = "%Y")
        )
    ),
    fluidRow(
        column(12,
            helpText("*Double click on the grey area to unselect data points")
        )
    ),
    fluidRow(
        column(8,
               plotlyOutput("avgPricePerYrPA")
        ),
        column(3,
               plotOutput("proportionNewAndResale")
        )
    ),
    fluidRow(
        column(12,
               plotlyOutput("compareNewResalePrice")
        )
    ),
    fluidRow(
        column(12,
               selectInput(inputId = "ViolinYear",
                           label = "Select planning areas to compare",
                           choices = allYears,
                           selected = allYears[1]),
               plotlyOutput("distNewResalePrice")
        )
    ),
    fluidRow(
        column(12,
               plotOutput("realValResaleHeatmap")
        )
    ),
    fluidRow(
        helpText("Done by Abhyuday, Bernice Ng, Ming Wei")
    )
)

server <- function(input, output, session) {
    
    ### COMMON DATA
    
    saleTypeColor <- c("New Sale" = "darkturquoise", "Resale" = "darkorange")
    
    
    ### REACTIVE DATA
    
    avgPricePerYrPAClick <- reactiveValues(data = NULL)
    
    ### CHARTS
    
    avgPricePerYrPAChart <- function(input){
        ggplot(input, aes(x=as.numeric(SaleYear), y=MeanTransactedPrice, col=PlanningArea)) + 
            geom_line() +
            geom_point(size=2) +
            scale_x_continuous(breaks = as.numeric(input$SaleYear)) +
            ylab("Average Transacted Price") +
            xlab("Year of Sale") +
            ggtitle("Average Executive Condo Transacted Price per Year & Planning Area Singapore")
    }
    
    compareNewResalePrice <- function(input){
        ggplot(input) +
            geom_segment( aes(x=SaleYear, xend=SaleYear, y=MeanTransactedPrice.x, yend=MeanTransactedPrice.y), color="grey") +
            geom_point( aes(x=SaleYear, y=MeanTransactedPrice.x, col="New Sale"), size=2 ) +
            stat_summary(aes(x=SaleYear, y=MeanTransactedPrice.x), fun.y = mean, geom ='line') +
            geom_point( aes(x=SaleYear, y=MeanTransactedPrice.y, col="Resale"), size=2 ) +
            stat_summary(aes(x=SaleYear, y=MeanTransactedPrice.y), fun.y = mean, geom ='line') +
            scale_color_manual(values = saleTypeColor) +
            ylab("Average Transacted Price") +
            xlab("Year of Sale") +
            ggtitle("Comparing Average Executive Condo Transacted Price per Year, Type of Sale & Planning Area Singapore")
    }
    
    avgPricePerYrPAPie <- function(input) {
        ggplot(input, aes(x="", y=n, fill=TypeofSale)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            geom_text(
                aes(
                    #x = 0.5,
                    label = round(
                        n,
                        digits = 0
                    )
                ), 
                size=5,
                position = position_stack(vjust = 0.5)
            ) + 
            scale_fill_manual(values = saleTypeColor) +
            theme_void()
    }
    
    distNewResalePriceViolin <- function(input){
        ggplot(input) +
            geom_point(aes(x=TypeofSale, y=MeanTransactedPrice, fill=TypeofSale), shape = 23, size = 2) +
            geom_violin(aes(x=TypeofSale, y=TransactedPrice, fill=TypeofSale)) +
            facet_grid(. ~ PlanningArea) +
            scale_fill_manual(values = saleTypeColor) +
            theme(panel.spacing = unit(0, "lines"), axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.grid.major.x = element_blank(),
                  panel.border = element_rect(color = "black", fill = NA, size = 1),
                  strip.background = element_rect(color = "black", size = 1)) +
            ylab("Transacted Price") +
            xlab("Planning Area") +
            ggtitle("Distribution Executive Condo Transacted Price per Year, Planning Area & Type of Sale")
    }
    
    realValResaleHeatmap <- function(input){
        str(input)
        ggplot(input, aes(x=SaleYear, y=PlanningArea, fill=RealValueEarned)) + 
            geom_tile() +
            scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
            ggtitle("Average gain/loss from selling EC per Planning Area & Year")
    }
    
    
    ### FILTERS AND DATA
    
    avgPricePerYrPAFilter <- function(){
        propertyECData %>%
            group_by(PlanningArea, SaleYear) %>%
            summarise(MeanTransactedPrice = mean(TransactedPrice)) %>%
            filter(
                PlanningArea %in% input$PlanningAreas &
                    as.numeric(SaleYear) >= as.numeric(substr(input$Years[1],0,4)) & 
                    as.numeric(SaleYear) <= as.numeric(substr(input$Years[2],0,4))
            )
    }
    
    transPriceData <- function(){
        filteredData <- propertyECData %>%
            filter(
                PlanningArea %in% input$PlanningAreas &
                    as.numeric(SaleYear) >= as.numeric(substr(input$Years[1],0,4)) & 
                    as.numeric(SaleYear) <= as.numeric(substr(input$Years[2],0,4))
            ) %>%
            group_by(SaleYear, TypeofSale) %>%
            summarise(MeanTransactedPrice = mean(TransactedPrice))
        
        full_join(filter(filteredData, TypeofSale == "New Sale"), filter(filteredData, TypeofSale == "Resale"), by = "SaleYear")
    }
    
    avgPricePerYrPAPieFilter <- function(){
        d <- propertyECData %>%
            filter(
                PlanningArea %in% input$PlanningAreas &
                    as.numeric(SaleYear) >= as.numeric(substr(input$Years[1],0,4)) & 
                    as.numeric(SaleYear) <= as.numeric(substr(input$Years[2],0,4))
            ) %>%
            group_by(TypeofSale) %>%
            count(TypeofSale)
    }
    
    avgPricePerYrPAPieAltFilter <- function(input){
        d <- propertyECData %>%
            filter(
                PlanningArea %in% input[1] &
                    as.numeric(SaleYear) == as.numeric(input[2])
            ) %>%
            group_by(TypeofSale) %>%
            count(TypeofSale)
    }
    
    distNewResalePriceFilter <- function(){
        propertyECData %>%
            filter(
                SaleYear == input$ViolinYear
            ) %>%
            group_by(PlanningArea, TypeofSale) %>%
            mutate(
                MeanTransactedPrice = mean(TransactedPrice)
            )
    }
    
    realValResaleFilter <- function(){
        str(currentValueData)
        currentValueData %>%
            group_by(SaleYear, PlanningArea) %>%
            mutate(
                RealValueEarned = mean(PurchasePriceCurrentValue - TransactedPrice)
            )
    }
    
    
    ### OUTPUTS
    
    output$avgPricePerYrPA <- renderPlotly({
        avgPricePerYrPAClick$data <- event_data("plotly_click", source = "avgPricePerYrPASrc")
        
        ggplotly(avgPricePerYrPAChart(avgPricePerYrPAFilter()), source = "avgPricePerYrPASrc")
    })
    
    output$compareNewResalePrice <- renderPlotly({
        ggplotly(compareNewResalePrice(transPriceData()), source = "compareNewResalePriceSrc")
    })
    
    output$proportionNewAndResale <- renderPlot({
        if(is.null(avgPricePerYrPAClick$data)){
            avgPricePerYrPAPie(avgPricePerYrPAPieFilter())
        }
        else{
            data <- avgPricePerYrPAClick$data
            
            filteredPA <- sort(unique(avgPricePerYrPAFilter()$PlanningArea))
            calcIndPA <- data$curveNumber + 1
            
            year <- as.character(data$x)
            PA <- filteredPA[calcIndPA]
            
            avgPricePerYrPAPie(avgPricePerYrPAPieAltFilter(list(PA = PA, year = year)))
        }
    })
    
    output$distNewResalePrice <- renderPlotly({
        ggplotly(distNewResalePriceViolin(distNewResalePriceFilter()))
    })
    
    output$realValResaleHeatmap <- renderPlot({
        realValResaleHeatmap(realValResaleFilter())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
