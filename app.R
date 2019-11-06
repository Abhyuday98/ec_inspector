#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


### PACKAGES

packages <- c("shiny", "tidyverse", "plotly")

for(p in packages){library
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}


### TRANFORMATION OF DATA

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
        column(12,
            selectInput(inputId = "PlanningAreas",
                label = "Select planning areas to compare",
                choices = selectPAList,
                selected = selectPAList[1:2],
                multiple = TRUE),
            sliderInput(inputId = "Years",
                label = "Select a year range",
                step = 1,
                min = minYear,
                max = maxYear,
                value = c(minYear, maxYear),
                dragRange=TRUE,
                timeFormat = "%Y")
        ),
        column(12,
            plotlyOutput("avgPricePerYrPA"),
            plotOutput("proportionNewAndResale"),
            plotlyOutput("compareNewResalePrice"),
            selectInput(inputId = "ViolinYear",
                label = "Select planning areas to compare",
                choices = allYears,
                selected = allYears[1]),
            plotlyOutput("distNewResalePrice")
        )
    )
)

server <- function(input, output) {
    
    ### COMMON DATA
    
    saleTypeColor <- c("New Sale" = "darkturquoise", "Resale" = "darkorange")
    
    
    ### REACTIVE DATA
    
    avgPricePerYrPASel <- reactiveValues(data = NULL)
    avgPricePerYrPAClick <- reactiveValues(data = NULL)
    
    ### CHARTS
    
    avgPricePerYrPAChart <- function(input){
        ggplot(input, aes(x=SaleYear, y=MeanTransactedPrice, col=PlanningArea)) + 
            geom_line() +
            geom_point(size=2) +
            stat_summary(fun.y = mean, geom ='line') +
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
        ggplot(input, aes(x="", y=MeanTransactedPrice, fill=TypeofSale)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            geom_text(
                aes(
                    x = 0.5,
                    label = paste(
                        round(
                            input$MeanTransactedPrice/sum(input$MeanTransactedPrice)*100,
                            digits = 0
                        ), 
                    "%")
                ), 
                size=5
            ) + 
            scale_fill_manual(values = saleTypeColor) +
            theme_void()
    }
    
    distNewResalePriceViolin <- function(input){
        ggplot(input, aes(x=PlanningArea, y=TransactedPrice, fill=TypeofSale)) +
            geom_violin() +
            scale_fill_manual(values = saleTypeColor) +
            ylab("Transacted Price") +
            xlab("Planning Area") +
            ggtitle("Distribution Executive Condo Transacted Price per Year, Planning Area & Type of Sale")
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
       propertyECData %>%
            filter(
                PlanningArea %in% input$PlanningAreas &
                    as.numeric(SaleYear) >= as.numeric(substr(input$Years[1],0,4)) & 
                    as.numeric(SaleYear) <= as.numeric(substr(input$Years[2],0,4))
            ) %>%
            group_by(TypeofSale) %>%
            summarise(MeanTransactedPrice = mean(TransactedPrice))
    }
    
    avgPricePerYrPAPieAltFunc <- function(input){
        propertyECData %>%
            filter(
                PlanningArea %in% input[1] &
                    as.numeric(SaleYear) == as.numeric(input[2])
            ) %>%
            group_by(TypeofSale) %>%
            summarise(MeanTransactedPrice = mean(TransactedPrice))
    }
    
    avgPricePerYrPAPieAltFilter <- function(input){
        length <- length(input$PA)
        
        if(length < 2){
            avgPricePerYrPAPieAltFunc(c(input$PA[1], input$year[1]))
        }
        else{
            i <- 0
            data <- NULL
            
            while(i < length){
                dataAddOns <- avgPricePerYrPAPieAltFunc(c(input$PA[i+1], input$year[i+1]))
                
                if(is.null(data)){
                    data <- dataAddOns
                }
                else{
                    data <- bind_rows(dataAddOns, data)
                }
                
                i <- i + 1
            }
            
            data %>%
                group_by(TypeofSale) %>%
                summarise(MeanTransactedPrice = mean(MeanTransactedPrice))
        }
    }
    
    distNewResalePriceFilter <- function(){
        propertyECData %>%
            filter(
                SaleYear == input$ViolinYear
            ) %>%
            group_by(PlanningArea, TypeofSale)
    }
    
    
    ### OUTPUTS
    
    output$avgPricePerYrPA <- renderPlotly({
        avgPricePerYrPASel$data <- event_data("plotly_selected", source = "avgPricePerYrPASrc")
        avgPricePerYrPAClick$data <- event_data("plotly_click", source = "avgPricePerYrPASrc")
        
        ggplotly(avgPricePerYrPAChart(avgPricePerYrPAFilter()), source = "avgPricePerYrPASrc")
    })
    
    output$compareNewResalePrice <- renderPlotly({
        ggplotly(compareNewResalePrice(transPriceData()), source = "compareNewResalePriceSrc")
    })
    
    output$proportionNewAndResale <- renderPlot({
        if(is.null(avgPricePerYrPASel$data) & is.null(avgPricePerYrPAClick$data)){
            avgPricePerYrPAPie(avgPricePerYrPAPieFilter())
        }
        else{
            data <- if(is.null(avgPricePerYrPAClick$data)){
                avgPricePerYrPASel$data
            }
            else{
                avgPricePerYrPAClick$data
            }
            
            filteredPA <- sort(unique(avgPricePerYrPAFilter()$PlanningArea))
            calcIndPA <- data$curveNumber - (length(filteredPA) - 1)

            year <- sort(unique(avgPricePerYrPAFilter()$SaleYear))[data$x]
            PA <- filteredPA[calcIndPA]

            avgPricePerYrPAPie(avgPricePerYrPAPieAltFilter(list(PA = PA, year = year)))
        }
    })
    
    output$distNewResalePrice <- renderPlotly({
        ggplotly(distNewResalePriceViolin(distNewResalePriceFilter()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
