#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages <- c("shiny", "tidyverse", "plotly")

for(p in packages){library
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

propertyECData <- list.files(path = "data", pattern = "*.csv") %>%
    str_c("data/", .) %>%
    map_df(~read_csv(., col_types = cols("Completion Date" = col_character()))) %>%
    select(., -c("No. of Units", "Property Type", "Type of Area"))

names(propertyECData) <- str_replace_all(names(propertyECData), "[ ($)]*", "")

# propertyECData$SaleYear <- str_extract_all(propertyECData$SaleDate, "[0-9]{4}")

propertyECData$SaleYear <- substr(propertyECData$SaleDate, nchar(propertyECData$SaleDate)-3, nchar(propertyECData$SaleDate))

minYear <- as.Date(min(propertyECData$SaleYear), format = "%Y")
maxYear <- as.Date(max(propertyECData$SaleYear), format = "%Y")



# Functions
lineChart <- function(input, output){
    ggplot(input, aes(x=SaleYear, y=TransactedPrice, col=PlanningArea)) + 
        geom_line() +
        geom_point(size=2) +
        stat_summary(fun.y = mean, geom ='line')
}




# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("EC Inspector Dashboard"),
    
    fluidRow(
        column(12,
            selectInput(inputId = "PlanningAreas",
                label = "Select planning areas to compare",
                choices = unique(propertyECData$PlanningArea),
                selected = unique(propertyECData$PlanningArea)[1:2],
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
            plotlyOutput("lineChart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    lineChartFilter <- reactive({
        filter(
            as.data.frame(propertyECData), 
            PlanningArea %in% input$PlanningAreas &
                as.Date(SaleYear, format="%Y") >= as.Date(input$Years[1], format="%Y") & 
                as.Date(SaleYear, format="%Y") <= as.Date(input$Years[2], format="%Y")
        )
    })
    
    output$lineChart <- renderPlotly({
        ggplotly(lineChart(lineChartFilter()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
