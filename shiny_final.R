
library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(lubridate)
library(reshape2)
library(dplyr)
library(shiny)

tmp=tempfile(fileext=".rds")
download.file("https://github.com/MEF-BDA503/gpj18-data-r-sizlari/blob/master/wfp_food_prices_turkey.rds?raw=true",destfile=tmp,mode='wb')
refined_data=read_rds(tmp)
file.remove(tmp)

data1 <- refined_data %>%
    group_by(type, market, name, year) %>%
    summarise(yearly_average_price=mean(value))

food_types <- refined_data %>%
    distinct(type) %>%
    unlist(.)

markets <- refined_data %>%
    distinct(market) %>%
    unlist(.)

years <- refined_data %>%
    distinct(year) %>%
    unlist(.)

names(food_types) <- NULL
names(markets) <- NULL
names(years) <- NULL

ui <- fluidPage(
    
    titlePanel("Food Prices in Turkey"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId ="food_type",
                        label = "Categories",
                        choices = unique(as.character(food_types))),
            selectInput(inputId = "markets",
                        label = "Market",
                        choices = unique(as.character(markets))),
            uiOutput('input3'),
            sliderInput("years", 
                        "Years",
                        min = min(refined_data$year),
                        max = max(refined_data$year),
                        value=c(2013, 2018), 
                        sep="",
                        step = 1)
        ),
        mainPanel(
            plotOutput("foodPlot")
        )
    )
)

server <- function(input, output) {
    
    output$input3 <- renderUI({
        names <- refined_data %>% 
            filter(type == input$food_type) %>% 
            distinct(name) %>% 
            unlist(.)
        names(names) <- NULL
        selectInput(inputId = "names",
                    label = "Product Name",
                    choices = c("All", unique(as.character(names))))
    })
    
    output$foodPlot <- renderPlot({
        
        food_type_data_set <- data1 %>% 
            filter(market == input$markets & type == input$food_type & year >= input$years[1] & year <= input$years[2])
        
        if(input$names != "All"){
            food_type_data_set <- food_type_data_set %>% 
                filter(name == input$names)
        } else {
            food_type_data_set <- refined_data %>%
                group_by(type, market, year) %>%
                summarise(yearly_average_price=mean(value)) %>%
                filter(market == input$markets & type == input$food_type & year >= input$years[1] & year <= input$years[2])
        }
        
        ggplot(food_type_data_set, aes(x = year, y = yearly_average_price)) +
            geom_line(stat = "identity") + aes(x = year, y = yearly_average_price) +
            geom_point()
        
    })
}

shinyApp(ui = ui, server = server)