
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