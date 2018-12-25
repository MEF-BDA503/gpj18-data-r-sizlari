
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
```