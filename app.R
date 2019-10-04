#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(plotly)
library(tidyverse)

raw_df <- read_csv(file = "data/refugee-data/asylum_seekers_monthly.csv")

data_df <- raw_df %>% 
            rename(Destination = `Country / territory of asylum/residence`) %>% 
            unite(Month, Year, col = Date, sep = "-") %>% 
            mutate(Date = parse_date(x = Date, format = "%B-%Y"))

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Asylum Seekers Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
        
         dateRangeInput(inputId = 'dateRange',
                        label = 'Date range input: yyyy-mm-dd',
                        start = min(data_df$Date), 
                        end = max(data_df$Date), 
                        min = min(data_df$Date), 
                        max = max(data_df$Date)), 
        
         sliderInput(inputId = "bins",
                     label = "Number of bins:",
                     min = 1,
                     max = 500000,
                     value = 8000), 
         
         selectInput(inputId = "target_name",
                     label = "Country of Destination", 
                     choices = unique(data_df$Origin), selected = "Germany"),
         
         selectInput(inputId = "origin_name",
                     label = "Country of Origin", 
                     choices = unique(data_df$Origin))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        plotlyOutput("barPlot"),
        
        plotlyOutput("linePlot")
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$barPlot <- renderPlotly({
      
     plt <- data_df %>% 
             filter(Destination == input$target_name) %>% 
             filter(Date > input$dateRange[1], Date < input$dateRange[2]) %>% 
             group_by(Origin) %>% 
             summarise(Value = sum(Value)) %>% 
             filter(Value > input$bins) %>% 
             mutate(Origin = reorder(Origin, Value)) %>% 
             ggplot() +
             geom_col(mapping = aes(x = Origin, y = Value), fill = "blue") +
             xlab(NULL) + ylab(NULL) + 
             coord_flip() +
             ggtitle(label = str_c("Number of Asylum Seekers in: ", input$target_name))
           
     plotly::ggplotly(plt)
     
   })
   
   
   output$linePlot <- renderPlotly({
     
     plt <- data_df %>% 
              filter(Destination == input$target_name) %>% 
              filter(Date > input$dateRange[1], Date < input$dateRange[2]) %>% 
              filter(Origin == input$origin_name) %>% 
              ggplot(mapping = aes(x = Date, y = Value)) +
              geom_line() +
              ggtitle(label = str_c("Number of Asylum Seekers in: ", input$target_name))
     
     plotly::ggplotly(plt)
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

