#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library('tidyverse')
library('plotly')
flights <- read_delim('data/nycflights13.csv')
# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Flight Stats",
    tabPanel("About",
             h1("2013 NYC Flights"),
             p("New York City is known to be a hub of culture, commerce,
               entertainment, and in this case,",
               em("transportation"),
               ". I'm looking at flights from 3 NYC airports in 2013:
               JFK, EWR, and LGA. A couple of questions I aim to answer 
               include which airport is more commonly used for longer, or 
               international travel, are there any times of the year where 
               travel distance tends to be longer? I mainly look at flight 
               distances as my responding variable."),
             
             mainPanel(
               textOutput("mptxt"),
               tableOutput("mptbl")
             )
             ),
    
    tabPanel("Plot",
    
    titlePanel("A plot of the flight distances"),
    
    sidebarLayout(
      sidebarPanel(
        
        sliderInput("count", "How many flights:",
                    min = 10,
                    max = 1000,
                    value = 200),
        sliderInput("sizeye", "Size of points:",
                    min = 5,
                    max = 15,
                    value = 5),
        fluidRow(column(6,
                        uiOutput("checkboxOri")
                        ),
                
      )
      
      ),
      mainPanel(
        plotlyOutput("plot"),
        textOutput("plotlbl")
        
      )
    )
    ),
    tabPanel("Table",
             
    titlePanel("A table of the flight data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
       sliderInput("Months", label = h3("Choose Months"), min = min(flights$month),
                   max = max(flights$month), value = c(1,12)),
       sliderInput("Days", label = h3("Choose Days"), min = min(flights$day),
                   max = max(flights$day), value = c(1,31))
      ),
      
      mainPanel(
        dataTableOutput("flight_table"),
        textOutput("tbltxt")
        
      )
    ))
)
)



    
    # Application title
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  output$flight_table <- renderDataTable({
    flights %>% 
      filter(month >= input$Months[1],
             month <= input$Months[2]) %>% 
      filter(day >= input$Days[1],
             day <= input$Days[2])
  })

  output$checkboxOri <- renderUI({
    checkboxGroupInput("origin", "Choose starting airport",
                       choices = unique(flights$origin),
                       selected = "EWR"
    )
  })
  sample <- reactive({
    s1 <- flights %>%
      filter(origin %in% input$origin)
    if(nrow(s1) > input$count)
      sample_n(s1, input$count)
    else
      s1
  })
  
  output$mptbl <- renderTable({
    flights %>% 
      sample_n(5) %>% 
      select("year", "month", "day", "carrier", "origin", "dest", "distance")
  })
  
  output$plot <- renderPlotly({
    plot_ly(data = sample(),
            x = ~month, y = ~distance, color = ~origin, 
            marker = list(size = input$sizeye),
            type = 'scatter',
            mode = 'markers')
  })  
  output$plotlbl <- renderText({paste("Of the selected airport origins,
     the farthest flight
                                      distance is", max(sample()$distance), "miles.")})
  
  output$mptxt <- renderText({paste("Sample data of select columns")})
  dateys <- reactive({
    c("Data is in a range of ", input$Months[1], "to", input$Months[2], "months,
      and", input$Days[1], "to", input$Days[2], "days")
  })
  output$tbltxt <- renderText({paste(dateys())})
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
