library(queuecomputer)
library(shiny)

#### Define UI ####
ui <- fluidPage(
    
    # Application title
    titlePanel("Testing queuecomputer in shiny"),
    h3("An illustration of unexpected and expected behaviour"),
    
    # Show a plot of the generated distribution
    mainPanel(
        column(width = 5,
               h4("This doesn't work"),
               helpText("The issue seems to arise when the number of servers can be set dynamically"),
               sliderInput('rateA1', 'Inter-arrival time', min = 1, max = 10, value = 1, step = .1),
               sliderInput('rateS1', 'Service time', min = 1, max = 10, value = 1, step = .1),
               sliderInput('servers1', 'Number of servers', min = 1, max = 10, value = 3, step = 1),
               textOutput("test1")
        ),
        
        column(width=2),
        
        column(width = 5,    
               h4("This does work"), 
               helpText("Things work as expected when the number of servers is fixed (playing with the sliders will change the utilisation factor)"),
               sliderInput('rateA2', 'Inter-arrival time', min = 1, max = 10, value = 1, step = .1),
               sliderInput('rateS2', 'Service time', min = 1, max = 10, value = 1, step = .1),
               textOutput("test2")
        )
        
    )
    
)

#### Define server logic #### 

server <- function(input, output) {
    
    # A reactive queue_list that updates when the user changes the number of servers     
    
    # This throws an error
    queue1 <- reactive({
        a = cumsum(rexp(100, 1/input$rateA1)) # arrival times
        s = rexp(100, 1/input$rateS1) # service times
        q <- queuecomputer::queue_step(arrivals = a, service = s, servers = as.numeric(input$servers1))
        return(q)
    })    
    
    # This works as expected
    queue2 <- reactive({
        a = cumsum(rexp(100, 1/input$rateA2)) # arrival times
        s = rexp(100, 1/input$rateS2) # service times
        q <- queuecomputer::queue_step(arrivals = a, service = s, servers = 2) 
        return(q)
    })    
    
    # The output to report: the utilisation factor returned by summary.queue_list    
    output$test1 <- renderText({
        x <- summary(queue1())
        paste('utilisation factor =', round(x$utilization, digits = 3))
    })
    
    output$test2 <- renderText({
        x <- summary(queue2())
        paste('utilisation factor =', round(x$utilization, digits = 3))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
