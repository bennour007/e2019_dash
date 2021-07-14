
library(shiny)
library(bslib)

source('resources.R')
source('ggtheme.R')

ui <- fluidPage(
    # adding theme wit h bslib
    
    theme = bs_theme(version = 4, bootswatch = "minty"),
    # Application title
    titlePanel("How much of a threshold is needed to form a majority in the parliament?"),
    fluidRow(
        column(5),
        column(6,
               h3(''),
               h3(''),
               h1(''),
               sliderInput("percent",
                           "Threshold",
                           min = 0,
                           max = 0.2,
                           value = 0)
        ),
        column(5)
    ),
    
    fluidRow(
        column(3),
        column(6,
               h3("The corresponding parliament:"),
               plotOutput("seats")
        ),
        column(3)
    )
    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("percent",
    #                     "Threshhold on the percentqge of votes on every juristiction",
    #                     min = 0,
    #                     max = 0.2,
    #                     value = 0)
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("seats")
    #     )
    # )
)


server <- function(input, output) {
    
    output$seats <- renderPlot({
        # generate bins based on input$bins from ui.R
        data_to_use <- good_data(data_ready, x = input$percent)  
        
        data_to_use %>% 
            mutate(party = fct_reorder(party, total_seats)) %>% 
            ggplot() +
            geom_col(aes(x = total_seats,
                         y = party)) + 
            theme_light()
        
    })
}

shinyApp(ui, server)



