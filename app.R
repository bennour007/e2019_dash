library(shiny)
library(shinydashboard)
library(bslib)
library(gt)
library(gfonts)


source('resources.R')
source('ggtheme.R')


ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = '3ATABA DASHBOARD'),
  dashboardSidebar(disable = T),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    fluidRow(
      column(3),
      column(6,
             box(
               width = NULL,
               status = 'primary',
               title = 'LE THERESHOLD!',
               h3('In this interactive app we aim to visualize the effect of changing the minimum percentage of the total votes aquired in a specific juristiction for each party on the overall look of the parliament.'),
               footer = 'If I recieve enough feedback on this app I may also add the effect on the results for each juristiction',
               h3('Use the slider below to set up the threshold you desire to observe, for example if you want to set the value to 2% slide the pointer to the value 0.02') 
             )
            )
      
      
    ),
    
    fluidRow(
      column(3),
      column(6,
             box(
               status = 'primary',
               width = NULL,
               h3(''),
               h3(''),
               h1(''),
               sliderInput("percent",
                           "Threshold",
                           min = 0,
                           max = 0.2,
                           value = 0)
             )
             ),
      column(5)
    ),
    
    fluidRow(
      column(1),
      column(7,
             box(
               width = NULL,
               h1(' '),
               h3("The corresponding parliament:"),
               h1(''),
               plotOutput("seats"),
               h1(' '),
               h3('Comparaison between the chosen & the current threshold for the top 7 parties:'),
               h1(''),
               plotOutput("comp")
             )
      ),
      column(3,
             box(
               width = NULL,
               h1(' '),
               h3('The percentage of the direct votes(quota) seats to the total seats'),
               h1(''),
               tableOutput("ratio")
             )
      ),
      column(1)
    )
  )
)
    
    
  



server <- function(input, output) {
  
  thematic::thematic_shiny()
  
  output$seats <- renderPlot({
    # generate bins based on input$bins from ui.R
    data_to_use <- good_data(data_ready, x = input$percent)  
    
    data_to_use %>% 
      mutate(party = fct_reorder(party, total_seats)) %>% 
      ggplot() +
      geom_col(aes(x = total_seats,
                   y = party), 
               fill = "skyblue") +
      labs(title = 'The effect of your chosen threshold on the overall look of the parliament',
           y = 'Parties',
           x = 'Total number of seats') +
      dash_theme
    
  })
  output$comp <- renderPlot({
    j_c(per = input$percent)
  })
  
  output$ratio <- renderTable({
    
    data_to_use <- good_data(data_ready, x = input$percent)
    
    data_to_use %>%
      mutate(diff = (total_seats - r_seats)/total_seats,
             diff = 100*round(diff, 2)) %>%
      arrange(-diff) %>% 
      gt() 
  })
}


shinyApp(ui, server)



