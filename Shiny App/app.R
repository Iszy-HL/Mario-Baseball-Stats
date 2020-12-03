# Load packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)

# Load data
mario_data <- read_csv("Mario_Baseball_Data_update.csv")

# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("Running Batting Averages"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select players to plot
                    selectInput(inputId = "player_name1", label = strong("Player 1"),
                                choices = unique(mario_data$player_name)),
                    
                    selectInput(inputId = "player_name2", label = strong("Player 2"),
                                choices = unique(mario_data$player_name))
                    
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    tags$a("Source: Collected Mario Superstar Baseball Data", target = "_blank")
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_player_1 <- reactive(
    mario_data %>%
      filter(
        player_name == input$player_name1
        ))
  
  selected_player_2 <- reactive(
    mario_data %>%
      filter(
        player_name == input$player_name2
      ))
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    ggplot()+
      geom_line(selected_player_1(), mapping = aes(x = cum_at_bats, y = running_avg)) +
      geom_line(selected_player_2(), mapping = aes(x = cum_at_bats, y = running_avg)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
      scale_color_discrete("Players") +
      ylim(0, .5) +
      labs(title = "Running Batting Average",
           x = "At Bats", y ="Batting Average") +
      theme_economist() 
  })
  
}

# Create Shiny object
shinyApp(ui, server)

