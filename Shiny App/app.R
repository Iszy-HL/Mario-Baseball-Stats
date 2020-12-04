# Load packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(ggthemes)
library(scales)

# Load data
mario_data <- read_csv("Mario_Baseball_Data_update.csv")
league_data <- read_csv("leaguewide_data.csv")

# Define UI
ui <- fluidPage(
          theme = shinytheme("superhero"),
          titlePanel("How do your favorite Mario Baseball characters compare?"),
          sidebarLayout(
              sidebarPanel(
                    
                    # Select players to plot
                    selectInput(inputId = "player_name1", 
                                label = strong("Player 1"),
                                choices = sort(unique(mario_data$player_name))),
                    
                    selectInput(inputId = "player_name2", 
                                label = strong("Player 2"),
                                choices = sort(unique(mario_data$player_name))),
                    
                    #Add Shiny Logo
                    img(src='shiny_logo.png', 
                        align = "top",  
                        height = "100px")
                    
                  ),
                  
                  # Output: Lineplot, reference, and image
                  mainPanel(
                    plotOutput(outputId = "lineplot", 
                               height = "300px"),
                    tags$a(href="https://github.com/Iszy-HL/Mario-Baseball-Stats/blob/master/Mario_Baseball_Data_update.csv",
                            "Source: Collected Mario Superstar Baseball Data"),
                    tags$b("Prepared by Iszy Hirschtritt Licht"),
                    img(src='mario-baseball.png', 
                        align = "bottom",  
                        height = "300px")
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
  
  
  # Create output function
  output$lineplot <- renderPlot({
    ggplot()+
      geom_hline(yintercept = 0.334, 
                 linetype = "dashed", 
                 color = "black", 
                 size=1) +
      geom_line(selected_player_1(), 
                mapping = aes(x = cum_at_bats, 
                              y = running_avg, 
                              color = player_name)) +
      geom_line(selected_player_2(), 
                mapping = aes(x = cum_at_bats, 
                              y = running_avg, 
                              color = player_name)) +
      scale_y_continuous(labels = number_format(accuracy = .001),
                         breaks = pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = pretty_breaks(n = 10)) +
      labs(title = "Running Batting Average",
           subtitle = "Overall League Average: 0.334",
           x = "At Bats", 
           y ="Batting Average") +
      scale_colour_tableau(name = "Players:") + 
      theme(title = element_text(size = 20), 
            legend.title = element_text(size = 20), 
            legend.text = element_text(size = 15),
            legend.key.size = unit(3,"line"))  + 
    guides(colour = guide_legend(override.aes = list(size=2, stroke=1.5)))
      
  })
  
}

# Create Shiny object
shinyApp(ui, server)

#Resources:
##https://cfss.uchicago.edu/notes/shiny/

