library(shiny)
library(ggplot2)
library(dplyr)

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-18"), ".xlsx", sep = "")
httr::GET(url, httr::authenticate(":", ":", type="ntlm"), httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- readxl::read_excel(tf)

df  <- df %>%
  rename(country = countriesAndTerritories) %>% 
  arrange(country, dateRep) %>%
  group_by(country) %>%
  mutate(Cumulative_Death = cumsum(deaths)) %>%
  ungroup() %>%
  filter(Cumulative_Death > 9) %>%
  group_by(country) %>%
  mutate(numbers_of_days = row_number(),
         First_Death_Date = min(dateRep)) %>% 
  select(country, numbers_of_days, deaths, Cumulative_Death)

ui <- fluidPage(
  titlePanel("Statistik Covid-19"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Country:", choices = unique(df$country), selected = "Sweden", multiple = TRUE),
      selectInput("var", "Variable:", choices = c("deaths", "Cumulative_Death"))),
    mainPanel(
      plotOutput("covid"))
  ))

server <- function(input, output, session){
  
  # input$country instead input$land
  selected <- reactive(filter(df, country %in% input$country))
  
  output$covid <- renderPlot({
    # switch to aes_string. map colour on country instead of input$land
    ggplot(selected(), aes_string(x = "numbers_of_days", y = input$var, colour = "country")) + 
      geom_line(size = 1.5) +
      labs(title = "Covid-19: Antal döda per 100 000 invånare",
           x = "DAGAR SEDAN ANTAL DÖDSFALL ÖVERSTEG TIO",
           y = paste0(input$var),
           caption = "Source: European Centre for Disease Prevention and Control") +
      guides(colour = guide_legend(title=NULL))
  })
}

shinyApp(ui, server)

