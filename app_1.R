# load packages
library(ggplot2)
library(shiny)
library(dplyr)
library(DT)

# Read in data
dogs <- read.csv("dogs.csv")

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Dog Breed Explorer"),

    # Sidebar layout
    sidebarLayout(
        sidebarPanel(
          
          # INPPUT: checkbox for dog sizes 
          checkboxGroupInput(inputId = "size", label = h4("Size of dog"),
                             choices = unique(dogs$size_category),
                             selected = unique(dogs$size_category)), #select all as default
          
          # INPUT: select options for dog coat type
          selectInput(inputId = "coat", label = h4("Type of coat"),
                      choices = unique(dogs$coat_type),
                      selected = unique(dogs$coat_type), #select all as default
                      multiple = TRUE),
          
          # Header for new section
          h4("Personality traits"),
          
          # Descriptive text about scoring
          h5("Scored from 1:5, with 5 indicating the HIGHEST level e.g. most friendly, most playful"),
          
          br(), # blank row

          # INPUT: sliders for personality traits
          sliderInput(inputId = "openness_level",
                      label = "Opennness with strangers",
                      min = 1,
                      max = 5,
                      value = c(1,3)),
          sliderInput(inputId= "playfulness_level",
                      label = "Playfulness",
                      min = 1,
                      max = 5,
                      value = c(1,3)),
          sliderInput(inputId ="energy_level",
                      label = "Energy levels",
                      min = 1,
                      max = 5,
                      value = c(1,4)),
          sliderInput(inputId ="barking_level",
                      label = "Barking tendencies",
                      min = 1,
                      max = 5,
                      value = c(1,4)),
          
          
        ),
    
        # Show a plot of the generated distribution
        mainPanel(
          # header
          h3("Most popular dogs"),
          
          # OUTPUT: datatable showing most popular dogs within criteria
          dataTableOutput("pop_table") 
        )
    )
)

# server
server <- function(input, output) {
    
# Output - pop table
# create datatable with dog popularity rankings
output$pop_table <- DT::renderDataTable({

#  browser()
  
  # filter with slider input value
  df <- dogs %>%
    filter(openness_to_strangers >= input$openness_level[1]) %>%
    filter(openness_to_strangers <= input$openness_level[2]) %>%
    filter(energy_level >= input$energy_level[1]) %>%
    filter(energy_level <= input$energy_level[2]) %>%
    filter(barking_level >= input$barking_level[1]) %>%
    filter(barking_level <= input$barking_level[2]) %>%
    filter(playfulness_level >= input$playfulness_level[1]) %>%
    filter(playfulness_level <= input$playfulness_level[2]) %>%
    filter(size_category %in% input$size) %>%
    filter(coat_type %in% input$coat) %>%
    select(breed, x2020_rank) %>%
    arrange(x2020_rank) %>%
    rename(rank = x2020_rank) %>%
    select(rank, breed) 

  # rendering a datatable using that data
  DT::datatable(df, rownames = FALSE, options = list(pageLength = 10))

  })
}

# Run the application 
shinyApp(ui = ui, server = server)