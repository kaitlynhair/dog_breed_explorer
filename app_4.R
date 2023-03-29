library(ggplot2)
library(shiny)
library(dplyr)
library(DT)

# read in data
dogs <- read.csv("dogs.csv")

# define UI 
ui <- fluidPage(theme = shinythemes::shinytheme("united"), #add a theme!
  
  # use shiny feedback              
  shinyFeedback::useShinyFeedback(),
  
  # navbar page layout with pages at top
  navbarPage("Dog Breed Explorer",
             
           # page 1
             tabPanel("Explore dog breeds",
                      
                      # sidebar layout 
                      sidebarLayout(
                        
                        # sidebar
                        sidebarPanel(
                          h2("Breed Characteristics"), 
                          
                          # INPPUT: checkbox for dog sizes 
                           shinyWidgets::checkboxGroupButtons(
                            inputId = "size",
                            size = "sm",
                            label = h4("Size of dog"),
                            choices =  c("X-Small","Small","Medium","Large", "X-Large"),
                            selected =  c("X-Small","Small","Medium","Large", "X-Large"),
                            individual = TRUE,
                            status = "primary",
                            checkIcon = list(
                              yes = icon("square-check"),
                              no = icon("square")
                            )),
                          
                          # INPUT: drop down options for dog coat type
                          selectInput("coat", label = h4("Type of coat"),
                                      choices = unique(dogs$coat_type),
                                      selected = unique(dogs$coat_type),
                                      multiple = TRUE),
                          
                          h2("Personality traits"),
                          h5("Scored from 1:5, with 5 indicating the HIGHEST level e.g. most friendly, most playful"),
                          br(), # blank row
                          
                          # INPUT: sliders for personality traits
                          # sliders
                          sliderInput("openness_level",
                                      label = "Opennness with strangers",
                                      min = 1,
                                      max = 5,
                                      value = c(1,5)),
                          sliderInput("playfulness_level",
                                      label = "Playfulness",
                                      min = 1,
                                      max = 5,
                                      value = c(1,5)),
                          sliderInput("energy_level",
                                      label = "Energy levels",
                                      min = 1,
                                      max = 5,
                                      value = c(1,5)),
                          sliderInput("barking_level",
                                      label = "Barking tendencies",
                                      min = 1,
                                      max = 5,
                                      value = c(1,5)),
                          
                          
                          shiny::actionButton(inputId = "goButton", "Filter dogs!")
                          # action button to initiate filtering
                          # shinyWidgets::actionBttn("goButton", "Filter dogs!",
                          #                          color = "success", 
                          #                          style = "pill")
                          
                          
                        ),
                        
                        # main panel
                        mainPanel(
                          
                          h3("Most popular dogs"),
                          
                          # OUTPUT: datatable showing most popular dogs within criteria
                          dataTableOutput("rank_table") 
                        ))),
             
             # page 2
             tabPanel("Compare dog breeds",
                      
                      # sidebar layout        
                      sidebarLayout(
                        sidebarPanel(
                          
                          h2("Select Dog Breed(s)"),
                          
                          # INPUT: drop down for different breeds
                          selectInput("breed", label = h4("Select dog breed(s)"),
                                      choices = unique(dogs$breed),
                                      selected =unique(dogs$breed)[c(25,83)], # 2 selected as default
                                      multiple = TRUE) # multiple selections possible
                        ),
                        
                        # main panel
                        mainPanel(
                          tabsetPanel(
                            # OUTPUT: radar plot of personality traits for given breed(s)
                            tabPanel("Personality", plotOutput("radar_pers", height = "800px", width="900px")),
                            # OUTPUT: bar plot of other traits for given breed(s)
                            tabPanel("Other characteristics", plotOutput("bar_characteristics", height = "600px")),
                            # OUTPUT: line plot of popularity for given breed(s) over time
                            tabPanel("Popularity over time", plotOutput("popularity",  height = "600px"))
                          ))))))
           



# server
server <- function(input, output) {
  
  # create datatable with dog popularity rankings
  output$rank_table <- DT::renderDataTable({
    
    # Take a dependency on input$goButton
    input$goButton
    
    # creating a dataframe with the input filters applied
    df <- dogs %>%
      filter(openness_to_strangers >= isolate(input$openness_level[1])) %>%
      filter(openness_to_strangers <= isolate(input$openness_level[2])) %>%
      filter(energy_level >= isolate(input$energy_level[1])) %>%
      filter(energy_level <= isolate(input$energy_level[2])) %>%
      filter(barking_level >= isolate(input$barking_level[1])) %>%
      filter(barking_level <= isolate(input$barking_level[2])) %>%
      filter(playfulness_level >= isolate(input$playfulness_level[1])) %>%
      filter(playfulness_level <= isolate(input$playfulness_level[2])) %>%
      filter(size_category %in% isolate(input$size)) %>%
      filter(coat_type %in% isolate(input$coat)) %>%
      select(breed, x2020_rank, image) %>%
      arrange(x2020_rank) %>%
      rename(rank = x2020_rank) %>%
      select(rank, breed, image) %>%
      mutate(image = paste0("<img src=", "'", image, "'", " height='72'></img>"))
    
    # df <- dogs %>%
    #   filter(openness_to_strangers >= input$openness_level[1]) %>%
    #   filter(openness_to_strangers <= input$openness_level[2]) %>%
    #   filter(energy_level >= input$energy_level[1]) %>%
    #   filter(energy_level <= input$energy_level[2]) %>%
    #   filter(barking_level >= input$barking_level[1]) %>%
    #   filter(barking_level <= input$barking_level[2]) %>%
    #   filter(playfulness_level >= input$playfulness_level[1]) %>%
    #   filter(playfulness_level <= input$playfulness_level[2]) %>%
    #   filter(size_category %in% input$size) %>%
    #   filter(coat_type %in% input$coat) %>%
    #   select(breed, x2020_rank, image) %>%
    #   arrange(x2020_rank) %>%
    #   rename(rank = x2020_rank) %>%
    #   select(rank, breed, image) %>%
    #   mutate(image = paste0("<img src=", "'", image, "'", " height='72'></img>"))
    
    # rendering a datatable using that data
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 10), escape=FALSE) # escape is false to allow HTML code in images
    
  })
  
  # reactive dataframe with selected dogs
  selected_dogs <- reactive({
    
    dogs <- dogs %>%
      filter(breed %in% input$breed)
  })
  output$popularity <-renderPlot({
    
    selected_dogs() %>%
      select(breed, contains("_rank")) %>%
      tidyr::pivot_longer(-breed, names_to ="Year", values_to="Popularity Ranking") %>%
      mutate(Year = gsub("x", "", Year)) %>%
      mutate(Year = gsub("_rank", "", Year)) %>%
      select(breed, Year, `Popularity Ranking`) %>%
      rename(Breed = breed) %>%
      ggplot(aes(x=Year, y=`Popularity Ranking`, group=Breed, colour=Breed)) +
      geom_line(linewidth=3) + 
      scale_y_reverse() +
      theme_minimal() +
      theme(text = element_text(size = 16)) +
      scale_colour_manual(values = c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
      )
    
  })
  
  # render radar plot with personality traits
  output$radar_pers <-renderPlot({
    
    # select relevant columns of reactive dataframe
    data <- selected_dogs()[,c(1,6:8, 14:21)]
    
    # user feedback
    # more_than_3 <- length(input$breed) > 3 #logical value
    # shinyFeedback::feedbackWarning(inputId = "breed", show = more_than_3, text = "Please select three or fewer breeds to compare!")

    # make breed the row name
    rownames(data) <- paste0(data$breed)
    data <- data %>%
      select(-breed)
    
    # add two lines to dataframe - with 11 columns
    # all values for first line should be max value in radar
    # all values for second line should be min value in radar
    data <- rbind(rep(5,11) , rep(0,11), data)
    
    # check data
    head(data)
    
    # Color vector
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    
    # plot with default options:
    fmsb::radarchart(data , axistype=1 , 
                     #custom polygon
                     pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                     #custom the grid
                     cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.8,
                     #custom labels
                     vlcex=0.8 
    ) +
      theme_minimal() +
      theme(text = element_text(size = 16))
    
    legend(x=0.8, y=0.8, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
    
    
  })
  
  # render bar plot with characteristics
  output$bar_characteristics <-renderPlot({
    
    selected_dogs()[,c(1,5,9:11)] %>% # select relevant columns
      mutate(size_category = gsub("Medium", 3, size_category)) %>% #convert text to numerical values
      mutate(size_category = gsub("X-Large", 5, size_category)) %>%
      mutate(size_category = gsub("X-Small", 1, size_category))  %>%
      mutate(size_category = gsub("Large", 4, size_category)) %>%
      mutate(size_category = gsub("Small", 2, size_category)) %>% 
      mutate(size_category = as.numeric(size_category)) %>%
      tidyr::pivot_longer(-breed, names_to="characteristic", values_to="score") %>%
      ggplot(aes(x=characteristic, y=score, fill=breed)) +
      geom_line(aes(group = characteristic), linewidth=2) +
      geom_point(aes(color = breed), size=7,  alpha=0.7) +
      coord_flip() +
      theme_minimal() +
      labs(y = "Lower -> Higher Scores", x= "") +
      theme(text = element_text(size = 16)) + 
      scale_colour_manual(values = c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
      )
  })

 
}
# Run the application 
shinyApp(ui = ui, server = server)
