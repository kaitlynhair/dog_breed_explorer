library(ggplot2)
library(shiny)
library(dplyr)
library(DT)

# Read in data
dogs <- read.csv("dogs.csv")

# define UI 
ui <- fluidPage(

    # application title
    titlePanel("Dog Breed Explorer"),

    # sidebar layout
    sidebarLayout(
        sidebarPanel(
          
          h2("Select Dog Breed(s)"),
          
          # INPUT: select input for different breeds
          selectInput(inputId = "breed", label = h4("Select dog breed(s)"),
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
          ))))


# server
server <- function(input, output) {
    
  # reactive dataframe with selected dogs
  selected_dogs <- reactive({
    
    dogs <- dogs %>%
      filter(breed %in% input$breed)
  })
  
  # output - popularity
  # generating plot for popularity
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
