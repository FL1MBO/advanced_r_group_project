library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2movies)

movies <- readRDS("data/clean_movies.rds")
movies_per_decade <- readRDS("data/movies_per_decade.rds")


ui <- fluidPage(
  # Create overal nagivation panes
  navbarPage("Movie rating analysis", theme = shinytheme("lumen"),
             # Tab for the temporal analysis
             tabPanel("Overview of data", fluid = TRUE,
                      mainPanel(
                        h3(verbatimTextOutput("info_data")),
                        tableOutput("table_1"))),
             tabPanel("Temporal analysis", fluid = TRUE,
                      # Sidebar with specific interactive elements for the temporal plots
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Temporal plot filters"),
                          fluidRow(column(3, 
                             
                                          # slider for the range movie release per year
                                          sliderInput(inputId = "range_years",
                                                      label = "Set the range of year of movie release:",
                                                      min = 1893,
                                                      max = 2005,
                                                      value = c(1893, 2005),
                                                      step = 1,
                                                      sep = "" ),
                                          
                                          # slider for the range movie release per decade
                                          sliderInput(inputId = "range_decades",
                                                      label = "Set the range of decade of movie release:",
                                                      min = 1890,
                                                      max = 2000,
                                                      value = c(1890, 2000),
                                                      step = 10,
                                                      sep = ""),
                                          
                                          # Check box for scaling on average votes per year
                                          checkboxInput(inputId = "votes",
                                                        label = "Scale ratings based on number of votes",
                                                        value = FALSE),
                                          
                                          # Check box for scaling on average number of movies per year
                                          checkboxInput(inputId = "number_movies",
                                                        label = "Scale ratings based on number of movies released", 
                                                        value = FALSE),
                                          
                                          # Check box group for genres
                                          checkboxGroupInput(inputId = "genre",
                                                             label = "Choose a genre (a movie can have multiple genres)",
                                                             choices = list("Action" = 1, "Animation" = 2, "Comedy" = 3, "Drama" = 4, "Documentary" = 5,"Romance" = 6),
                                                             selected = c(1, 2, 3, 4, 5, 6)),
                          )))),
                      mainPanel(plotOutput("plot_year"),
                                plotOutput("plot_decade"),
                                plotOutput("plot_genre"))
                      ),
             # Second tab for the other plot
             tabPanel("Voting analysis", fluid = TRUE,0
                      # Sidebar with specific interactive elements for the temporal plots
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Voting plot filters"),
                          fluidRow(column(3,
                                          # Slider for the range of votes
                                          sliderInput(inputId = "range_votes",
                                                      label = "Minimum number of votes",
                                                      min = min(movies$votes),
                                                      max = max(movies$votes),
                                                      value = c(min(movies$votes,max(movies$votes)))))))),
                      mainPanel(navset_card_underline(
                        title = "Analysis",
                        nav_panel("Plot rating decades", plotOutput("plot_votes")),
                        nav_panel("Regression", verbatimTextOutput("text_regression"), "Table regression", tableOutput("table_regression")),
                      )
             ))
             )
  )
)

  

server <- function(input, output){
  
## Part 1: information on the data
  output$info_data <- renderText({
"This dataset contains movie ratings for many movies released between 1893 - 2005.
The data contains 58,788 movies and 24 columns. These columns are: 
  - Movie title
  - Year
  - Length (minutes)
  - Budget
  - Rating
  - Votes
  - r1-10 (percentile per score 1-10, rounded to nearest 10%)
  - MPAA rating
  - Genre (action, animation, comedy, drama, documentary, romance,
    short)
The data is can be checked in the following table:"
  })

## Part 2: table with the number of movies per decade
  output$table_decades <- renderTable(movies_per_decade)
  
## Part 3: add Luisa's table  
  output$table_1 <- renderTable(movies_per_decade)

## Part 4: plot of the average rating per year
  output$plot_year <- renderPlot({ 
      movies |> filter(
        year >= input$range_years[1], 
        year <= input$range_years[2]
      ) |> 
      ggplot(aes(year, mean_rating)) + 
        geom_line() +
        xlab("year of movie release") +
        ylab("mean rating") + scale_x_continuous(breaks = seq(1890, 2010, 10))
      
  })
    
### Part 5: Plot of the average rating per decade 
  output$plot_decade <- renderPlot({
      movies |> filter(
        decade >= input$range_decades[1], 
        decade <= input$range_decades[2]
      ) |> 
      ggplot(aes(decade, mean_rating_decade)) + 
        geom_point() + geom_line() +
        xlab("decade of movie release") +
        ylab("mean rating per decade") + 
        scale_x_continuous(breaks = seq(1890, 2010, 10)) + 
        scale_y_continuous(breaks = seq(4, 7, 0.25))
    })

### Part 6: Plot of the ratings per genre 
  output$plot_genre <- renderPlot({
    movies |> filter(
      decade >= input$range_decades[1], 
      decade <= input$range_decades[2]
    ) |> 
      ggplot(aes(decade, mean_rating_decade)) + 
      geom_point() + geom_line() +
      xlab("decade of movie release") +
      ylab("mean rating per decade") + 
      scale_x_continuous(breaks = seq(1890, 2010, 10)) + 
      scale_y_continuous(breaks = seq(4, 7, 0.25))
  })
  
### Part 7: boxplot with votes
  output$plot_votes <- renderPlot({
    movies |> filter(
      decade >= input$range_decades[1], 
      decade <= input$range_decades[2]
    ) |> 
      ggplot(aes(decade, mean_rating_decade)) + 
      geom_point() + geom_line() +
      xlab("decade of movie release") +
      ylab("mean rating per decade") + 
      scale_x_continuous(breaks = seq(1890, 2010, 10)) + 
      scale_y_continuous(breaks = seq(4, 7, 0.25))
  })
  
### Part 8: Linear regression output 
  output$text_regression <- renderText({
  "The linear regression of ... resulted in this ... "})
  output$table_regression <- renderTable(movies_per_decade)
}

shinyApp(ui = ui, server = server)


# can create a function for making the plot 
    # (see helpers.R from tutorial)



