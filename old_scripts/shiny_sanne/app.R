library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2movies)

movies <- readRDS("data/clean_movies.rds")
movies_per_decade <- readRDS("data/movies_per_decade.rds")

ui <- page_sidebar(
  title = "Analysis movie data",
  theme = bs_theme(preset = "cerulean"),
  sidebar = sidebar(h4("Temporal plot filters"),
                    
                    # Slider input for the range of years ----
                    sliderInput(
                      "range_years",
                      "Set the range of year of movie release:",
                      min = 1893,
                      max = 2005,
                      value = c(1893, 2005), step = 1, sep = "" ),
                    
                    # Slider input for the range of decades ----
                    sliderInput(
                      "range_decades",
                      "Set the range of decade of movie release:",
                      min = 1890,
                      max = 2000,
                      value = c(1890, 2000), step = 10, sep = ""),
                    
                    # Check boxes for scaling on votes ----
                    checkboxInput(
                      "votes",
                      "Scale ratings based on number of votes",
                      value = FALSE),
                    
                    # Check boxes for scaling on number of movies ----
                    checkboxInput(
                      "number_movies",
                      "Scale ratings based on number of movies released",
                      value = FALSE),
                    
                    # Multiple check boxes for the genres ----
                    checkboxGroupInput(
                      "genre",
                      label = "Choose a genre (a movie can have multiple genres)",
                      choices = list("Action" = 1, "Animation" = 2, "Comedy" = 3, "Drama" = 4, "Documentary" = 5, "Romance" = 6),
                      selected = c(1, 2, 3, 4, 5, 6)
                    )
  ),
  navset_card_underline(title = "Analysis of movie data",
    
    # Panel with summary text ----
    nav_panel("Summary", verbatimTextOutput("info_data"), "Table", tableOutput("table")),
    
    # Panel with table ----
    nav_panel("Table of top 10 movies on IMDb", tableOutput("table_10")),
    
    # Panel with plot per year ----
    nav_panel("Plot yearly average rating", plotOutput("plot_year")),
    
    # Panel with plot per decades ----
    nav_panel("Plot average rating over decades", plotOutput("plot_decade")),
    
    # Panel with plot per genre ---
    nav_panel("Plot rating genres over time", plotOutput("plot_genre")),
    
    # Panel with plot with votes over decades ---
    nav_panel("Plot rating votes over decades", plotOutput("plot_votes")),
    
    # Panel with regression information ---
    nav_panel("Regression", verbatimTextOutput("text_regression"),
              "Table regression", tableOutput("table_regression"),
              "Plot regression", plotOutput("plot_regression")
              ),
    
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
  
  output$plot_regression <- renderPlot({})

}

shinyApp(ui = ui, server = server)


# can create a function for making the plot 
    # (see helpers.R from tutorial)



