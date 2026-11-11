library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2movies)

movies <- readRDS("data/clean_movies.rds")
movies_per_decade <- readRDS("data/movies_per_decade.rds")


ui <- page_sidebar(
  title = "Temporal analysis movie data",
  sidebar = sidebar(helpText("Movie data is presented per year"),
  sliderInput(
    "range_years",
    "Set the range of year of movie release:",
    min = 1893,
    max = 2005,
    value = c(1893, 2005), step = 1, sep = "" ),
  sliderInput(
    "range_decades",
    "Set the range of decade of movie release:",
    min = 1890,
    max = 2000,
    value = c(1890, 2000), step = 10, sep = ""),
  checkboxInput(
    "votes",
    "Scale ratings based on number of votes",
    value = FALSE
  ),
  checkboxInput(
    "number_movies",
    "Scale ratings based on number of movies released",
    value = FALSE
  )),
  navset_card_underline(
    title = "Visualizations",
    
    # Panel with summary ----
    nav_panel("Summary", verbatimTextOutput("info_data"), "Table", tableOutput("table")),
    
    # Panel with table ----
  #  nav_panel("Table", tableOutput("table")),
    
    # Panel with plot 1 ----
    nav_panel("Plot rating yearly", plotOutput("plot_year")),
    # Panel with plot 1 ----
    nav_panel("Plot rating decades", plotOutput("plot_decade")),
    
    
  ))

## If you want to use cards instead of navigation panes
#  card(
#    card_header("Overview of the data"),
#    verbatimTextOutput("info_data"),
#    tableOutput("table")
#  ),
#  card(
#    card_header("Plot of the average rating per year"),
#    plotOutput("plot_year")
#  ),
#  card(
#    card_header("Plot of the average rating per decade"),
#    plotOutput("plot_decade")
#  )
#)

  

server <- function(input, output){
  
## Part 1: information on the data
  output$info_data <- renderText({
    "This dataset contains movie ratings for many movies released between 1893 - 2005."
  })

## Part 2: table with the number of movies per decade
  output$table <- renderTable(movies_per_decade)

## Part 3: plot of the average rating per year
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
    
### Part 4: Plot of the average rating per decade 
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
}

shinyApp(ui = ui, server = server)


# can create a function for making the plot 
    # (see helpers.R from tutorial)



