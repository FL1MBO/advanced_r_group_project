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
    "slider1",
    "Set the range of year of movie release:",
    min = 1893,
    max = 2005,
    value = c(1893, 2005), sep="" )),
  card(
    card_header("Overview of the data"),
    verbatimTextOutput("info_data"),
    tableOutput("table")
  ), 
  card(
    card_header("Plot of the average rating per year"),
    plotOutput("plot_year")
  ),
  card(
    card_header("Plot of the average rating per decade"),
    plotOutput("plot_decade")
  )
)

  

server <- function(input, output){
  output$info_data <- renderText({
    "This dataset contains movie ratings for many movies released between 1893 - 2005."
  })
  output$table <- renderTable(movies_per_decade)
  output$plot_year <- renderPlot(
    {
      ggplot(movies, aes(year, mean_rating)) + 
        geom_line() +
        xlab("year of movie release") +
        ylab("mean rating") + scale_x_continuous(breaks = seq(1890, 2010, 10))
      
    }
  )
  output$plot_decade <- renderPlot({
      ggplot(movies, aes(decade, mean_rating_decade)) + 
        geom_point() + geom_line() +
        xlab("decade of movie release") +
        ylab("mean rating per decade") + 
        scale_x_continuous(breaks = seq(1890, 2010, 10)) + 
        scale_y_continuous(breaks = seq(4, 7, 0.25))
    }
  )
}

shinyApp(ui = ui, server = server)


# can create a function for making the plot 
    # (see helpers.R from tutorial)



