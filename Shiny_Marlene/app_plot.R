
# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tools)
library(ggplot2movies)

# Load data --------------------------------------------------------------------

min_votes <- min(movies$votes)
max_votes <- max(movies$votes)

clean_movies <- movies |>
  select(-budget, -mpaa) |>
  mutate(genre = factor(case_when(rowSums(pick(Action:Romance)) > 1 ~ "Crosslisted",
                                  Action == 1 ~ "Action",
                                  Animation == 1 ~ "Animation",
                                  Comedy == 1 ~ "Comedy",
                                  Drama == 1 ~ "Drama",
                                  Documentary == 1 ~ "Documentary",
                                  Romance == 1 ~ "Romance",
                                  rowSums(pick(Action:Romance)) < 1 ~ "No genre")))

# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  title = "Movie browser",
  sidebar = sidebar(
    selectInput(
      inputId = "y",
      label = "Y-axis:",
      choices = c(
        "Number of votes" = "votes",
        "Rating" = "rating",
        "Length" = "length",
        "Year" = "year",
        "Genre" = "genre"
      ),
      selected = "rating"
    ),
    
    selectInput(
      inputId = "x",
      label = "X-axis:",
      choices = c(
        "Number of votes" = "votes",
        "Rating" = "rating",
        "Length" = "length",
        "Year" = "year",
        "Genre" = "genre"
      ),
      selected = "year"
    ),
    
    selectInput(
      inputId = "z",
      label = "Color by:",
      choices = c(
        "Number of votes" = "votes",
        "Rating" = "rating",
        "Length" = "length",
        "Year" = "year",
        "Genre" = "genre"
      ),
      selected = "genre"
    ),
    
    checkboxGroupInput(
      inputId = "genre",
      label = "Choose a genre",
      choices = list("Crosslisted" = "Crosslisted", "No genre" = "No genre", 
                     "Action" = "Action", "Animation" = "Animation", 
                     "Comedy" = "Comedy", "Drama" = "Drama", 
                     "Documentary" = "Documentary", "Romance" = "Romance"),
      selected = genres),
    
    h4("Temporal plot filters"),
    
    # Slider input for the range of years ----
    sliderInput(
      "range_year",
      "Set the range of year of movie release:",
      min = 1893,
      max = 2005,
      value = c(1893, 2005), step = 1, sep = "" ),
    
    # Slider input for filtering out low votes ----
    sliderInput(
      "votes",
      "Set the range of votes per movie considered",
      min = min_votes,
      max = max_votes,
      value = c(min_votes, 50000), step = 10, sep = "" )),
  
  card(
    plotOutput(outputId = "scatterplot"),
    textOutput(outputId = "description")
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    clean_movies |> 
      filter(genre %in% input$genre, 
             between(year, input$range_year[1], input$range_year[2]),
             between(votes, input$votes[1], input$votes[2])) |> 
      ggplot(aes(x = .data[[input$x]], y = .data [[input$y]], color = .data[[input$z]])) +
      geom_point() +
      geom_smooth(aes(color = .data[[input$z]])) +
      theme_classic()#+
     # geom_line(avg(clean_movies$year))
  })
  
  output$description <- renderText({
    paste0("The plot above, visualizes the relationship between ", input$x, " and ",
           input$y, ", conditional on ", input$z, ".")
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
