#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(gt)
library(ggplot2)
library(ggplot2movies)
library(tidyverse)
library(stringr)
library(shiny)
library(bslib)

movies <- movies

min_date <- min(movies$year)
max_date <- max(movies$year)

# genre assignment with accounting for multiple assignments per movie
genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance")

clean_movies <- movies |>
  select(-budget, -mpaa) |>
  mutate(
    genre_combo = apply(
      pick(all_of(genres)), 1,
      \(x) paste(genres[as.logical(x)], collapse = " + ")
    ),
    genre = factor(ifelse(genre_combo == "", "None", genre_combo))
  )

# finding the highest and lowest rated movies on IMDB and their statistics

# Define UI for application that draws a table
ui <- page_sidebar(
  
  sidebar = sidebar(
    
    HTML(paste0("Movies released in the following date will be considered 
                 Pick a date range between ", min_date, " and ", max_date, ".")),
    
    br(), br(),
    
    dateInput(inputId = "date",
              label = "Select date:",
              value = "2013",
              min = min_decade, max = max_decade)
  ),
  
  card(
    tableOutput(outputId = "table")
  )
)

# Define server logic required to draw a table
server <- function(input, output, session) {
  
  output$table <- renderTable({
    highest_table <- clean_movies |> filter(rating == max(rating)) |>
      select(title, year, rating, votes, genre, length) |> filter(thtr_rel_date >= as.POSIXct(input$date)) |>
      gt() |>
      tab_header(title = "Highest rated movies on IMDB") |>
      cols_label(
        title = "Title",
        year  = "Year",
        rating = "Rating (1-10)",
        votes = "Votes",
        genre = "Genre",
        length = "Length (min)"
      ) |>
      fmt_number(columns = votes, decimals = 0) |>
      fmt_number(columns = rating, decimals = 1) |>
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_column_labels(everything())
      ) |>
      tab_footnote(
        footnote = "Data from ggplot2's movies dataset.",
        locations = cells_title(groups = "title")
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tools)
library(ggplot2movies)

# Load data --------------------------------------------------------------------

movies <- movies

genres <- c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance")

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
    
#    checkboxGroupInput(
#      inputId = "selected_type",
#      label = "Select genre:",
#      choices = c("Action", "Animation", "Comedy", "Drama", "Documentary", "Romance"),
#      selected = "Action"
#    )
   ),
  
  card(
    plotOutput(outputId = "scatterplot"),
    textOutput(outputId = "description")
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    ggplot(clean_movies,
           aes(x = .data[[input$x]], y = .data[[input$y]], color = .data[[input$z]])) +
      geom_point()
  })
  
  output$description <- renderText({
    paste0("The plot above, visualizes the relationship between ", input$x, " and ",
           input$y, ", conditional on ", input$z, ".")
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
