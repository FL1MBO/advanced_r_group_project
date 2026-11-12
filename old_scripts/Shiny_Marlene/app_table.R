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

min_date <- min(movies$year)
max_date <- max(movies$year)

genres <- c("No genre", "Crosslisted", "Action", "Animation", "Comedy", "Drama", "Documentary", "Romance")

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

# Define UI for application that draws a table
ui <- page_sidebar(
  sidebar = sidebar(checkboxGroupInput(
    inputId = "genre",
    label = "Choose a genre",
    choices = list("Crosslisted" = "Crosslisted", "No genre" = "No genre", 
                   "Action" = "Action", "Animation" = "Animation", 
                   "Comedy" = "Comedy", "Drama" = "Drama", 
                   "Documentary" = "Documentary", "Romance" = "Romance"),
    selected = genres)),
  
  gt_output("table")
)

# Define server logic required to draw a table
server <- function(input, output) {
  
  output$table <- render_gt(
    {
      clean_movies |>
        select(title, year, rating, votes, genre, length)|>
        filter(genre == input$genre)|>
        arrange(desc(rating)) |>
        slice_head(n = 10) |>
        gt() |>
        tab_header(title = "Top 10 movies on IMDB by your selection") |>
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

