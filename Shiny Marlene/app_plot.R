
# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tools)
library(ggplot2movies)

# Load data --------------------------------------------------------------------

movies <- movies

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
