# Page sidebar version
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
                      value = FALSE),
                    checkboxInput(
                      "number_movies",
                      "Scale ratings based on number of movies released",
                      value = FALSE),
                    checkboxGroupInput(
                      "genre",
                      label = "Choose a genre (a movie can have multiple genres)",
                      choices = list("Action" = 1, "Animation" = 2, "Comedy" = 3, "Drama" = 4, "Documentary" = 5, "Romance" = 6),
                      selected = c(1, 2, 3, 4, 5, 6)
                    )
  ),
  navset_card_underline(
    title = "Visualizations",
    
    # Panel with summary ----
    nav_panel("Summary", verbatimTextOutput("info_data"), "Table", tableOutput("table")),
    
    # Panel with table ----
    #  nav_panel("Table", tableOutput("table")),
    
    # Panel with plot per year ----
    nav_panel("Plot rating yearly", plotOutput("plot_year")),
    
    # Panel with plot per decades ----
    nav_panel("Plot rating decades", plotOutput("plot_decade")),
    
    # Panel with plot per genre
    nav_panel("Plot rating genres over time", plotOutput("plot_genres")
              )
  )
)

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


# Fluid page version 
ui <- fluidPanel(
  titlePanel("Movie rating exploration"),
  fluidRow(
    column(2, wellPanel(
      h4("Temporal plot filters"),
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
        "Scale ratings based on number of votes", value = FALSE),
      checkboxInput(
        "number_movies",
        "Scale ratings based on number of movies released", value = FALSE),
      checkboxGroupInput(
        "genre",
        label = "Choose a genre (a movie can have multiple genres)",
        choices = list("Action" = 1, "Animation" = 2, "Comedy" = 3, "Drama" = 4, "Documentary" = 5, "Romance" = 6),
        selected = c(1, 2, 3, 4, 5, 6)),
      wellPanel(
        h4("filters on votes"),
        sliderInput(
          "range_votes", 
          "Minimum number of votes",
          min = min(movies$votes), 
          max = max(movies$votes), 
          value = c(min(movies$votes,max(movies$votes))))
      )
    )),
    navset_card_underline(
      title = "Analysis",
      
      # Panel with summary ----
      nav_panel("Summary", verbatimTextOutput("info_data"), "Table", tableOutput("table_decades")),
      
      # Panel with table ----
      nav_panel("Table", tableOutput("table_1")),
      
      # Panel with plot per year ----
      nav_panel("Plot rating yearly", plotOutput("plot_year")),
      
      # Panel with plot per decades ----
      nav_panel("Plot rating decades", plotOutput("plot_decade")),
      
      # Panel with plot per genre
      nav_panel("Plot rating genres over time", plotOutput("plot_genre")),
      nav_panel("Plot rating decades", plotOutput("plot_votes")),
      nav_panel("Regression", verbatimTextOutput("text_regression"), "Table regression", tableOutput("table_regression")),
      
    )
  )
)