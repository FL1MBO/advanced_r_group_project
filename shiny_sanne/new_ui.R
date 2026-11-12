

ui <- fluidPage(
  # Create overal nagivation panes
  navbarPage("Movie rating analysis", theme = theme("lumen"),
             # Tab for the temporal analysis
             tabPanel("Overview of data", fluid = TRUE,
                      mainPanel(
                        h3(verbatimTextOutput("info_data")),
                        tableOutput("table_1"))),
             tabPanel("Temporal analysis", fluid = TRUE,
                      # Sidebar with specific interactive elements for the temporal plots
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Temporal plot filters",
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
                      )),
             # Second tab for the other plot
             tabPanel("Voting analysis", fluid = TRUE,
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
                                                      value = c(min(movies$votes,max(movies$votes))))))),
                        mainPanel(navset_card_underline(
                          title = "Analysis",
                          nav_panel("Plot rating decades", plotOutput("plot_votes")),
                          nav_panel("Regression", verbatimTextOutput("text_regression"), "Table regression", tableOutput("table_regression")),
                        ))
                      )
             )
  )
)
