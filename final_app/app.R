library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2movies)
library(gt)
library(DT)


movies <- readRDS("data/clean_movies.rds")
movies_per_decade <- readRDS("data/movies_per_decade.rds")
genres <- c("No genre", "Crosslisted", "Action", "Animation", "Comedy", "Drama", "Documentary", "Romance")

source("helpers.R")

ui <- page_sidebar(
  title = "Analysis movie data",
  theme = bs_theme(preset = "cerulean"),
  sidebar = sidebar(h4("Filters"),
                    
                    # Multiple check boxes for the genres ----
                    checkboxGroupInput(inputId = "genre",
                                       label = "Choose a genre",
                                       choices = list(
                                         "Crosslisted" = "Crosslisted",
                                         "No genre" = "No genre", 
                                         "Action" = "Action",
                                         "Animation" = "Animation", 
                                         "Comedy" = "Comedy",
                                         "Drama" = "Drama", 
                                         "Documentary" = "Documentary",
                                         "Romance" = "Romance"),
                                       selected = genres),
                    
                    # Slider input for the range of votes ---
                    sliderInput(
                      "votes",
                      label = "Range of interest:",
                      min = min(clean_movies$votes), max = max(clean_movies$votes), 
                      value = c(min(clean_movies$votes), max(clean_movies$votes))),
                    
                    # Slider input for the range of years ----
                    sliderInput(inputId ="range_years",
                                label = "Set the range of year of movie release:",
                                min = 1893,
                                max = 2005,
                                value = c(1893, 2005),
                                step = 1,
                                sep = "" ),
              
                    # Selection for linear model 
                    helpText("Fit a linear model for rating based on:"),
                    checkboxGroupInput("predictor", "Select predictor(s):",
                                       choices = c("Year" = "year", 
                                                   "Length" = "length",
                                                   "Votes" = "votes",
                                                   "Percentage of 1/10 votes" = "r1",
                                                   "Percentage of 2/10 votes" = "r2",
                                                   "Percentage of 3/10 votes" = "r3",
                                                   "Percentage of 4/10 votes" = "r4",
                                                   "Percentage of 5/10 votes" = "r5",
                                                   "Percentage of 6/10 votes" = "r6",
                                                   "Percentage of 7/10 votes" = "r7",
                                                   "Percentage of 8/10 votes" = "r8",
                                                   "Percentage of 9/10 votes" = "r9",
                                                   "Percentage of 10/10 votes" = "r10"),
                                       selected = "year")),
  navset_card_underline(title = "Analysis of movie data",
                        
                        # Panel with summary text ----
                        nav_panel("Summary", verbatimTextOutput("info_data")),
                        nav_panel("Data table ", DT::dataTableOutput("table_votes")),
                        
                        # Panel with table ----
                        nav_panel("Table of top 10 movies on IMDb", gt_output("table_10")),
                        
                        # Panel with plot per year 
                        nav_panel("Plot yearly average rating", plotOutput("plot_year")),
                        
                        # Panel with regression information ---
                        nav_panel("Regression", verbatimTextOutput("summary_regression")),
                        nav_panel("Plot regression", plotOutput("diagnostic_plots")),
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
The data is can be checked in table in the next tab"
  })
  
  ## Part 2: table with the number of movies per decade
  output$table_votes <- DT::renderDataTable({
    req(input$votes)
    
    table_votes(data = clean_movies,
                min = input$votes[1],
                max = input$votes[2], 
                cols = c(1, 4:15)
    )}, 
    options = list(packageLength = 20, scrollX = TRUE))
  
  ## Part 3: add Luisa's table  
  output$table_10 <- render_gt(
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
  model <- reactive({
    req(input$predictor) 
    all_predictors <- paste(input$predictor, collapse = " + ")
    formula_regression <- paste("rating ~", all_predictors)
    lm(as.formula(formula_regression), data = clean_movies)
  })
  output$summary_regression <- renderPrint({
    summary(model())
  })
  
  output$diagnostic_plots <- renderPlot({
    req(model())
    fit <- model()
    par(mfrow = c(2, 2))
    plot(fit, which = c(1:3, 5))
  })

}

shinyApp(ui = ui, server = server)


# can create a function for making the plot 
# (see helpers.R from tutorial)



