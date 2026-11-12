library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2movies)
library(gt)
library(tools)
library(DT)


movies <- readRDS("data/clean_movies.rds")

genres <- c("No genre", "Crosslisted", "Action", "Animation", "Comedy", "Drama", "Documentary", "Romance")
min_votes <- min(movies$votes)
max_votes <- max(movies$votes)

source("helpers.R")

ui <- page_sidebar(
  title = "Analysis movie data",
  theme = bs_theme(preset = "cerulean"),
  sidebar = sidebar(h2("Filters"),
                    
                    # Slider input for the range of votes ---
                    h6("Adjusting the range of votes change the data table and the plot"),
                    sliderInput(
                      "votes",
                      label = "Range of number of votes per movie:",
                      min = min(movies$votes), max = max(movies$votes), 
                      value = c(min(movies$votes), max(movies$votes))),
                    
                    
                    # Multiple check boxes for the genres ----
                    h6("The genre boxes work with the top 10 data and the plot:"),
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
                    
                    h6("Fit a the plot based on the following options:"),
                    
                    # Select input for X-axis
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
                    # Select input for Y-axis --- 
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
                    
                    # Select input for the colours --- 
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
                    # Slider input for filtering out low votes ----
                    
                    checkboxInput(inputId = "points", "Show points", value = TRUE),
                    
                    # Check boxes for scaling on number of movies ----
                    checkboxInput(inputId = "lines", "Show lines", value = TRUE),
                    
  
                    # Slider input for the range of years ----
                    sliderInput(inputId ="range_year",
                                label = "Set the range of year of movie release:",
                                min = 1893,
                                max = 2005,
                                value = c(1893, 2005),
                                step = 1,
                                sep = "" ),
                    
                    
                    # Selection for linear model 
                    h6("Fit a linear model for rating based on:"),
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

    navset_card_underline(
                        
                        # Panel with summary text ----
                        nav_panel("Summary", verbatimTextOutput("info_data")),
                        
                        # Panel with the full data table --- 
                        nav_panel("Data table ", DT::dataTableOutput("table_votes")),
                        
                        # Panel with table of the top 10 movies ----
                        nav_panel("Table of top 10 movies on IMDb", gt_output("table_10")),
                        
                        # Panel with the interactive plot ---
                        nav_panel("Plot yearly average rating", plotOutput("scatterplot"),
                                  textOutput("description")),
                        
                        # Panel with regression summary ---
                        nav_panel("Regression", verbatimTextOutput("summary_regression"), 
                                  verbatimTextOutput("coef_regression"), 
                                  verbatimTextOutput("confidence_regression")),
                        
                        # Panel with the regression plots ---
                        nav_panel("Plot regression", plotOutput("diagnostic_plots")),

)
)

server <- function(input, output, session){
  
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

This shiny app contains the follow tabs:
  - The overall data, which reacts to the range of votes in the slider.
  - The top 10 movies, which react to the genres selected in the check boxes.
  - An interactive plot, which reacts to most buttons and sliders.
  - The summary of a linear model, with rating as the dependent variable. Use the check boxes at the bottom to select the predictors.
  - The plots of the linear model."
  })
  
  ## Part 2: table with the number of movies per decade
  output$table_votes <- DT::renderDataTable({
    req(input$votes)
    
    table_votes(data = movies,
                min = input$votes[1],
                max = input$votes[2], 
                cols = c(1, 23, 2, 5, 4, 6:15)
    )}, 
    options = list(packageLength = 20, scrollX = TRUE))
  
  ## Part 3: add Luisa's table  
  output$table_10 <- render_gt({
      movies |>
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
  
  ## Part 4: Interactive plot
  output$scatterplot <- renderPlot({
    raw_plot <- movies |>
        filter(
          genre %in% input$genre,
          between(year, input$range_year[1], input$range_year[2]),
          between(votes, input$votes[1], input$votes[2])
        ) |>
      ggplot(aes(
          x = .data[[input$x]],
          y = .data [[input$y]],
          color = .data[[input$z]]
        ))  +
        theme_classic()
      
      if (input$points) {
        raw_plot <- raw_plot + geom_point()
      }
      
      if (input$lines) {
        raw_plot <- raw_plot + geom_smooth(aes(color = .data[[input$z]]), se = FALSE)
      }
      
      return(raw_plot)
      
    })
    
    output$description <- renderText({
      paste0(
        "The plot above, visualizes the relationship between ",
        input$x,
        " and ",
        input$y,
        ", conditional on ",
        input$z,
        "."
      )
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
    lm(as.formula(formula_regression), data = movies)
  })
  
  output$summary_regression <- renderPrint({
    summary(model())
  })
  
  output$coef_regression <- renderPrint({
    coef(model())
  })
  
  output$confidence_regression <- renderPrint({
    confint(model(), level = 0.950)
  })
  
  output$diagnostic_plots <- renderPlot({
    req(model())
    fit <- model()
    par(mfrow = c(2, 2))
    plot(fit, which = c(1:3, 5))
  })
}

shinyApp(ui = ui, server = server)




