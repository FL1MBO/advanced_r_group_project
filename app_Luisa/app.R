library(shiny)
library(bslib)
library(ggplot2)
library(DT)

clean_movies <- readRDS("~/Advanced_R/advanced_r_group_project/app_Luisa/data/clean_movies.rds")
source("~/Advanced_R/advanced_r_group_project/app_Luisa/helpers.R")

ui <- page_sidebar(
  title = "Movie data - votes",
  sidebar = sidebar(
    helpText(
      "Create a scatter plot of movie ratings according to number of votes by users"
    ),
    selectInput(
      inputId = "rating_percent",
      label = "Color by percentage of:",
      choices = c(
        "1/10 reviews"  = "Percentage of 1/10 reviews",
        "2/10 reviews"  = "Percentage of 2/10 reviews",
        "3/10 reviews"  = "Percentage of 3/10 reviews",
        "4/10 reviews"  = "Percentage of 4/10 reviews",
        "5/10 reviews"  = "Percentage of 5/10 reviews",
        "6/10 reviews"  = "Percentage of 6/10 reviews",
        "7/10 reviews"  = "Percentage of 7/10 reviews",
        "8/10 reviews"  = "Percentage of 8/10 reviews",
        "9/10 reviews"  = "Percentage of 9/10 reviews",
        "10/10 reviews" = "Percentage of 10/10 reviews"
      )
    ),
    sliderInput(
      "votes",
      label = "Range of interest:",
      min = min(clean_movies$votes), max = max(clean_movies$votes), 
      value = c(min(clean_movies$votes), max(clean_movies$votes))
    )
  ),
  textOutput("min_max"),
  
  navset_card_underline(
    nav_panel("Plot", plotOutput("plot_votes")),
    nav_panel("Table", DT::dataTableOutput("table_votes"))
  )
)

server <- function(input, output) {
  
  output$min_max <- renderText({
    paste("You have chosen a range that goes from",
          input$votes[1], "to", input$votes[2])
  })
  
  output$plot_votes <- renderPlot({
    args <- switch(input$rating_percent,
                   "Percentage of 1/10 reviews" = list(rating_percent = quote(r1)),
                   "Percentage of 2/10 reviews" = list(rating_percent = quote(r2)),
                   "Percentage of 3/10 reviews" = list(rating_percent = quote(r3)),
                   "Percentage of 4/10 reviews" = list(rating_percent = quote(r4)),
                   "Percentage of 5/10 reviews" = list(rating_percent = quote(r5)),
                   "Percentage of 6/10 reviews" = list(rating_percent = quote(r6)),
                   "Percentage of 7/10 reviews" = list(rating_percent = quote(r7)),
                   "Percentage of 8/10 reviews" = list(rating_percent = quote(r8)),
                   "Percentage of 9/10 reviews" = list(rating_percent = quote(r9)),
                   "Percentage of 10/10 reviews" = list(rating_percent = quote(r10))
    )
    
    args$min <- input$votes[1]
    args$max <- input$votes[2]
    
    do.call(plot_rating_against_votes, args)
  })
  
  output$table_votes <- DT::renderDataTable({
    req(input$votes)
    
    table_votes(data = clean_movies,
                min = input$votes[1],
                max = input$votes[2], 
                cols = c(1, 4:15)
    )}, 
    options = list(packageLength = 20, scrollX = TRUE))
  
}

shinyApp(ui, server)
