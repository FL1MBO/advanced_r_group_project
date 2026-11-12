library(shiny)
library(bslib)
library(ggplot2)

clean_movies <- readRDS("~/Advanced_R/advanced_r_group_project/app_Luisa/data/clean_movies.rds")

ui <- page_sidebar(
  title = "Movie data IMDB",
  sidebar = sidebar(
    helpText(
      "Fit a linear model for rating based on:"),
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
                       selected = "year")
  ), 
  verbatimTextOutput("summary_regression"),
  plotOutput("diagnostic_plots")
  )
  

server <- function(input, output) {
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
