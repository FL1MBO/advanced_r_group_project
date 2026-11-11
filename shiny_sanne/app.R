library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2movies)

movies <- ggplot2movies::movies

clean_movies <- movies |>   
  map(~str_trim(.x)) |>   
  as_tibble() |> 
  select(-budget, -mpaa) |> 
  mutate_at(vars(-title), as.numeric) |>  
  mutate( as_date("%Y")) |> 
  filter(votes >= cutoff_votes) 

clean_movies <- clean_movies |> 
  group_by(year) |> 
  mutate(mean_rating = mean(rating), mean_votes = mean(votes)) 


ui <- page_sidebar(
  title = "Temporal analysis movie data",
  sidebar = sidebar("Movie data is presented per year"),
  card(
    card_header("Overview of the data"),
    tableOutput("table")
  ), 
  card(
    card_header("Plot of the average rating per year"),
    plotOutput("plot"),
    sliderInput(
      "slider1",
      "Set value range",
      min = 1893,
      max = 2005,
      value = c(1893, 2005)
    )
  )
)
  

server <- function(input, output){
 output$table <- renderTable(overview_decades, striped = True)
  output$plot <- renderPlot(
    {
      ggplot(data = clean_movies, aes(aes(year)) + 
               geom_bar(fill = "#69b3a2", colour = "#000000") +
               xlab("year of movie release") +
               ylab("number of movies"))
    }
  )
}

shinyApp(ui = ui, server = server)


# can create a function for making the plot 
    # (see helpers.R from tutorial)