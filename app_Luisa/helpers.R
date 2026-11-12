plot_rating_against_votes <- function(rating_percent, min, max) {
  
  clean_movies |> 
    subset(votes >= min & votes <= max) |> 
    ggplot( 
      aes(votes, rating)) +
    geom_point(aes(color = {{rating_percent}})) +
    labs( x = "Total user votes",
          y = "Average rating /10") +
    stat_smooth(method = "lm", 
                formula = y ~ 1 + x, 
                se = FALSE, 
                color = "orange3", 
                linetype = 1) +
    stat_smooth(method = "lm", 
                formula = y ~ 1 + log(x), 
                se = FALSE, 
                color = "red3", 
                linetype = 1) + 
    scale_color_viridis_c()
  }


table_votes <- function(data, min, max, cols = c(1, 4:15)) {
  filtered_data <- data[data$votes >= min & data$votes <= max, ]
  filtered_data[, cols, drop = FALSE]
}