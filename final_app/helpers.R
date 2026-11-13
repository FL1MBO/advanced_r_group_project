# Data table function
table_votes <- function(data, min_votes, max_votes, year_range, cols = c(1:15, 23)) {
  filtered_data <- data[data$votes >= min_votes & data$votes <= max_votes, ]
  filtered_data <- data[data$year == year_range, ]
  filtered_data[, cols, drop = FALSE]
}
