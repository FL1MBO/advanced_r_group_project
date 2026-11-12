
table_votes <- function(data, min, max, cols = c(1, 4:15)) {
  filtered_data <- data[data$votes >= min & data$votes <= max, ]
  filtered_data[, cols, drop = FALSE]
}