library(ggplot2movies)

movies <- as.tibble(ggplot2movies::movies) %>% 
  na.omit() %>% 
  