library(tidyverse)
library(ggplot2movies)

movies <- readRDS("data/clean_movies.rds")
movies_per_decade <- readRDS("data/movies_per_decade.rds")

ggplot(movies, aes(year, mean_rating)) + 
         geom_line() +
         xlab("year of movie release") +
         ylab("number of movies")


ggplot(movies, aes(year, mean_rating)) + 
  geom_line() +
  xlab("year of movie release") +
  ylab("mean rating") + scale_x_continuous(breaks = seq(1890, 2010, 10))


ggplot(movies, aes(decade, mean_rating_decade)) + 
  geom_line() +
  xlab("decade of movie release") +
  ylab("mean rating per decade") + scale_x_continuous(breaks = seq(1890, 2010, 10))

ggplot(movies, aes(decade, mean_rating_decade)) + 
  geom_point() + geom_line() +
  xlab("decade of movie release") +
  ylab("mean rating per decade") + 
#  xlim(1890, 2010) + ylim(4, 7) +
  scale_x_continuous(breaks = seq(1890, 2010, 10)) + 
  scale_y_continuous(breaks = seq(4, 7, 0.25))
