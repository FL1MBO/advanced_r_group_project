library(ggplot2movies)

movies <- as.tibble(ggplot2movies::movies) 

# correlation between the budget of a movie and its average score

library(DataExplorer)
t(introduce(movies))

# Distribution per year
ggplot(movies, aes(x = length)) +
  geom_density(lwd = 1.2, linetype = 1)



# Graphs with the distributions of the variables that might be interesting
# (we could also do the r1, r2,...)

movies %>% 
  pivot_longer(
  cols = 2:6,   
  names_to = "variable",      
  values_to = "value"         
) %>% 
  ggplot(aes(x = value, fill = variable, colour = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(
    ~ variable, 
    scales = "free",
  labeller = as_labeller(c(
    "year" = "Year",
    "length"= "Length",
    "budget"= "Budget",
    "rating"= "Rating",
    "votes"= "Number of votes"))) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = NULL, y = "Density") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none")


# Get a summary table for the whole dataset with interesting values

movies %>% 
  select(2:16) %>% 
  pivot_longer(everything(), 
               names_to = "variable", 
               values_to ="value") %>%
  group_by(variable) %>% 
  summarise(
    Mean = mean(value, na.rm = TRUE),
    Minimum = min(value, na.rm = TRUE),
    Maximum = max(value, na.rm = TRUE),
    Missing = sum(is.na(value))) %>% 
  gt() %>% 
  fmt_number(columns = c(Mean, Minimum, Maximum, Missing),
             decimals = 2) %>%
  tab_header(
    title = "Movied - Summary statistics"
  )
  
