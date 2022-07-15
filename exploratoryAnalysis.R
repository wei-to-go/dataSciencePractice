#practice subsetting data
#use a combination of filter, select, mutate, arrange, summarise, group_by, sample, and/or slice
#create a visualization using ur new subset of data

library(ggplot2)
library(dplyr)
show_data <- read.csv("data/tv_shows.csv")
View(show_data)

head(show_data)

cleanData <- select(show_data, Title:Rotten.Tomatoes) %>%
                filter(Age != "")

ggplot(data = cleanData, aes(x = factor(Age, c("all", "7+","13+","16+","18+")), fill = Age)) +
  geom_bar() +
  labs(x = "age ratings",
       y = "# of tv shows",
       title = "age ratings of popular tv shows",
       fill = "age ratings")

