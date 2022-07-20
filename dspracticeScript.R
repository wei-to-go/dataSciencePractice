#### load in required libraries ####
library(ggplot2)
library(dplyr)

#### Load in csv file ####

show_data <- read.csv("tv_shows.csv")

#### save r object as a file ####
saveRDS(show_data, "data/show_data.RDS")

#### practice plotting ####

show_data$Title[1:50]

ggplot(data = show_data, aes(x = Year)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(breaks = seq(from = 1920, to = 2040, by = 10)) +
  scale_y_continuous(breaks = seq(from = 0, to = 3000, by = 100))

ggplot(data = show_data, aes(x = Age, fill = Age)) +
  geom_bar() +
  scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 100)) +
  labs(y = "# of shows",
       x = "age rating",
       fill = "age rating",
       title = "age ratings of popular tv shows")



#unsupervised machine learning
head(show_data)

subbedTomatoes <- sub("/100","", show_data$Rotten.Tomatoes)

subbedIMDb <- sub("/10","", show_data$IMDb)

numberData <- mutate(show_data, 
                     numericRating = ((as.numeric(subbedTomatoes)) + (as.numeric(subbedIMDb) * 10)) / 2)

numberData <- filter(numberData, Age != "")

show_numerics <- select(numberData, Title, Year, Age, numericRating) %>%
  scale()


show_clusters <- kmeans(show_numerics, center = 3)
show_clusters #our results

show$cluster <- show_clusters$cluster #add cluster column to original dataset
head(show_data)

ggplot(show_numerics, aes(x = Year, y = numericRating)) +
  geom_point(aes(color = Age))


