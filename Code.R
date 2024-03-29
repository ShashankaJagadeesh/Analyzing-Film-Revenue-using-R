install.packages("readr")
install.packages("dplyr")

library(readr)
library(dplyr)

disney_plus_titles <- read.csv("C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/netflix_titles.csv", fileEncoding = "UTF-8")
netflix_titles <- read.csv("C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/disney_plus_titles.csv", fileEncoding = "UTF-8")
imdb_movies <- read.csv("C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/imdb_movies.csv", fileEncoding = "UTF-8")


library(dplyr)

colnames(imdb_movies)
colnames(netflix_titles)
colnames(disney_plus_movies)

disney_plus_movies <- filter(disney_plus_titles, type == "Movie")
netflix_movies <- filter(netflix_titles, type == "Movie")

head(netflix_movies,15)

library(dplyr)

# Assuming disney_plus_movies and netflix_movies are already filtered to contain only movies

# Add a new column to each dataset to indicate the source
disney_plus_movies$source <- 'Disney+'
netflix_movies$source <- 'Netflix'

# Combine the two datasets
combined_movies <- bind_rows(disney_plus_movies, netflix_movies)

head(combined_movies, 20)
# Remove duplicate movie titles, keeping the first occurrence
combined_movies_unique <- combined_movies %>%
  distinct(title, .keep_all = TRUE)

# Display the first 15 rows of the combined, unique movies dataset
head(combined_movies_unique, 30)

imdb_movies <- rename(imdb_movies, title = names)

final_dataset <- merge(combined_movies_unique, imdb_movies, by = "title", all.x = TRUE)

head(final_dataset, 15)

final_dataset <- na.omit(final_dataset)

final_dataset$example_column[is.na(final_dataset$example_column)] <- median(final_dataset$example_column, na.rm = TRUE)

final_dataset <- unique(final_dataset)

write.csv(final_dataset, "C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/final_dataset1.csv", row.names = FALSE)

final_dataset <- select(final_dataset, -orig_title)
final_dataset$title <- sapply(final_dataset$title, function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = ""))




