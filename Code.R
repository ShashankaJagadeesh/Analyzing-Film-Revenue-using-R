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
colnames(disney_plus_titles)

disney_plus_movies <- filter(disney_plus_titles, type == "Movie")
netflix_movies <- filter(netflix_titles, type == "Movie")

head(netflix_movies,15)

library(dplyr)


#new column to each dataset to indicate the source
disney_plus_movies$source <- 'Disney+'
netflix_movies$source <- 'Netflix'

# Combining the two datasets
combined_movies <- bind_rows(disney_plus_movies, netflix_movies)

head(combined_movies, 20)
# Removing duplicate movie titles, keeping the first occurrence
combined_movies_unique <- combined_movies %>%
  distinct(title, .keep_all = TRUE)

head(combined_movies_unique, 30)


final_dataset <- merge(combined_movies_unique, imdb_movies, by = "title", all.x = TRUE)

head(final_dataset, 15)

final_dataset <- na.omit(final_dataset)

final_dataset$example_column[is.na(final_dataset$example_column)] <- median(final_dataset$example_column, na.rm = TRUE)

final_dataset <- unique(final_dataset)

#write.csv(final_dataset, "C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/final_dataset1.csv", row.names = FALSE)

final_dataset <- select(final_dataset, -orig_title)
#final_dataset <- select(final_dataset, -overview)
final_dataset$title <- sapply(final_dataset$title, function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = ""))
final_dataset$genre <- sapply(final_dataset$genre, function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = ""))

library(tidyverse)

final_dataset$budget_x[is.na(final_dataset$budget_x)] <- median(final_dataset$budget_x, na.rm = TRUE)
final_dataset$revenue[is.na(final_dataset$revenue)] <- median(final_dataset$revenue, na.rm = TRUE)


#----------GLM regression model-----------------

summary(glm_model_simplified)

glm_model_no_genre <- glm(revenue ~ budget_x + release_year + score, 
                          data = final_dataset, 
                          family = gaussian())

summary(glm_model_no_genre)

#---------------GAM regression model------------





#-------- improving the model------

# Fit a GAM with an interaction term between budget_x and score
gam_model_interaction <- gam(revenue ~ s(budget_x) + s(release_year) + s(score) + ti(budget_x, score),
                             data = final_dataset, family = gaussian())

summary(gam_model_interaction)

#-------------2nd iteration-------------------------


library(mgcv)
gam_model_enhanced <- gam(revenue ~ s(log(budget_x)) + s(release_year) + s(score) + ti(log(budget_x), score),
                          data = final_dataset, family = gaussian(),
                          method = "REML")  # REML for smoothing parameter estimation

summary(gam_model_enhanced)

#Visualizing the effects of the predictors
par(mfrow=c(3,1))  # Arrange plots in 3 rows and 1 column
plot(gam_model_enhanced, pages=1)  # Plot the effects


#--------------qualitative analysis----------

if (!require("tm")) install.packages("tm")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

library(tm)
library(wordcloud)
library(RColorBrewer)
library(tm)

text_corpus <- Corpus(VectorSource(final_dataset$overview))

inspect(text_corpus[1:10])

# Convert to lowercase
text_corpus <- tm_map(text_corpus, content_transformer(tolower))

inspect(text_corpus[1:10])


custom_stopwords <- stopwords("english")
custom_stopwords <- c(custom_stopwords, "his", "her", "some", "other", "additional", "words")
print(custom_stopwords)


# Create a text corpus from the overview column
text_corpus <- Corpus(VectorSource(final_dataset$overview))

text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, custom_stopwords)  # Use custom list here
text_corpus <- tm_map(text_corpus, stripWhitespace)

tdm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

print(word_freqs_df)

# top 20 most frequent words for the word cloud
top_20_words <- head(word_freqs_df, 20)

# wordcloud
wordcloud(words = top_20_words$word, freq = top_20_words$freq,
          max.words = 20, 
          scale = c(1.6, 0.2),  
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))


#------------word association-----------------------------------

if (!require("tm", quietly = TRUE)) install.packages("tm")
library(tm)


text_corpus <- Corpus(VectorSource(final_dataset$overview))


text_corpus <- tm_map(text_corpus, content_transformer(tolower)) # Convert to lowercase
text_corpus <- tm_map(text_corpus, removePunctuation)            # Remove punctuation
text_corpus <- tm_map(text_corpus, removeNumbers)                # Remove numbers
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english")) # Remove stopwords
text_corpus <- tm_map(text_corpus, stripWhitespace)              # Remove extra white spaces


tdm <- TermDocumentMatrix(text_corpus)

inspect(tdm[1:5, 1:5]) # Inspect first 5 terms and documents

associations <- findAssocs(tdm, "life", 0.25) 

print(associations)

#-----------------Visualisation-------------------------------------------

install.packages("plotly")

library(plotly)

# Data
terms <- c("laura", "acclaimed", "thirtyyearold", "unapologetic", "void",
           "iterations", "laura’s", "andrea", "exploring", "desires",
           "fredricksen", "persistent", "deepest", "horribly", "realities",
           "everyday", "womanizer", "lease", "fullest", "longs", "many")

values <- c(0.28, 0.27, 0.27, 0.27, 0.27, 0.27, 0.26, 0.25, 0.23, 0.22,
            0.21, 0.21, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.17, 0.16)

# bar chart
fig <- plot_ly(x = ~terms, y = ~values, type = 'bar', marker = list(color = 'indianred'))
fig <- fig %>% layout(title = 'Term Frequencies in $life Theme',
                      xaxis = list(title = 'Terms', tickangle = -45),
                      yaxis = list(title = 'Frequency'),
                      template = 'plotly_white')

fig


#------------------ Geographical---------------

if (!require("leaflet")) install.packages("leaflet")
if (!require("dplyr")) install.packages("dplyr")
if (!require("countrycode")) install.packages("countrycode")

library(leaflet)
library(dplyr)
library(countrycode)
library(readr)

if (!require("countrycode", quietly = TRUE)) install.packages("countrycode")
library(countrycode)

# Convert ISO country codes to country names
final_dataset$country_name <- countrycode(final_dataset$country.y, "iso2c", "country.name")

# Check the first few rows to ensure the conversion worked
head(final_dataset)

if (!require("maps", quietly = TRUE)) install.packages("maps")
if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2")

library(maps)
library(ggplot2)

# world map data, which includes lat/lon for country centers
world_map <- map_data("world")

# unique list of countries with their mean latitude and longitude
country_coords <- world_map %>%
  group_by(region) %>%
  summarize(lat = mean(lat), lon = mean(long), .groups = 'drop')


# Merge the coordinates
final_dataset <- merge(final_dataset, country_coords, by.x = "country_name", by.y = "region", all.x = TRUE)

head(final_dataset)

na_count_country_name <- sum(is.na(final_dataset$country_name))
cat("Number of NA values in country_name column:", na_count_country_name, "\n")
na_count_country_code <- sum(is.na(final_dataset$country.y))
cat("Number of NA values in country.y column:", na_count_country_code, "\n")

invalid_rows <- which(is.na(final_dataset$lat) | is.na(final_dataset$lon) |
                        !is.numeric(final_dataset$lat) | !is.numeric(final_dataset$lon))

length(invalid_rows)

final_dataset <- final_dataset %>%
  mutate(
    lat = ifelse(country.y == "US", 37.0902, lat),
    lon = ifelse(country.y == "US", -95.7129, lon),
    lat = ifelse(country.y == "GB", 55.3781, lat),
    lon = ifelse(country.y == "GB", -3.4360, lon),
    lat = ifelse(country.y == "HK", 22.3193, lat),
    lon = ifelse(country.y == "HK", 114.1694, lon)
  )

leaflet(data = final_dataset) %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, popup = ~country_name) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # 
  setView(lng = mean(final_dataset$lon, na.rm = TRUE), 
          lat = mean(final_dataset$lat, na.rm = TRUE), 
          zoom = 2) 


#Cluster Markers
leaflet(data = final_dataset) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~country_name, 
                   clusterOptions = markerClusterOptions()) %>%
  setView(lng = mean(final_dataset$lon, na.rm = TRUE), 
          lat = mean(final_dataset$lat, na.rm = TRUE), 
          zoom = 2)

if (!require("leaflet")) install.packages("leaflet")
library(leaflet)


#Heatmap
install.packages("leaflet.extras")
library("leaflet.extras")
leaflet(data = final_dataset) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng = ~lon, lat = ~lat, intensity = ~1, radius = 20, blur = 15) %>%
  setView(lng = mean(final_dataset$lon, na.rm = TRUE), 
          lat = mean(final_dataset$lat, na.rm = TRUE), 
          zoom = 2)

#----------------insights---------------------------

library(plotly)
library(dplyr)

# Sum revenue by country
revenue_by_country <- final_dataset %>%
  group_by(country.y) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE))

# interactive bar chart
plot_ly(data = revenue_by_country, x = ~country.y, y = ~total_revenue, type = 'bar') %>%
  layout(title = 'Total Revenue by Country',
         xaxis = list(title = 'Country'),
         yaxis = list(title = 'Total Revenue'))

library(tidyr)
genre_counts <- final_dataset %>%
  separate_rows(genre, sep = ",") %>%
  count(genre, sort = TRUE)

#  interactive pie chart
plot_ly(data = genre_counts, labels = ~genre, values = ~n, type = 'pie') %>%
  layout(title = 'Distribution of Shows by Genre')

# avg score by lang
avg_score_by_lang <- final_dataset %>%
  group_by(orig_lang) %>%
  summarise(average_score = mean(score, na.rm = TRUE))

# plot
plot_ly(data = avg_score_by_lang, x = ~orig_lang, y = ~average_score, type = 'scatter', mode = 'markers+lines') %>%
  layout(title = 'Average Score by Original Language',
         xaxis = list(title = 'Original Language'),
         yaxis = list(title = 'Average Score'))


#scatter plot
plot_ly(data = final_dataset, x = ~budget_x, y = ~revenue, type = 'scatter', mode = 'markers') %>%
  layout(title = 'Budget vs. Revenue',
         xaxis = list(title = 'Budget'),
         yaxis = list(title = 'Revenue'))




