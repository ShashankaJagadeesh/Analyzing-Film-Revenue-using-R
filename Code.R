options(repos = c(CRAN = "https://cran.rstudio.com/"))
if (!require("tm", quietly = TRUE)) install.packages("tm", quiet = TRUE)
if (!require("wordcloud", quietly = TRUE)) install.packages("wordcloud", quiet = TRUE)
if (!require("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer", quiet = TRUE)
if (!require("leaflet", quietly = TRUE)) install.packages("leaflet", quiet = TRUE)
if (!require("countrycode", quietly = TRUE)) install.packages("countrycode", quiet = TRUE)
if (!require("mgcv", quietly = TRUE)) install.packages("mgcv", quiet = TRUE)
if (!require("maps", quietly = TRUE)) install.packages("maps", quiet = TRUE)
if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2", quiet)
install.packages(c("readr", "corrplot", "reshape2", "gbm", "randomForest"))



library(ggplot2)
library(maps)
library(randomForest)
library(caret)
library(gbm)
library(mgcv)
library(readr)
library(dplyr)
library(tidyverse)
library(mgcv)
library(corrplot)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(leaflet)
library(countrycode)
library(plotly)
library(reshape2)
library(stats)
disney_plus_titles <- read.csv("C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/netflix_titles.csv", fileEncoding = "UTF-8")
netflix_titles <- read.csv("C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/disney_plus_titles.csv", fileEncoding = "UTF-8")
imdb_movies <- read.csv("C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/imdb_movies.csv", fileEncoding = "UTF-8")


colnames(imdb_movies)
colnames(netflix_titles)
colnames(disney_plus_titles)

#----------retrieve dataset---------
disney_plus_titles <- read.csv("C:/Users/PC/Documents/GitHub/RAssignment/netflix_titles.csv", fileEncoding = "UTF-8")
netflix_titles <- read.csv("C:/Users/PC/Documents/GitHub/RAssignment/disney_plus_titles.csv", fileEncoding = "UTF-8")
imdb_movies <- read.csv("C:/Users/PC/Documents/GitHub/RAssignment/imdb_movies.csv", fileEncoding = "UTF-8")

# --------Merging the datasets----------
disney_plus_movies <- filter(disney_plus_titles, type == "Movie")
netflix_movies <- filter(netflix_titles, type == "Movie")

disney_plus_movies$source <- 'Disney+'
netflix_movies$source <- 'Netflix'

# Combining the two datasets
combined_movies <- bind_rows(disney_plus_movies, netflix_movies)

# Removing duplicate movie titles
combined_movies_unique <- combined_movies %>%
  distinct(title, .keep_all = TRUE)

final_dataset <- merge(combined_movies_unique, imdb_movies, by = "title", all.x = TRUE)

final_dataset <- na.omit(final_dataset)

final_dataset$example_column[is.na(final_dataset$example_column)] <- median(final_dataset$example_column, na.rm = TRUE)

final_dataset <- unique(final_dataset)

#write.csv(final_dataset, "C:/Users/PC/Documents/Assignments/R programming/Assignment 2/Datasets/final_dataset1.csv", row.names = FALSE)

final_dataset <- select(final_dataset, -orig_title)
#final_dataset <- select(final_dataset, -overview)
final_dataset$title <- sapply(final_dataset$title, function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = ""))
final_dataset$genre <- sapply(final_dataset$genre, function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = ""))


final_dataset$budget_x[is.na(final_dataset$budget_x)] <- median(final_dataset$budget_x, na.rm = TRUE)
final_dataset$revenue[is.na(final_dataset$revenue)] <- median(final_dataset$revenue, na.rm = TRUE)


#------------- correlation----------------------
#Selecting only numerical columns for the correlation matrix
numerical_data <- final_dataset[, c("release_year", "budget_x", "revenue", "score")]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_data, use = "pairwise.complete.obs")  # Handles missing values

# Melt the correlation matrix for use with ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plotting the heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  labs(x = '', y = '', title = 'Heatmap of Correlation Matrix') +
  coord_fixed()

cor_matrix <- cor(final_dataset[, c("release_year", "budget_x", "revenue", "score")], 
                  use = "complete.obs")

# Optionally, round the correlation matrix for easier viewing
cor_matrix_rounded <- round(cor_matrix, 2)

# Print the rounded correlation matrix
print(cor_matrix_rounded)

cor_matrix_df <- as.data.frame(cor_matrix_rounded)

# View the data frame
View(cor_matrix_df)


#------------- GLM model----------------------
set.seed(123) # for reproducibility
index <- createDataPartition(final_dataset$revenue, p = 0.7, list = FALSE)
train_data <- final_dataset[index, ]
test_data <- final_dataset[-index, ]

# training data
glm_model_no_genre <- glm(revenue ~ budget_x + release_year + score, 
                          data = train_data, 
                          family = gaussian())

# Summary of the GLM model
summary(glm_model_no_genre)


predictions <- predict(glm_model_no_genre, newdata = test_data, type = "response")

# Evaluate the model performance
test_data$predicted_revenue <- predictions
rmse <- sqrt(mean((test_data$revenue - test_data$predicted_revenue)^2))
cat("RMSE:", rmse, "\n")

postResample(pred = predictions, obs = test_data$revenue)

glm_metrics <- postResample(pred = predictions, obs = test_data$revenue)
rmse_glm <- glm_metrics[1]
rsq_glm <- glm_metrics[2]

#------------- GAM model----------------------
set.seed(123) # for reproducibility
index <- createDataPartition(final_dataset$revenue, p = 0.7, list = FALSE)
train_data <- final_dataset[index, ]
test_data <- final_dataset[-index, ]

# Fit the GAM model on the training data
gam_model <- gam(revenue ~ s(budget_x) + s(release_year) + s(score), 
                 data = train_data, 
                 family = gaussian())

# Summary of the GAM model
summary(gam_model)

# Predict on the test data
predictions <- predict(gam_model, newdata = test_data)

# Evaluate the model performance
test_data$predicted_revenue <- predictions
rmse <- sqrt(mean((test_data$revenue - test_data$predicted_revenue)^2))
cat("RMSE:", rmse, "\n")

postResample(pred = predictions, obs = test_data$revenue)

gam_metrics <- postResample(pred = predictions, obs = test_data$revenue)
rmse_gam <- gam_metrics[1]
rsq_gam <- gam_metrics[2]


gam_model_enhanced <- gam(revenue ~ s(log(budget_x)) + s(release_year) + s(score) + ti(log(budget_x), score),
                          data = train_data, family = gaussian(),
                          method = "REML")

# Summary of the enhanced GAM model
summary(gam_model_enhanced)


#------------- 2nd iteration----------------------
# Predict on the test data
predictions_enhanced <- predict(gam_model_enhanced, newdata = test_data)

# Evaluate the model performance on the test data
test_data$predicted_revenue_enhanced <- predictions_enhanced
rmse_enhanced <- sqrt(mean((test_data$revenue - test_data$predicted_revenue_enhanced)^2))
cat("Enhanced Model RMSE:", rmse_enhanced, "\n")

gam_enhanced_metrics <- postResample(pred = predictions_enhanced, obs = test_data$revenue)
rmse_gam_enhanced <- gam_enhanced_metrics[1]
rsq_gam_enhanced <- gam_enhanced_metrics[2]


#------Random forest-----------------
set.seed(123) # for reproducibility
index <- createDataPartition(final_dataset$revenue, p = 0.7, list = FALSE)
train_data <- final_dataset[index, ]
test_data <- final_dataset[-index, ]

# Fit the Random Forest model on the training data
rf_model <- randomForest(revenue ~ budget_x + release_year + score, 
                         data = train_data,
                         ntree = 500,
                         importance = TRUE)

# Summarize the model
print(rf_model)

# Variable importance
var_imp <- importance(rf_model)
print(var_imp)

# Predict on the test data
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance on the test data
test_data$predicted_revenue <- predictions
rmse_rf <- sqrt(mean((test_data$revenue - test_data$predicted_revenue)^2))
cat("Random Forest Model RMSE:", rmse_rf, "\n")


# You may need to calculate the residuals for each movie
test_data$residuals <- test_data$revenue - test_data$predicted_revenue

# Aggregate residuals by release year
aggregate_resid <- aggregate(residuals ~ release_year, data = test_data, FUN = mean)

# Create a data frame for aggregate residuals
aggregate_resid_df <- as.data.frame(aggregate_resid)


# Creating an interactive plot using plotly
interactive_plot <- plot_ly(data = aggregate_resid_df, x = ~release_year, y = ~residuals, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Average Residuals by Release Year",
         xaxis = list(title = "Release Year"),
         yaxis = list(title = "Average Residuals"))

# Display the plot
interactive_plot

rf_metrics <- postResample(pred = predictions, obs = test_data$revenue)
rmse_rf <- rf_metrics[1]
rsq_rf <- rf_metrics[2]



#----------Classification------------------------------------------------


# Convert revenue to a binary variable
median_revenue <- median(final_dataset$revenue, na.rm = TRUE)
final_dataset$revenue_class <- ifelse(final_dataset$revenue > median_revenue, "High", "Low")

set.seed(123)

# Splitting the dataset into training and testing sets
splitIndex <- createDataPartition(final_dataset$revenue_class, p = .8, list = FALSE, times = 1)
train_data <- final_dataset[splitIndex, ]
test_data <- final_dataset[-splitIndex, ]

# Prepare training control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train the model using only the training set
rf_model <- train(revenue_class ~ budget_x + release_year + score + orig_lang,
                  data = train_data,
                  method = "rf",
                  trControl = fitControl,
                  metric = "ROC")

# Model Summary
print(rf_model)

predictions <- predict(rf_model, newdata = test_data)
predictions_factor <- factor(predictions, levels = c("Low", "High"))
actual_factor <- factor(test_data$revenue_class, levels = c("Low", "High"))

confMatrix <- confusionMatrix(predictions_factor, actual_factor)
print(confMatrix)

#-----------Summ
#--------------qualitative analysis----------


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


# Convert ISO country codes to country names
final_dataset$country_name <- countrycode(final_dataset$country.y, "iso2c", "country.name")

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

#Cluster Markers
leaflet(data = final_dataset) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~country_name, 
                   clusterOptions = markerClusterOptions()) %>%
  setView(lng = mean(final_dataset$lon, na.rm = TRUE), 
          lat = mean(final_dataset$lat, na.rm = TRUE), 
          zoom = 2)



#----------------insights---------------------------


# Sum revenue by country
revenue_by_country <- final_dataset %>%
  group_by(country.y) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE))

# interactive bar chart
plot_ly(data = revenue_by_country, x = ~country.y, y = ~total_revenue, type = 'bar') %>%
  layout(title = 'Total Revenue by Country',
         xaxis = list(title = 'Country'),
         yaxis = list(title = 'Total Revenue'))

genre_counts <- final_dataset %>%
  separate_rows(genre, sep = ",") %>%
  count(genre, sort = TRUE)
  

interactive_bar_chart <- plot_ly(data = genre_counts, x = ~genre, y = ~n, type = 'bar', marker = list(color = 'rgba(55, 128, 191, 0.7)')) %>%
  layout(title = 'Distribution of Shows by Genre',
         xaxis = list(title = 'Genre'),
         yaxis = list(title = 'Count'))

interactive_bar_chart

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


# Convert revenue to a binary variable
median_revenue <- median(final_dataset$revenue, na.rm = TRUE)
final_dataset$revenue_class <- ifelse(final_dataset$revenue > median_revenue, "High", "Low")

set.seed(123)

# Splitting the dataset into training and testing sets
splitIndex <- createDataPartition(final_dataset$revenue_class, p = .8, list = FALSE, times = 1)
train_data <- final_dataset[splitIndex, ]
test_data <- final_dataset[-splitIndex, ]

# Prepare training control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train the model using only the training set
rf_model <- train(revenue_class ~ budget_x + release_year + score + orig_lang,
                  data = train_data,
                  method = "rf",
                  trControl = fitControl,
                  metric = "ROC")

# Model Summary
print(rf_model)

predictions <- predict(rf_model, newdata = test_data)
predictions_factor <- factor(predictions, levels = c("Low", "High"))
actual_factor <- factor(test_data$revenue_class, levels = c("Low", "High"))

confMatrix <- confusionMatrix(predictions_factor, actual_factor)
print(confMatrix)


# summary table
accuracy_summary <- data.frame(
  Model = c("GLM", "GAM", "GAM 2nd Iteration", "Random Forest"),
  RMSE = c(rmse_glm, rmse_gam, rmse_gam_enhanced, rmse_rf),
  R_Squared = c(rsq_glm, rsq_gam, rsq_gam_enhanced, rsq_rf)
)

print(accuracy_summary)


# ----------------------wordcloud----------------------------
corpus <- Corpus(VectorSource(final_dataset$overview))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Build a term-document matrix
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

set.seed(1234)
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq, min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"), main = "Most Common Words in Movie Overviews")

#wordcloud analysis

analysis_dataset <- final_dataset[, c("revenue", "budget_x", "release_year", "score", "overview")]

analysis_dataset$overview <- as.character(analysis_dataset$overview)

# Create indicator variables for the presence of keywords "young", "life", "new" in the new dataset
keywords <- c("young", "life", "new")
for(keyword in keywords) {
  analysis_dataset[[keyword]] <- as.numeric(grepl(keyword, analysis_dataset$overview, ignore.case = TRUE))
}

# Remove rows with NA 
analysis_dataset <- na.omit(analysis_dataset)


full_model_analysis <- lm(revenue ~ budget_x + release_year + score + young + life + new, data = analysis_dataset)
summary(full_model_analysis)

#-----------------Association-------------------

text_corpus <- Corpus(VectorSource(final_dataset$overview))


text_corpus <- tm_map(text_corpus, content_transformer(tolower)) # Convert to lowercase
text_corpus <- tm_map(text_corpus, removePunctuation)            # Remove punctuation
text_corpus <- tm_map(text_corpus, removeNumbers)                # Remove numbers
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english")) # Remove stopwords
text_corpus <- tm_map(text_corpus, stripWhitespace)              # Remove extra white spaces


tdm <- TermDocumentMatrix(text_corpus)

associations <- findAssocs(tdm, "life", 0.25) 

print(associations)

#Visualisation

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


#-----------------------geographical---------------------------

# Convert ISO country codes to country names
final_dataset$country_name <- countrycode(final_dataset$country.y, "iso2c", "country.name")

# world map data, which includes lat/lon for country centers
world_map <- map_data("world")

# unique list of countries with their mean latitude and longitude
country_coords <- world_map %>%
  group_by(region) %>%
  summarize(lat = mean(lat), lon = mean(long), .groups = 'drop')

# Merge the coordinates
final_dataset <- merge(final_dataset, country_coords, by.x = "country_name", by.y = "region", all.x = TRUE)

na_count_country_name <- sum(is.na(final_dataset$country_name))
cat("Number of NA values in country_name column:", na_count_country_name, "\n")
na_count_country_code <- sum(is.na(final_dataset$country.y))
cat("Number of NA values in country.y column:", na_count_country_code, "\n")

invalid_rows <- which(is.na(final_dataset$lat) | is.na(final_dataset$lon) |
                        !is.numeric(final_dataset$lat) | !is.numeric(final_dataset$lon))

final_dataset <- final_dataset %>%
  mutate(
    lat = ifelse(country.y == "US", 37.0902, lat),
    lon = ifelse(country.y == "US", -95.7129, lon),
    lat = ifelse(country.y == "GB", 55.3781, lat),
    lon = ifelse(country.y == "GB", -3.4360, lon),
    lat = ifelse(country.y == "HK", 22.3193, lat),
    lon = ifelse(country.y == "HK", 114.1694, lon)
  )

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

#Cluster Markers
leaflet(data = final_dataset) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~country_name, 
                   clusterOptions = markerClusterOptions()) %>%
  setView(lng = mean(final_dataset$lon, na.rm = TRUE), 
          lat = mean(final_dataset$lat, na.rm = TRUE), 
          zoom = 2)


#-------------revenue by country-------------------

# Sum revenue by country
revenue_by_country <- final_dataset %>%
  group_by(country_name) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE))

# Using a logarithmic scale on the y-axis 
interactive_plot <- plot_ly(data = revenue_by_country, x = ~country_name, y = ~total_revenue, type = 'bar',
                            marker = list(color = ~total_revenue, colorscale = 'Jet')) %>%
  layout(title = 'Total Revenue by Country (Log Scale)',
         xaxis = list(title = 'Country'),
         yaxis = list(title = 'Total Revenue (Log Scale)', type = 'log'))
interactive_plot

#---------------shows by genre----------------

genre_counts <- final_dataset %>%
  separate_rows(genre, sep = ",") %>%
  count(genre, sort = TRUE)

interactive_bar_chart <- plot_ly(data = genre_counts, x = ~genre, y = ~n, type = 'bar', marker = list(color = 'rgba(55, 128, 191, 0.7)')) %>%
  layout(title = 'Distribution of Shows by Genre',
         xaxis = list(title = 'Genre'),
         yaxis = list(title = 'Count'))

interactive_bar_chart

#  interactive pie chart of shows by Genre
plot_ly(data = genre_counts, labels = ~genre, values = ~n, type = 'pie') %>%
  layout(title = 'Distribution of Shows by Genre')

#---------score by language---------

# avg score by language
avg_score_by_lang <- final_dataset %>%
  group_by(orig_lang) %>%
  summarise(average_score = mean(score, na.rm = TRUE))

# plot
plot_ly(data = avg_score_by_lang, x = ~orig_lang, y = ~average_score, type = 'scatter', mode = 'markers+lines') %>%
  layout(title = 'Average Score by Original Language',
         xaxis = list(title = 'Original Language'),
         yaxis = list(title = 'Average Score'))

#--------scatter plot Budget vs Revenue-----------
plot_ly(data = final_dataset, x = ~budget_x, y = ~revenue, type = 'scatter', mode = 'markers') %>%
  layout(title = 'Budget vs. Revenue',
         xaxis = list(title = 'Budget'),
         yaxis = list(title = 'Revenue'))

