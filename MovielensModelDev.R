library(tidyverse)
library(caret)

# Data files were generated using the following code provided by edX. 
# This downloads the dataset and creates the necessary partition
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Merge the ratings with the title and genre information, since these are in separate files

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# edx is the training set
# validation is the test set and consists of 10% of MovieLens dataset

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Data visualization
# ------------------

# Check first 5 rows and see the number of observations (dimension)
head(edx)
dim(edx)

# Count number of movies and users in the dataset
edx %>%
  summarise(n_movies = n_distinct(movieId),
            n_users = n_distinct(userId))
# Dataset was visually inspected through sorting and there seem to be no NA values, which is good

# Which movies have the highest average ranking?
edx %>%
  group_by(title) %>%
  summarise(rating = mean(rating)) %>%
  top_n(10) %>%
  arrange(desc(rating))

# Which movies have the most ratings?
edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  top_n(10) %>%
  arrange(desc(count))

# Is there a movie effect? i.e. do some movies get a lot more ratings
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "lightsteelblue") + 
  scale_x_log10() +
  ylab("Number of movies") +
  xlab("log Number of ratings") +
  ggtitle("Histogram of rating frequency")
# There are a different number of ratings for each movie so taking the mean for each is a plausible model

# Is there a user effect? i.e. do some users rate more
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black", fill = "lightsteelblue") + 
  scale_x_log10() +
  ylab("Number of movies") +
  xlab("log Number of users") +
  ggtitle("Histogram of user rating frequency")
# Some users make a lot more ratings

# Do some users rate higher than others?
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black", fill = "lightsteelblue") +
  ylab("Number of ratings") +
  xlab("Average rating of users") +
  ggtitle("Histogram of average user rating")
# Users seem to rate differently so a user effect is worth accounting for in the model

# Is there a genre effect? i.e. do movies in certain genres always get a higher rating?
# Note: mixed genre (e.g. Drama Comedy) are split into separates with this code.
edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  xlab("Genres") +
  ylab("Average rating") +
  ggtitle("Average rating of films in each genre") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# This shows a clear genre effect in this dataset

# Machine learning models and prediction
# --------------------------------------

# Definition of RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Building the recommendation system
# Model 1: Just use the average rating as starting predicted movie rating

mu_hat <- mean(edx$rating)
mu_hat

naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse

# Report model RMSE results in a table 
rmse_results <- data_frame(method = "Taking the average", RMSE = naive_rmse)
rmse_results

# Model 2: including the average rating for a movie
# Note: same as this: fit <- lm(rating ~ as.factor(userId), data = movielens)

mu_hat <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat))

predicted_ratings <- mu_hat + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Rating Effect Model",
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Model 3: also including average user ratings
# Note: same as this: lm(rating ~ as.factor(movieId) + as.factor(userId))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

# Model 4: movie rating plus average genre ratings
# Note: same as this: lm(rating ~ as.factor(movieId) + as.factor(genres))

genre_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(b_g = mean(rating - mu_hat))

predicted_ratings <- mu_hat + validation %>% 
  left_join(genre_avgs, by='userId') %>%
  .$b_g

model_4_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre Effect Model",  
                                     RMSE = model_4_rmse ))

# The final list of models sorted by increasing RMSE
rmse_results %>% 
  arrange(RMSE) %>%
  knitr::kable()
