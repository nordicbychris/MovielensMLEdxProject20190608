library(tidyverse)
library(caret)
library(ggplot2)

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

model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Rating Effect Model",
                                     RMSE = model_1_rmse ))
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

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()



