library(dplyr)
library(lubridate)
library(ggplot2)


# Check class and rough form
head(edx)
class(edx)

# Check dimensions of data frame
nrow(edx)
ncol(edx)
dim(edx)

# Count number of 0 ratings
zerorating <- edx %>%
  filter(rating == 0) %>% 
  summarise(ratings_num=n())
zerorating

# Count number of 3 ratings
threerating <- edx %>%
  filter(rating == 3) %>%
  tally()
threerating

# Count number of movies and users
edx %>%
  summarise(n_movies = n_distinct(movieId),
            n_users = n_distinct(userId))

# Group by genre and count number of movies ratings in each for Drama, Comedy, Thriller and Romance
edx %>% filter(str_detect(genres,"Drama")) %>% nrow()
edx %>% filter(str_detect(genres,"Comedy")) %>% nrow()
edx %>% filter(str_detect(genres,"Thriller")) %>% nrow()
edx %>% filter(str_detect(genres,"Romance")) %>% nrow()

# Group by genre graders code
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Which movie has greatest number of rankings
edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  top_n(10) %>%
  arrange(desc(count))

# 5 Most common rankings sorted max to min
edx %>% 
  group_by(rating) %>%
  summarize(count = n()) %>%
  top_n(5) %>%
  arrange(desc(count)) 

