# Check if packages should be installed:
if('caret' %in% rownames(installed.packages()) == FALSE) {install.packages('caret')}
if('dlyr' %in% rownames(installed.packages()) == FALSE) {install.packages('dplyr')}
if('tidyverse' %in% rownames(installed.packages()) == FALSE) {install.packages('tidyverse')}


# Load libraries:
library('caret')
library('dplyr')
library('tidyverse')

# Create edx set, validation set and submission file (as per provided instruction):
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data:
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set:
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set:
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Learners will develop their algorithms on the edx set
# Algorithm should predict rating values for validation set:
validation <- validation %>% select(-rating)

# Release memory by removing unnecessary objects:
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Set the penalization term lambda
# Based on cross-validation analysis smallest numbers of lambda 
# Delivers better accuracy. In fact, the best accuracy value is 
# achieved with lambda = 0. However, we cannot exclude penalizing term
# from the equation as we need to account for large movie and user effects (b is far from zero):
lambda <- 0.25

# Estimate average raiting:
mu <- mean(edx$rating)
  
# Estimate movie effect:
b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))

# Estimate user effect:  
b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Calculate predicted rating:  
predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(rating = mu + b_i + b_u) 

# Set the proper rounding rule. Predictions have a floating point,
# which should be approximated to halves-numbers as digits in rating are.
# Taking into consideration the fact that there are more whole star ratings
# then half star, I decided to round to whole number (Alternative 2):

predicted_ratings <- predicted_ratings %>%
    # mutate(rating = (ceiling(rating*2) /2))     # Alternative 1 
    mutate(rating = (round(rating, digits = 0)))  # Alternative 2
  
# Add predictions to validation set by adding one more column for rating:
validation <- data.frame(cbind(validation, predicted_ratings$rating))
colnames(validation)[6] <- "rating"

# Ratings will go into the CSV submission file below:
write.csv(validation %>% select(userId, movieId, rating),
          "submission.csv", na = "", row.names=FALSE)


