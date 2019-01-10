library("dplyr")
library('dslabs')
library('caret')
library('e1071')
library('purrr')
library('lubridate')
library('tidyverse')

# Create edx set, validation set and submission file 
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

# Validation set will be 10% of MovieLens data
set.seed(1)
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

###### form your rating answer file
check <- validation
write.csv(check %>% select(userId, movieId, rating),
          "check.csv", na = "", row.names=FALSE)

# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings
validation <- validation %>% select(-rating)

# Ratings will go into the CSV submission file below:
write.csv(validation %>% select(userId, movieId) %>% mutate(rating = NA),
          "submission.csv", na = "", row.names=FALSE)
rm(dl, ratings, movies, test_index, temp, movielens, removed)



##############################CROSS-VAL of DIFFERENT ALGORITHMS

# lets take a subset of edx dataset
edx_sub <- edx[1:250,]

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# Naive Bayes
#set.seed(7)
#fit.nb <- train(rating~ as.factor(movieId) + as.factor(userId), data=edx_sub, method="nb", trControl=control)
# Linear Regression
set.seed(7)
fit.lm <- train(rating~ as.factor(movieId) + as.factor(userId), data=edx_sub, method="lm", trControl=control)
# SVM
set.seed(7)
fit.svm <- train(rating~ as.factor(movieId) + as.factor(userId), data=edx_sub, method="svmLinear", preProc=c("center", "scale"), trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(rating~ as.factor(movieId) + as.factor(userId), data=edx_sub, method="rf", trControl=control)
# collect resamples
results <- resamples(list(LM=fit.lm, SVM=fit.svm, RF=fit.rf))

# summarize differences between modes
summary(results)

# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)



################################# LINEAR SQUARED APPROACH #########
lambdas <- seq(0, 10, 0.25)
lambdas <- 0.25


accuracy <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(rating = mu + b_i + b_u) 
  
  predicted_ratings <- predicted_ratings %>%
    #mutate(rating = (ceiling(rating*2) /2))     #Alternative 1 
    mutate(rating = (round(rating, digits = 0))) #Alternative 2
  
  return(mean(predicted_ratings$rating == check$rating)) })
# Alternative 1 = Current accuracy = 0.25066925
# Alternative 2 = Current accuracy = 0.361403361

# xtab <- table(predicted_ratings$rating, check$rating)

#  confusionMatrix(xtab)

qplot(lambdas, accuracy)
lambdas[which.max(accuracy)]

############################ RECOMMENDERLAB ###############

# If not installed, first install following packages in R
install.packages('recommenderlab')
library(recommenderlab)
library(reshape2)

tr <- edx[,1:3]

# Using acast to convert above data as follows:
g<-acast(tr, userId ~ movieId)

# Convert it as a matrix
R<-as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")

# normalize the rating matrix
r_m <- normalize(r)
as(r_m, "list")

# Can also turn the matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)
as(r_b, "matrix")

# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))




################################# START OF QUIZ
# How many zeros/threes were given as ratings in the edx dataset?
sum(edx$rating == 3.0)

# How many different movies are in the edx dataset?
my_data <- edx %>% distinct(movieId)
nrow(my_data)  #10677

# How many different users are in the edx dataset?
my_users <- edx %>% distinct(userId)  # 69878

# How many movie ratings are in each of the following genres in the edx dataset?
# Drama
drama <- edx %>% group_by(genres) %>%
  filter(str_detect(genres,'Drama'))%>%
  nrow()  # 3910127
# Comedy
comedy <- edx %>% group_by(genres) %>%
  filter(str_detect(genres,'Comedy')) %>%
  nrow()  # 3540930
# Thriller
thriller <- edx %>% group_by(genres) %>%
  filter(str_detect(genres,'Thriller')) %>%
  nrow()  # 2325899
# Romance
romance <- edx %>% group_by(genres) %>%
  filter(str_detect(genres,'Romance')) %>%
  nrow()  # 1712100

# Which movie has the greatest number of ratings?
a <- movies$movieId[which(movies$title == 'Forrest Gump (1994)')]
b <- movies$movieId[which(movies$title == 'Jurassic Park (1993)')]
c <- movies$movieId[which(movies$title == 'Pulp Fiction (1994)')]
d <- movies$movieId[which(movies$title == 'Shawshank Redemption, The (1994)')]
e <- movies$movieId[which(movies$title == 'Speed 2: Cruise Control (1997)')]
vector <- c(a, b, c, d, e)

temp <- edx %>% filter(movieId %in% vector) %>%
  group_by(movieId) %>% count(movieId) 
# Pulp fiction

# What are the five most given ratings in order from most to least?
temp2 <- edx %>% group_by(rating) %>%
  count(rating)
# 4.0, 3.0, 5.0, 3.5, 2.0

################################ END OF QUIZ


