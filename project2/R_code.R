#########################################################
# dataset: Basic Computer Data
# url: https://www.kaggle.com/kingburrito666/basic-computer-data-set
# capstone: IDV Learners
#########################################################

# Check if packages should be installed:

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org") 
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") 


# Load packages: 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)

# Download dataset from url:
# https://www.kaggle.com/kingburrito666/basic-computer-data-set
# set working folder to the one where dataset is being saved in

df <- read.csv("Computers.csv",sep=',')
head(df)

# Let's drop X column as it's a redundant data:

df$X <- NULL


################# DATA VIZUALIZATION ###################################
# Kernal density plots are usually a much more 
# effective way to view the distribution of a variable.
# Please, make sure you install this package properly:

if(!require(sm)) install.packages("sm", repos = "http://cran.us.r-project.org") 
library(sm)

# I utilize kernal density plots to compare groups,
# 'sm.density.compare' allows me to superimpose the 
# kernal density plots of price distribution for two
# different groups of each of the catergorical variables.

# Let's compare Price distributions for computers with 
# cd's and without with kernal density plot:

sm.density.compare(df$price, df$cd, xlab="price")
title(main="Price distribution density by CDs category (yes/no)")
legend("topright", levels(df$cd), fill=2+(0:nlevels(df$cd)))

# Now, compare Price distributions for computers with 
# multi-function and without:

sm.density.compare(df$price, df$multi, xlab="price")
title(main="Price distribution density by multi category (yes/no)")
legend("topright", levels(df$multi), fill=2+(0:nlevels(df$multi)))

# Finally, lets look at Price distributions for premium
# computers and standard: 

sm.density.compare(df$price, df$premium, xlab="price")
title(main="Price distribution density by premium category (yes/no)")
legend("topright", levels(df$premium), fill=2+(0:nlevels(df$premium)))

# Analysis of 3 kernel density plots shows me that in case of grouping 
# variable 'cd', the price distribution shifts to the right if computer has a CD ('yes')

# As for numerical variables, let's look at correlation matrix that shows 
# correlation coefficients between numeric variables. 
# First, install necessary packages and load libraries:

if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org") 
if(!require(corrgram)) install.packages("corrgram", repos = "http://cran.us.r-project.org") 


library(corrplot)
library(corrgram)

# Correlation matrix shows that 'price' positively correlated with 'ram', 'hd', 'screen' and 'speed'
# and negatively correlated with 'trend':

corrplot(cor(df %>% select_if(is.numeric), method='pearson'),method='square',order="AOE",tl.cex=1)

# Let's look at more complex visualization, which comprises of all our variables: 
# numeric and categorical. For this, let's make 2 lists:
# Variables 'price', 'speed', 'hd', 'ram', 'screen', 'ads', 'trend' are numeric list
# Variables 'cd', 'multi' and 'premium' are categorical list

num_column <- c('price', 'speed', 'hd', 'ram', 'screen', 'ads', 'trend')
cat_column <- c('cd', 'multi', 'premium')

# Install required package and load the library:

if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org") 
library(gridExtra)

list_plot <- list()
iter <- 0
for(feature in cat_column){
  for(j in 2:length(num_column)){
    iter <- iter + 1
    x = num_column[1]
    y = num_column[j]
    list_plot[[iter]] <- ggplot(data=df, aes_string(x, y, color = feature)) + 
       geom_point(size = 1, alpha = .5) + 
       theme(legend.position = 'top', axis.title = element_text(size=10)) + 
       scale_color_manual(name = feature, values = c("#FF0000", "#0000FF"))
  }
}

# Generation of the plot may take a few moment (depending on your computational power)
# I suggest you to Zoom In the plot, that comprises of 18 subplots
# We can see, that this viisualization supports insights we got from previous sections:
# For example, for all numeric variables positively correlated with price we observe
# that 'cd' variable is more often 'yes' with increasing 'price' variable

do.call(grid.arrange, c(list_plot, ncol = 6))


###################### DATA PREPROCESSING ######################################

######## MISSING DATA
# Let's check if there are any missing values in the dataset
# No NA's detected, no need to take care of missing data in our case:

any(is.na(df))

######## ENCODING CATEGORICAL VARIABLES
# One way to take care of categorical variables is to introduce dummy variables.
# There are many ways to create dummy variables, in the course we used factor() 
# to deal with categorical variable. I also use 'dummy.code()' from 'psych'-package.
# To keep it simple, let's encode our categorical variables with familiar factor()-approach":

df$cd = factor(df$cd,
                 levels = c('yes', 'no'),
                 labels = c(1, 0))

df$multi = factor(df$multi,
               levels = c('yes', 'no'),
               labels = c(1, 0))

df$premium = factor(df$premium,
               levels = c('yes', 'no'),
               labels = c(1, 0))

head(df)


########################## CROSS - VALIDATION #####################################

# Split data: 

set.seed(1234)
test_index <- createDataPartition(y = df$price, times = 1, p = 0.1, list = FALSE)
train <- df[-test_index, ]
test <- df[test_index, ]

# Also, let's exclude price column that we want to predict:

test_final <- test[, 2:10] 

# Cross-validation on the TRAIN dataset
# I've chosen popular regression algorithms:
# Linear regression, Support Vector Machine, Generalized Boosted Regression and Random Forest.
# Let's declare list of regressors: 

regressors <- c('lm','svmLinear','gbm','rf')

# Run train command for each regressor from our list and additionally let's keep track of timing:
# (Please, give it some time, might take 5-7 minutes)

time_tracker <- list()
model <- list()
iter <- 0
trControl <- trainControl(method = "cv", number = 10)

set.seed(7)
for (regressor in regressors) {
  iter <- iter + 1
  start.time <- Sys.time()
  model[[iter]] <- train(price ~ . , data = train, method = regressor, trControl = trControl, preProcess=c('center', 'scale'))
  end.time <- Sys.time()
  # Keep track of time for each regressor:
  time_tracker <- c(time_tracker, as.numeric(difftime(end.time, start.time, units="sec")))
}


# Results of cross-validation and plots:

names <- c()
for(i in 1:length(model)){
  names[i] <- model[[i]]$method
}

set.seed(7)
results <- resamples(list('lm' = model[[1]], 'svmLinear' = model[[2]], 'gbm' = model[[3]], 'rf' = model[[4]]))
bwplot(results, scales = list(relation = "free")) 
summary(results)

# Let's create a dataframe with results of modelling
# For each regressor, we list time required for algorithm to run
# mean RMSE and mean R squared (how well model fits data):

rmse_time <- data.frame('regressor' = names, 
                        'time' = c(time_tracker[[1]], 
                                  time_tracker[[2]],
                                  time_tracker[[3]],
                                  time_tracker[[4]]), 
                        'mean_RMSE' = c(mean(results[["values"]][["lm~RMSE"]]),
                                        mean(results[["values"]][["svmLinear~RMSE"]]),
                                        mean(results[["values"]][["gbm~RMSE"]]),
                                        mean(results[["values"]][["rf~RMSE"]])),
                        'mean_Rsquared' = c(mean(results[["values"]][["lm~Rsquared"]]),
                                            mean(results[["values"]][["svmLinear~Rsquared"]]),
                                            mean(results[["values"]][["gbm~Rsquared"]]),
                                            mean(results[["values"]][["rf~Rsquared"]])) )


################ VISUALIZATION OF THE CROSS-VAL RESULTS ################################

# We need to install and load reshape2 package to transform resulting dataframe:

if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org") 
library(reshape2)

# Let's visualize model key performing metrics (please, ZOOM IN)
# We can see that Random Forest requires significantly longer time to perform
# with mean RMSE comparably smaller and mean R squared being proportionally higher.
# Generalized Boosted Regression also demonstrates good performance, with 
# significantly shorter time, small mean RMSE and high mean R squared.
# Both models, Generalized Boosted Regression and Random Forest, worth to be considered.

# First, we reshape the resulting dataframe:

rmse_time <- melt(rmse_time)


# Plot results of cross-validation for our chosen regressors:

g1 <- ggplot(rmse_time[1:4,], aes(x = regressor, y= value, fill = variable), xlab="xlab") +
        geom_bar(stat="identity", width=.5, position = "dodge", color='red') +
        coord_flip() +
        theme(legend.position="bottom")
g2 <- ggplot(rmse_time[5:8,], aes(x = regressor, y= value, fill = variable), xlab="xlab") +
        geom_bar(stat="identity", width=.5, position = "dodge", color='blue') +
        coord_flip()+
        theme(legend.position="bottom")
g3 <- ggplot(rmse_time[9:12,], aes(x = regressor, y= value, fill = variable), xlab="xlab") +
        geom_bar(stat="identity", width=.5, position = "dodge", color='green') +
        coord_flip()+
        theme(legend.position="bottom")
grid.arrange(g1, g2, g3, nrow = 2)


# Because tuning GBM takes way too long (I terminated the process after 23 minutes)
# And it's the second best model, we proceed with the one that showed best performance
# even though it took longer CPU processing time - Random Forest

######################### RANDOM FOREST ###################################
regressor <- 'rf'

# Next step is to choose best parameteres/best RF model:

control <- trainControl(method="cv", number=10)
set.seed(7)
rf_model <- train(price ~ . , data = train, method = regressor, trControl = control, preProcess=c('center', 'scale'))

# By running the code above, we obtained the best model and we can see its optimal parameters:

rf_model                         #  mtry for RF model is equaled to 5, based on RMSE
plot(rf_model$finalModel)        #  number of trees is 500

# Let's calculate our predictions based on RF-model we trained:

rf_results <- predict(rf_model, test_final)


# Add predictions to our test dataframe and rearrange columns to its original order:

test_final <- cbind(test_final, rf_results)
names(test_final)[10] <- 'price'
test_final <- test_final[c(10, 1, 2, 3, 4, 5, 6, 7, 8, 9)]

# Re-factor categorical variables to its original format:

test_final$cd = factor(test_final$cd,
                       levels = c(1, 0),
                       labels = c('yes', 'no'))
                 

test_final$multi = factor(test_final$multi,
                       levels = c(1, 0),
                       labels = c('yes', 'no'))

test_final$premium = factor(test_final$premium,
                       levels = c(1, 0),
                       labels = c('yes', 'no'))

head(test_final)

# Let's look at the resulting RMSE.
# RMSE is negatively-oriented score such as lower values are better. 
# RMSE on the test set for RF model is quite close to RMSE on the train set (153 vs 157).
# Our model performed on the test set as expected.

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
set.seed(7)
rf_rmse <- RMSE(test$price, test_final$price)
rf_rmse

# Write final test dataset with predicted price to CSV file (optional):

write.csv(test_final %>% mutate(price=round(price, 0)), 
          "price_predictions", na = "", row.names=FALSE)

