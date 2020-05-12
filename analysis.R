if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("caret")) install.packages("caret")
if (!require("stringr")) install.packages("stringr")
if (!require("scales")) install.packages("scales")
if (!require("recosystem")) install.packages("recosystem")
if (!require("httr")) install.packages("httr")

library(tidyverse)
library(caret)
library(lubridate)
library(stringr)
library(scales)
library(recosystem)
library(httr)

# load preprocessed data
load("./data/preprocessedData.Rda")

#############################################################################################
###                            Preliminary analysis                                        ##
#############################################################################################

#look at the data
glimpse(edx)

glimpse(validation)

glimpse(edx_genre_split)

glimpse(validation_genre_split)

glimpse(edx_genre_long)

glimpse(validation_genre_long)


#number of ditinct users and movies
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId)) %>%
  knitr::kable()


#Distribution of number of movie ratings
edx %>% 
  group_by(movieId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(fill = "#0072B2", color = "black", bins = 50) +
  xlim(c(0, 2000)) +
  xlab("bins") +
  ylab("number of user ratings per movie ") 
#ggtitle("Distribution of User Ratings per movie")

#Distribution of Users
edx %>% 
  group_by(userId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(fill = "#0072B2", color = "black", bins = 100) +
  xlim(c(0, 500)) +
  xlab("bins") +
  ylab("number of movies rated per user ") 
#ggtitle("Distribution of Movies rated per user")

#time effect
time_eff <- edx %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  group_by(rounded_date) %>%
  summarize(rating_time = mean(rating))

# fitting a smoothing function
fit <- loess(rating_time ~ as.numeric(rounded_date),
             degree = 1, span = .03, data = time_eff)

time_eff %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(rounded_date, rating_time)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(rounded_date, smooth), color="red") +
  xlab("year (binned in weeks)") +
  ylab("Average rating")

#Remove the variable  
rm(time_eff,fit)

# Genre effect
# Average rating per genre
# Note we use long formated set to plot the genre effect
edx_genre_long %>%
  group_by(genres) %>%
  summarize(mean_rating_genre = mean(rating)) %>%
  arrange(-mean_rating_genre) %>%
  ggplot(aes(reorder(genres, mean_rating_genre), mean_rating_genre, fill= mean_rating_genre))+
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_gradient(low = "yellow", high = "red")+
  scale_y_continuous(limits = c(3.2, 4.1),oob = rescale_none) +
  ylab("Mean Rating") +
  xlab("Genre") 

#rmse to evaluate our models
rmse <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#############################################################################################
###                            Model 1: Simple model                                      ##
#############################################################################################
# Model 1: Simple model Y = mu +e- Normal distribution- Mean ratings for all the users

mu <- mean(edx$rating) 
mu
rmse_model_1 <- rmse(validation$rating,mu)
# a data frame to store the results

rmse_res <- data.frame(method = "Simple Model", RMSE = rmse_model_1)
rmse_res%>% knitr::kable(.)

#############################################################################################
###                            Model 2: Movie effects                                      ##
#############################################################################################
# Model 2 : Movie effects Y = mu + bu+ e. We can calculate the bias term by getting the expectation value
#of Y- mu
mu <- mean(edx$rating) 

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predict_y_hat_bi <- mu + 
  validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

rmse_model_2 <- rmse(predict_y_hat_bi, validation$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method="Movie Effects Model",
                                           RMSE = rmse_model_2 ))

rmse_res %>% knitr::kable()

#Clean up
rm(predict_y_hat_bi,movie_avgs,mu)
gc()

#############################################################################################
###                            Model 3: Movie and User effects                             ##
#############################################################################################
# Model 3 Movie and User effects Let's add a bias term from user to the model Y = mu + bi + bu+ emu <- mean(edx$rating) 
mu <- mean(edx$rating) 

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

predict_y_hat_bi_bu <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_model_3 <- rmse(predict_y_hat_bi_bu, validation$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method="Movie and User Effects Model",
                                           RMSE = rmse_model_3 ))

rmse_res %>% knitr::kable()

#Clean up
rm(predict_y_hat_bi_bu,user_avgs,movie_avgs,mu)

#############################################################################################
###              Model 4: Movie and User effects with Regularization                       ##
#############################################################################################
# Model 4:Movie and User effects with Regularization

# Divide the edx set for cross-validation and train when using regularization
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx[-test_index,] # Trainset
temp <- edx[test_index,]

# Make sure userId and movieId in cross validation set are also in edx set
edx_cv <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")
# Add rows removed from cross validation set back into edx train set
removed <- anti_join(temp, edx_cv)
edx_train <- rbind(edx_train, removed)

rm(removed,temp)

#Initialize a sequence of lambdas
lambdas <- seq(0, 10, 0.25)

rmses_reg <- sapply(lambdas, function(l){
  
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_cv %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(rmse(predicted_ratings, edx_cv$rating))
})

rm(mu,b_i,b_u,predicted_ratings)

# Plot and look at the minimum
data.frame(lambdas,rmses_reg) %>% ggplot(aes(lambdas, rmses_reg)) +
  geom_point() +
  xlab(expression(lambda)) +
  ylab("RMSE")

lambda = lambdas[which.min(rmses_reg)]

#rm(rmses_reg,lambdas)

mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings_reg_movie_user <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_model_4 <- rmse(predicted_ratings_reg_movie_user, validation$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method="Movie and User Effects with Regularization Model",
                                           RMSE = rmse_model_4 ))
#Clean up
rm(mu,b_i,b_u,predicted_ratings_reg_movie_user)

rmse_res %>% knitr::kable(.)


#############################################################################################
###            Model 5:  Movie, User Effect with Regularization and Time Effect            ##
#############################################################################################
# Model 5:  Movie, User Effect with Regularization and Time Effect

lambda <- 5
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

d_ui <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  group_by(rounded_date) %>%
  summarize(d_ui = mean(rating - b_i - b_u - mu))

# fitting a smoothing function
fit <- loess(d_ui ~ as.numeric(rounded_date),
             degree=1, span = .02, data=d_ui)

# Replace the values with the fitted values from the smooth function
d_ui$d_ui <- fit$fitted 

predicted_ratings_reg_movie_user_time <-
  validation %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(d_ui, by = "rounded_date") %>%
  mutate(pred = mu + b_i + b_u + d_ui) %>%
  pull(pred)

rmse_model_5 <- rmse(predicted_ratings_reg_movie_user_time, validation$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method="Movie, User Effect with Regularization
                                           and Time Effect",RMSE = rmse_model_5 ))
#Clean up
rm(mu,b_i,b_u,d_ui,fit,predicted_ratings_reg_movie_user_time)

rmse_res %>% knitr::kable(.)

#############################################################################################
###       Model 6: Movie, User Effect with Regularization, Time and Genre Effect           ##
#############################################################################################
# Model 6: Movie, User Effect with Regularization, Time and Genre Effect

lambda <- 5

mu <- mean(edx$rating)

b_i <- edx_genre_split %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx_genre_split %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

d_ui <- edx_genre_split%>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  group_by(rounded_date) %>%
  summarize(d_ui = mean(rating - b_i - b_u - mu))

# fitting a smoothing function
fit <- loess(d_ui ~ as.numeric(rounded_date),
             degree=1, span = .02, data=d_ui)
# Replace the values with the fitted values from the smooth function
d_ui$d_ui <- fit$fitted 

# Create a data frame with all the terms b_i, b_u, d_ui
temp <- edx_genre_split %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(d_ui, by = "rounded_date")

# Calculate the coefficients for each genre for a total of 20 genres
b_g_array <- as.data.frame(sapply(1:length(genres_list),function(k){
  temp %>% filter(.[8+k] == 1) %>%
    summarize(b_g = mean(rating - mu - b_i - b_u - d_ui))
}))
# Since the b_g's are associated with each movie, filter by movie
temp <- temp %>% distinct(movieId,.keep_all = TRUE)
# Calculate the sum of corresponding coefficients mutiplied with values for each movie.
b_g_sum <- rowSums((temp[,9:28]) * (b_g_array[rep(seq_len(nrow(b_g_array)), each = nrow(temp)), ]))  
# Construct a data frame with genre contributions
b_g <- data.frame(movieId = temp$movieId, b_g = b_g_sum)

rm(temp)

predicted_ratings_reg_movie_user_time_genre <-
  validation_genre_split %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(d_ui, by = "rounded_date") %>%
  left_join(b_g, by = "movieId")%>%
  mutate(pred = mu + b_i + b_u + d_ui + b_g) %>%
  pull(pred)

rmse_model_6 <- rmse(predicted_ratings_reg_movie_user_time_genre, validation_genre_split$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method="Movie, User Effect with Regularization, Time and                Genre Effect", RMSE = rmse_model_6))

rm(mu,b_i,b_u,d_ui,b_g,b_g_sum,b_g_array,fit,predicted_ratings_reg_movie_user_time_genre)

rmse_res %>% knitr::kable(.)

#############################################################################################
###     Model 7: Bias effects with Matrix Factorization of Residuals using SGD             ##
#############################################################################################
# Model 7: Bias effects with Matrix Factorization of Residuals using SGD

mu <- mean(edx$rating)

b_i <- edx_genre_split %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx_genre_split %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

d_ui <- edx_genre_split%>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  group_by(rounded_date) %>%
  summarize(d_ui = mean(rating - b_i - b_u - mu))

# fitting a smoothing function
fit <- loess(d_ui ~ as.numeric(rounded_date),
             degree=1, span = .02, data=d_ui)
# Replace the values with the fitted values from the smooth function
d_ui$d_ui <- fit$fitted 

# Create a data frame with all the terms b_i, b_u, d_ui
temp <- edx_genre_split %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(d_ui, by = "rounded_date")

# Calculate the coefficients for each genre for a total of 20 genres
b_g_array <- as.data.frame(sapply(1:length(genres_list),function(k){
  temp %>% filter(.[8+k] == 1) %>%
    summarize(b_g = mean(rating - mu - b_i - b_u - d_ui))
}))
# Since the b_g's are associated with each movie, filter by movie
temp <- temp %>% distinct(movieId,.keep_all = TRUE)
# Calculate the sum of corresponding coefficients mutiplied with values for each movie.
b_g_sum <- rowSums((temp[,9:28]) * (b_g_array[rep(seq_len(nrow(b_g_array)), each = nrow(temp)), ]))  
# Construct a data frame with genre contributions
b_g <- data.frame(movieId = temp$movieId, b_g = b_g_sum)

rm(temp)

residuals <-
  edx_genre_split %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(d_ui, by = "rounded_date") %>%
  left_join(b_g, by = "movieId")%>%
  mutate(rsi = rating - mu - b_i - b_u - d_ui - b_g) %>%
  select(userId,movieId,rsi)

# Start the recommender object
r = Reco()

# Tune the parameters in tuneparams.R and load it

load("./data/tuneparams_SGD.Rda")
# Plot to get the best paramaters
cross_val_rmse_res %>% ggplot(aes(dim,loss_fun, group = iter,color = iter)) +
  geom_line() +
  geom_point() +
  xlab("Number of factors") +
  ylab("RMSE") +
  scale_colour_discrete()+
  ggtitle("RMSE's for different latent factors")

# Use 20 iterations and 200 latent factors
# Train the model with the choosen parameters
r$train(data_memory(residuals$userId, residuals$movieId, residuals$rsi, index1 = TRUE),
        opts = c(dim = 200,costp_l2 = 0.01, costq_l2 = 0.1,lrate = .1,costp_l1 = 0, costq_l1 = 0, 
                 nthread = 6, niter = 20))

#predict the residuals for the validation set
predict_resi <- r$predict(data_memory(validation_genre_split$userId,
                                      validation_genre_split$movieId, 
                                      NULL, index1 = TRUE), out_memory())
# Convert it into a data frame
predict_resi <- data.frame(userId = validation_genre_split$userId,
                           movieId = validation_genre_split$movieId, resi = predict_resi)

predict_rating_bias_SGD <-
  validation_genre_split %>% 
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(d_ui, by = "rounded_date") %>%
  left_join(b_g, by = "movieId")%>%
  left_join(predict_resi, by = c("userId","movieId")) %>%
  mutate(predict =  mu + b_i + b_u + d_ui + b_g + resi) %>%
  pull(predict)

rmse_model_7 <- rmse(predict_rating_bias_SGD,validation_genre_split$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method="Bias effects with Matrix Factorization
                                           of Residuals using SGD", RMSE = rmse_model_7 ))

rm(b_i,b_u,d_ui,b_g,predict_resi,predict_rating_bias_SGD)

rmse_res %>% knitr::kable(.)


#############################################################################################
###         Model 8: Matrix Factorization of Ratings using SGD                             ##
#############################################################################################
# Model 8: Matrix Factorization of Ratings using SGD

# Tune params in tuneparams.c
 # Plot the tune parameter to get the best params
cross_val_rmse_rat %>% ggplot(aes(dim,loss_fun, group = iter,color = iter)) +
  geom_line() +
  geom_point() +
  xlab("Number of factors") +
  ylab("RMSE") +
  scale_colour_discrete()+
  ggtitle("RMSE's for different latent factors")

# choose 30 iterations and 200 latent factors 
# Train the model with the choosen parameters

r$train(data_memory(edx$userId, edx$movieId, edx$rating, index1 = TRUE),
        opts = c(dim = 200,costp_l2 = 0.01, costq_l2 = 0.1,lrate = .1,costp_l1 = 0, costq_l1 = 0, 
                 nthread = 6, niter = 30))
#predict the ratings for the validation set
predict_rating_SGD <- r$predict(data_memory(validation$userId, validation$movieId, 
                                            NULL, index1 = TRUE), out_memory())

rmse_model_8 <- rmse(predict_rating_SGD,validation$rating)

rmse_res <- bind_rows(rmse_res, data_frame(method=" Matrix Factorization of Ratings using SGD"
                                           , RMSE = rmse_model_8))

rmse_res %>% knitr::kable(.)

save(rmse_res,lambdas,rmses_reg, file = "./data/results.Rda")
