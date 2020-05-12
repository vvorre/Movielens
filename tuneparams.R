if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("stringr")) install.packages("stringr")
if (!require("scales")) install.packages("scales")
if (!require("recosystem")) install.packages("recosystem")

library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(recosystem)


load("preprocessedData.Rda")

#############################################################################################
###                           Tuning for Model 7                                           ##
#############################################################################################

lambda <- 5

mu <- mean(edx_genre_split$rating)

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
fit <- loess(d_ui ~ as.numeric(rounded_date -  date("2000-01-01")),
             degree=1, span = .02, data=d_ui)

d_ui$d_ui <- fit$fitted 

# Create a data frame with all the terms b_i, b_u, d_ui
temp <- edx_genre_split %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(rounded_date = round_date(date_reviewed,"week")) %>%
  left_join(d_ui, by = "rounded_date")

# Calculate the coefficients for each genre for a total of 20 genres
b_g_array <- as.data.frame(sapply(1:20,function(k){
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


# Tune the parameters
# No. of iterations array
iter <- c(10,20,30,40)
#initialize a data frame
cross_val_rmse_res <- data.frame(dim = as.numeric(),iter = as.numeric(),loss_fun = as.numeric())
for (i in 1:length(iter)){
  #tune the model
  opts = r$tune(data_memory(residuals$userId, residuals$movieId, residuals$rsi,
                            index1 = TRUE), opts = list(dim = c(10,25,50,100,150,200,250),nfold = 10, 
                                                        niter = iter[i],costp_l2 = 0.01, costq_l2 = 0.1,
                                                        lrate = .05, costp_l1 = 0, costq_l1 = 0, nthread = 6))
  # Add the rmse results for each iteration number
  cross_val_rmse_res <- rbind(cross_val_rmse_res,opts$res %>% mutate(iter = iter[i]) %>% select(dim,iter,loss_fun))
}

cross_val_rmse_res$iter = as.factor(cross_val_rmse_res$iter)
cross_val_rmse_res$dim = as.factor(cross_val_rmse_res$dim)

# Plot to get the best paramaters
cross_val_rmse_res %>% ggplot(aes(dim,loss_fun, group = iter,color = iter)) +
  geom_line() +
  geom_point() +
  xlab("Number of factors") +
  ylab("RMSE") +
  scale_colour_discrete()+
  ggtitle("RMSE's for different dimensions")

#############################################################################################
###                           Tuning for Model 8                                           ##
#############################################################################################

iter <- c(10,20,30,40)
cross_val_rmse_rat <- data.frame(dim = as.numeric(),iter = as.numeric(),loss_fun = as.numeric())
for (i in 1:length(iter)){
  opts = r$tune(data_memory(edx$userId, edx$movieId, edx$rating,
                            index1 = TRUE), opts = list(dim =c(10,25,50,100,150,200,250),nfold = 10, 
                                                        niter = iter[i],costp_l2 = 0.01, costq_l2 = 0.1,
                                                        lrate = .05, costp_l1 = 0, costq_l1 = 0, nthread = 6))
  cross_val_rmse_rat <- rbind(cross_val_rmse_rat,opts$res %>% mutate(iter = iter[i]) %>% select(dim,iter,loss_fun))
}

cross_val_rmse_rat$iter = as.factor(cross_val_rmse_rat$iter)
cross_val_rmse_rat$dim = as.factor(cross_val_rmse_rat$dim)

cross_val_rmse_rat %>% ggplot(aes(dim,loss_fun, group = iter,color = iter)) +
  geom_line() +
  geom_point() +
  xlab("Number of factors") +
  ylab("RMSE") +
  scale_colour_discrete()+
  ggtitle("RMSE's for different dimensions")

# Save the tuning parameters
save(cross_val_rmse_rat,cross_val_rmse_res,file = "./data/tuneparams_SGD.Rda")
