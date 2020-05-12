if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")
if (!require("httr")) install.packages("httr")


library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(httr)

# Method to choose the download process
# 0 - download preprocessed data
# 1 - download data from movielens and process it locally
choose_method <- 0 # default is set to 0

if(choose_method == 0){
# temp file to store the zip
temp <- tempfile(fileext = ".zip")
# write the content of the download to a binary file
writeBin(content(GET('https://www.dropbox.com/s/p5b5d0oknxujf32/data_movielens.zip?dl=1'), "raw"), temp)
# unzip it to data folder
unzip(temp , exdir = 'data' )
# rm the tempfile
rm(temp)
}else {
#############################################################################################
###                              Download the data                                         ##
#############################################################################################
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
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

save(edx,validation,file="./data/data.Rda")

load("./data/data.Rda")

#############################################################################################
###                            Preprocessing the data                                      ##
#############################################################################################

head(edx)

#Convert the timestamp into date_reviewed and seperate released year and title
temp1 <- edx %>% mutate(date_reviewed = date(as_datetime(timestamp))) %>%
  mutate(title = str_trim(title)) %>%
  extract(title,c("title", "year_released"), regex = "^(.*) \\((\\d{4})\\)$", remove = TRUE) 
#Validating that title and year_released have no null values 
temp1%>% filter(is.na(temp1$year_released) | is.na(temp1$title))%>% knitr::kable(.)

#Assigning data frame to edx
edx <- temp1
# remove the temp1 variable
rm(temp1)

# Add date reviewed and seperate released year and title for validation
temp2 <- validation %>% mutate(date_reviewed = date(as_datetime(timestamp))) %>%
  mutate(title = str_trim(title)) %>%
  extract(title,c("title", "year_released"), regex = "^(.*) \\((\\d{4})\\)$", remove = TRUE) 
temp2%>% filter(is.na(temp2$year_released) | is.na(temp2$title))%>% knitr::kable(.)
validation <- temp2
rm(temp2)

#Unique Genres in the whole list
genres_list <- unique((unlist(str_split(edx$genres, "\\|"))))

#Create columns for each genre. If a particular genre is present - 1 else 0
genres_columns <- sapply(seq(1,length(genres_list)),function(k){
  return(as.numeric(str_detect(genres_list[k],edx$genres)))})

#Rename the columns to the genres avaialable
colnames(genres_columns) = genres_list

#data frame with genres split
edx_genre_split <- cbind(edx,genres_columns)

#Create columns for each genre. If a particular genre is present - 1 else 0
genres_columns <- sapply(seq(1,length(genres_list)),function(k){
  return(as.numeric(str_detect(genres_list[k],validation$genres)))})

#Rename the columns to the genres avaialable
colnames(genres_columns) = genres_list

#data frame with genres split
validation_genre_split <- cbind(validation,genres_columns)

# Partition the edx into 4 parts to convert it to long format of genres. Partitioning due to small RAM
nelem_part <- round(nrow(edx)/4)
edx_partion_1 <- edx[1:nelem_part,]
edx_partion_2 <- edx[(nelem_part+1):(2*nelem_part),]
edx_partion_3 <- edx[(2*nelem_part+1):(3*nelem_part),]
edx_partion_4 <- edx[(3*nelem_part+1):nrow(edx),]

split_edx_1  <- edx_partion_1  %>% separate_rows(genres, sep = "\\|")
split_edx_2  <- edx_partion_2  %>% separate_rows(genres, sep = "\\|")
split_edx_3  <- edx_partion_3  %>% separate_rows(genres, sep = "\\|")
split_edx_4  <- edx_partion_4  %>% separate_rows(genres, sep = "\\|")

edx_genre_long <- rbind(split_edx_1,split_edx_2,split_edx_3,split_edx_4)
#Cleanup
rm(edx_partion_1,edx_partion_2,edx_partion_3,edx_partion_4,split_edx_1,split_edx_2,
   split_edx_3,split_edx_4)

#Convert Validation into long format
validation_genre_long <- validation%>% separate_rows(genres, sep = "\\|")

#Save the data into preprocessedData.Rda
save(edx,genres_list,validation,edx_genre_split,validation_genre_split,
     edx_genre_long,validation_genre_long,file="./data/preprocessedData.Rda")

}