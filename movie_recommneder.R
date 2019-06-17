library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

link <- read.csv("C:/Users/Jackson/Documents/Data Science/Machie Learning with R/data/links.csv")
movies <- read.csv("C:/Users/Jackson/Documents/Data Science/Machie Learning with R/data/movies.csv", stringsAsFactors = FALSE)
ratings <- read.csv("C:/Users/Jackson/Documents/Data Science/Machie Learning with R/data/ratings.csv")
tags <- read.csv("C:/Users/Jackson/Documents/Data Science/Machie Learning with R/data/tags.csv")

## Data Preprocessing 

genres <- as.data.frame(movies$genres, stringAsFactors = FALSE)
genres2 <- as.data.frame(tstrsplit(genres[,1],'[|]', type.convert = TRUE),
                                   stringAsFactors = FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation","Children", 
                "comedy", "Crime", "Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", 'Musical', 'Mystery', 'Romance', 'Sci-Fi', "Thriller", 'War', "Western") # total of 18 genres

genre_matrix  <- matrix(0,9743, 18) #empty matrix, for 9742 + 1 no. of movies, with 18 genres
genre_matrix[1,] <- genre_list #set first row to be genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix 
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert matrix into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringAsFactors = FASLE) #ignore first row, as it is the genre list

#convert from characters to integers
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
}

#create a matrix to search for a movie by genre
years <- as.data.frame(movies$title, stringsAsFactors = FALSE)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`,6),5),1,4))

search_matrix <- cbind(movies[,1], substr(movies[,2],1, nchar(movies[,2])-6), years, genre_matrix2)

colnames(search_matrix) <- c("movieId","title","year",genre_list)

write.csv(search_matrix, "C:/Users/Jackson/Documents/Data Science/Machie Learning with R/data/search.csv")
search_matrix <- read.csv("C:/Users/Jackson/Documents/Data Science/Machie Learning with R/data/search.csv", stringsAsFactors = FALSE)

#search of an Action movie produced in 1995:
subset(search_matrix, Action == 1 & year ==1995)$title

## Creating User Profile
binaryratings <- ratings

#ratings of 4/5 are mapped to 1 representing likes
#ratings of 3 and below are mapped to -1 to represent dislike

for (i in 1:nrow(binaryratings)) {
  if(binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else {
    binaryratings[i,3] <- -1
  }
}

#format binaryratings matrix to correct format
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = 'rating', na.rm = FALSE)

for (i in 1:ncol(binaryratings2)) {
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
#remove movieId cols. Rows are movieId, cols and userId
binaryratings2 = binaryratings2[,-1] 

#remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId))
ratingmovieIds <- length(unique(ratings$movieId))
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL

#remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(genre_matrix3) <- NULL

#calculate the dot product of the genre matrix and the ratings matrix and obtain user profiles

#Calculate the dot product for the User Profiles
result = matrix(0,18,610)
for (c in ncol(binaryratings2)){
  for (i in ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i])*(binaryratings2[,c])) #ratings per genre
  }
}


for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if(result[i,c] < 0) {
      result[i,c] <- 0
    }
    else{
      result[i,c] <- 1
    }
  }
}

result2 <- result[1,]
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat, function(x){as.integer(x)}))

#calculate jaccard distance between user profiles and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:8852]))
rows <- which(sim_results == min(sim_results))

#recommend movies
head(movies[rows,])

#Assumption is made that users like similar items, and retrieve movies that are closest in similarity to a user's profile,
#which represents a users preference for an item's features
# use Jaccard Distance to measure the similarity between user profiles 

## User-Based Collaborative Filtering Approach

#create ratings matrix. Rows = userId, COlumns = movieId
ratingmat <- dcast(ratings, userId~ movieId, value.var = 'rating', na.rm = FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #removes userId

# Method : UCBF 
# Similarity Calculation Method : Cosine Similarity
# Nearest Neighbors : 30

#convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ],
                               method = "cosine",
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users, main = "User similarity"))

#compute similarity between first four movies
similarity_items <- similarity(ratingmat[,1:4], method = 'cosine', which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

#Exploring values of ratings:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are the unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each raing value
table_ratings

#Visualize the rating:
vector_ratings <- vector_ratings[vector_ratings != 0] # ratings == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + ggtitle("Distribution of the ratings")

#Exploring viewings of movies:

views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) #create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
table_views$title <- NA

for (i in 1:9742){
  table_views[i,3] <- as.character(subset(movies,
                                          movies$movieId == table_views[i,1])$title)
}
table_views[1:6,]

ggplot(table_views[1:6,], aes(x = movie, y = views)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = table_views[1:6,]$title)
  ggtitle("Number of views of the top movies")
  
#Distribution of average movie rating  
average_ratings <- colMeans(ratingmat)

qplot(average_ratings) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 50]
qplot(average_ratings_relevant) + 
  stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))
  
#Visualizing the matrix:
image(ratingmat, main = "Heatmap of the rating matrix")
image(ratingmat[1:20, 1:25], main = "Heatmap of the first rows and columns")
image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)],
      main = "Heatmap of the top users and movies")

min_n_movies <- quantile(rowCounts(ratingmat), 0.99)
min_n_users <- quantile(colCounts(ratingmat), 0.99)
min_n_movies
min_n_users

image(ratingmat[rowCounts(ratingmat) > min_n_movies,
                           colCounts(ratingmat) > min_n_users],
      main = "Heatmap of the top users and movies")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                     colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)],
      main = "Heatmap of the top users and movies")

## Data Preperation
ratings_movies <- ratingmat[rowCounts(ratingmat) > 50,
                            colCounts(ratingmat) >50]

ratings_movies

## Item-based Collaborative Filtering Model
which_train <- sample(x = c(TRUE,FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE, 
                      prob = c(0.8,0.2))

recc_data_train <- ratings_movies[which_train,]
recc_data_test <- ratings_movies[!which_train,]


recc_model <- Recommender(data = recc_data_train, 
                          method = "IBCF",
                          parameter = list(k = 30))
recc_model

#Exploring the IBCF recommender model 
model_details <- getModel(recc_model)
class(model_details$sim)
dim(model_details$sim)

n_items_top <- 20
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

col_sums<- colSums(model_details$sim > 0)
qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of the column count") 

n_recommended <- 10 #items to recommend to each user

recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_predicted

recc_user_1 <- recc_predicted@items[[1]] # recommendation for the first user
movie_user_1 <- recc_predicted@itemLabels[recc_user_1]
movie_user_2 <- movie_user_1

for (i in 1:10){
  movie_user_2[i] <- as.character(subset(movies, movies$movieId == movie_user_1[i])$title)
}

movie_user_2

recc_matrix <- sapply(recc_predicted@items,
                      function(x){as.integer(colnames(ratings_movies)[x])})

recc_matrix[,1:4]

number_of_items <- factor(table(recc_matrix))

qplot(number_of_items) + ggtitle("Distribution of the number of items for IBCF \nNumber of times a movie is recommended")

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, movies$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie title", "No of times")
head(table_top)

## Create UBFC Recommender Model

recommender_model <- Recommender(recc_data_train,
                                 method = "UBCF",
                                 param = list(method = "Cosine", nn = 30))

model_details <- getModel(recommender_model)
model_details$data

n_recommended <- 10 #items to recommend to each user

reccommender_predicted <- predict(object = recommender_model, 
                          n = n_recommended)
reccommender_predicted

reccommender_matrix <- sapply(reccommender_predicted@items,
                      function(x){as.integer(colnames(ratings_movies)[x])})

reccommender_matrix[,1:4]

number_of_items <- factor(table(reccommender_matrix))

qplot(number_of_items) + ggtitle("Distribution of the number of items for UBCF \nNumber of times a movie is recommended")

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(as.integer(names(number_of_items_top)),
                        number_of_items_top)

for (i in 1:4){
  table_top[i,1] <- as.character(subset(movies, movies$movieId == table_top[i,1])$title)
}

colnames(table_top) <- c("Movie title", "No of times")
head(table_top)


###

recom <- predict(recommender_model,
                 ratingmat[1],
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom

recc_matrix <- sapply(recom@items, function(x){colnames(ratingmat)[x] })
dim(recc_matrix)

recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)

for( i in 1:10){
  recom_result[i] <- as.character(subset(movies,
                                          movies$movieId == as.integer(recom_list[[1]][i]))$title)
}

#Evaluation:
evaluation_scheme <- evaluationScheme(ratingmat,
                                      method = 'cross-validation',
                                      k = 5, given = 3,
                                      goodRating= 5)
evaluation_result <- evaluate(evaluation_scheme,
                              method = "UBCF",
                            n = c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_result)[[1]]

eval_results # recommened movies for user


#Evaluating the Recommender system

## Spliting the Data

min(rowCounts(ratings_movies))

percentage_training <- 0.8
items_to_keep <- 5
rating_threshold <- 3
n_eval <- 1

eval_sets <- evaluationScheme(data = ratings_movies,
                              method = 'split',
                              train = percentage_training,
                              given = items_to_keep,
                              goodRating = rating_threshold,
                              k = n_eval)
eval_sets

getData(eval_sets, "train")
getData(eval_sets, "known")
getData(eval_sets, "unknown")

qplot(rowCounts(getData(eval_sets, "unknown"))) +
  geom_histogram(binwidth = 10) +
  ggtitle("Unknown items by the users")


#Bootstrapping the data

eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "bootstrap",
                              train = percentage_training,
                              given = items_to_keep,
                              goodRating = rating_threshold,
                              k = n_eval)

table_train <- table(eval_sets@runsTrain[[1]])
n_repitions <- factor(as.vector(table_train))
qplot(n_repitions) + ggtitle("Number of reptitinos in the training set")
#shows omst of the users have been sampled fewer than four times

## Using Cross-Validation

n_fold <- 4 
eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "cross-validation",
                              given = items_to_keep,
                              goodRating = rating_threshold,
                              k = n_fold)

size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

model_to_evaluate <- "IBCF"
model_parameters <- NULL

eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate,
                                parameter = model_parameters)

items_to_recommend <- 10

eval_prediction <- predict(object = eval_recommender,
                           newdata = getData(eval_sets, "known"),
                           n = items_to_recommend,
                           type = 'ratings')
qplot(rowCounts(eval_prediction)) +
        geom_histogram(binwidth = 10) +
        ggtitle("Distribution of movies per user")

#Accuracy measures of each user

eval_accuracy <- calcPredictionAccuracy( x = eval_prediction,
                                          data = getData(eval_sets, "unknown"),
                                          byUser = TRUE)
head(eval_accuracy)

qplot(eval_accuracy[,"RMSE"]) + 
  geom_histogram(binwidth = 0.1) + 
  ggtitle("Distribution of the RMSE by User")

#Accuracy of the whole model
eval_accuracy <- calcPredictionAccuracy( x = eval_prediction,
                                         data = getData(eval_sets, "unknown"),
                                         byUser = FALSE)
head(eval_accuracy)

## Evaluating the recommendations

results <- evaluate( x = eval_sets,
                    method = models_to_evaluate,
                    n = seq(10,100,10))

head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indicies_summed <- Reduce("+", getConfusionMatrix(results))[,columns_to_sum]
head(indicies_summed)

# PLot of ROC and precision/recall curves

plot(results, annotate = TRUE, main = "ROC curve")
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")
# shows that if a small % of movies if recommended, the precision of the prediciton decreases, and vice versa eg. higher percentage of rated movies is recommended the higher is the recall

#optimizing hyperparamaters

vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <-lapply(vector_k, function(k){
  list(name = "IBCF",
       param = list(method = "cosine", k = k))
})

names(models_to_evaluate) <- paste0("ICBF_k_", vector_k)

n_recommendations <- c(1, 5, seq(10,100,10))
list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)

plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-Recall")



