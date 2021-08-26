
## Step1: Load the packages and read in the data

library(stringr) 
library(jsonlite)
library(plyr)
library(lubridate)
library(parallel)
library(foreach)
library(rpart)
library(randomForest)
library(xgboost)

library(doSNOW)
library(foreach)
library(parallel)

library(plyr); library(dplyr)

################################### SECTION 3: PREPARE THE DATA  ###########################################

# Set up your multiple cores as separate workers and then make them a cluster.
workers <- detectCores()
workers # How many cores do you have?
cluster <- makeCluster(workers, type = "SOCK")
registerDoSNOW(cluster)

# Load in the training set.
train.df<-tweetsFestival


dim(train.df)


# 3.1.2. Adding derived attributes in R

# str_count is from package "stringr"

train.df$Handle_Count <- str_count(train.df$text, "@") # Returns the number of "@" in a tweet.

train.df$Hashtag_Count <- str_count(train.df$text, "#") # Returns the number of "#" in a tweet.


# Read in the "Happiness Dictionary". 
happiness_dictionary <- read.csv("Hedonometer.csv")
head(happiness_dictionary)

happiness_dictionary_happs <- happiness_dictionary[ , c(2, 3)]

# We use the column "happs" to score the words.
head(happiness_dictionary_happs)

ntweets <- length(train.df$textCleanLem)
for (i in 1:ntweets) {
  this_tweet <- tolower(as.character(train.df$textCleanLem[i]))
  this_tweet_words <- strsplit(this_tweet, split = " ")
  this_tweet_words_vector <- unlist(this_tweet_words)
  this_tweet_words_df <- data.frame("word" = this_tweet_words_vector)
  single_tweet_merged <- merge(this_tweet_words_df , happiness_dictionary_happs, by = "word", type = "inner")
  train.df$avg_score[i] <- mean(single_tweet_merged$happs)
}

#3.2. Missing data

summary(train.df["avg_score"])


train.df$avg_score<-ifelse(is.na(train.df$avg_score),
                           mean(train.df$avg_score,na.rm=TRUE),
                           train.df$avg_score)

summary(train.df["avg_score"])


# Write train.df into a CSV file.
write.csv(train.df, '/Users/taniaturdean/Desktop/data analytics 2 twitter_train_allVariables.csv', row.names=FALSE)


################################### SECTION 2: UNDERSTAND THE DATA  ###########################################

#2.3. Correlation between the variables

#summary stats for the first 12 census data columns 

summary(train.df[,2:20]) 

#numeric columns
numeric.df<-train.df[, c("hour","year","month","day","Handle_Count","Hashtag_Count","avg_score","retweets_nr","reply_nr","like_nr","quote_nr","Score")]

#correlation plot

library(ellipse);library(RColorBrewer);    options(repr.plot.height=10)
my_colors=colorRampPalette(brewer.pal(5, "Spectral"))(100)

data=cor(numeric.df,use="complete.obs")

plotcorr(data , col=my_colors[data*50+50], mar = c(0,0,0,0), 
         cex.lab=0.7, type = "upper" , diag=FALSE)


################################### SECTION 3: PREPARE THE DATA  ###########################################

#3.3. Data splitting 

# creating the test set - tweets happiness score, handle and hashtag count

tweet1 <- "Go for a day or mix and match to see all your favourite artists! 1-day @Sonoma_Harvest  #SonomaHarvest Music Festival
tickets go on sale today at 10am PDT!. See your favourite artists including @BrunoMars, @Imaginedragons, @Mylie_Cyrus, @foals, @thegreatkhalid
View lineups + buy tickets >>bit.ly/SHMF19Tix"

tweet2 <- " One weekend isn’t enough
We wanted more unforgettable music moments in wine country, so we created @Sonoma_Harvest #SonomaHarvest. Celebrate and smile with us! #celebration
Tickets ON SALE NOW! Snag them for your crew before we sell out: >>bit.ly/SHMF19Tix "


tweet_words <- strsplit(tweet, split = " ")
tweet_words_vector <- unlist(tweet_words)
tweet_words_df <- data.frame("word" = tweet_words_vector)
tweet_merged <- join(tweet_words_df , happiness_dictionary_happs, by = "word", type = 'inner')
tweet1_score <- mean(tweet_merged$happs) # Score: 
tweet1_score

str_count(tweet1, "@")
str_count(tweet1, "#")


# The second tweet: 
# "One weekend isn’t enough .We wanted more unforgettable music moments in wine country, so we created @Sonoma_Harvest. Celebrate and smile with us! Tickets ON SALE NOW! Snag them for your crew before we sell out: >>bit.ly/SHMF19Tix "
tweet2 <- " One weekend isn’t enough
We wanted more unforgettable music moments in wine country, so we created @Sonoma_Harvest #SonomaHarvest. Celebrate and smile with us! #celebration
Tickets ON SALE NOW! Snag them for your crew before we sell out: >>bit.ly/SHMF19Tix "
tweet_words <- strsplit(tweet, split = " ")
tweet_words_vector <- unlist(tweet_words)
tweet_words_df <- data.frame("word" = tweet_words_vector)
tweet_merged <- join(tweet_words_df , happiness_dictionary_happs, by = "word", type = 'inner')
tweet2_score <- mean(tweet_merged$happs) # Score: 
tweet2_score


str_count(tweet2, "@")
str_count(tweet2, "#")

