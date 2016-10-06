library(twitteR)
#library(ROAuth)
library(data.table)
library(ggmap)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(SnowballC)
library(dplyr)

consumer_key <- '8V6t91hmH9j1WMDqbSICxmNxd'
consumer_secret <- 'hXVyQdHJx7hJFP3OsvSEIUb5r1162G9y49nLXUogKmBrUzQr5n'
access_token <- '363858843-PijDtvRucsA8BoRvl18xhsHHVe2MZhBep5DueVdF'
access_secret <- 'zpXDXliCuLmDQCLMLBpxRESqjGjhsdG9UNDaFmIKz3AZS'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

########################
### GET AT FOLLOWERS ###
########################

at_user <- getUser("AutoTrader_UK")

followers <- at_user$getFollowers(n = 1000)

# turn followers list into a data frame
followers_df <- rbindlist(lapply(followers, as.data.frame))

# remove proteced or verified followers or with blank locations
followers_df <- followers_df[location!="" | protected == FALSE | verified == FALSE | lang == "en"]

write.csv(followers_df, "ATD_twitter_followers.csv")


### INFER GOECODE FROM TEXT LOCATIONS USING GGMAP ###
followers_df$location <- gsub("%", " ",followers_df$location)

# execute this function over the entire location vector instead of looping through it
geocode_apply <- function(x) { geocode(x, source = "google", output = "all") }
geocde_results <- sapply(followers_df$location, geocode_apply, simplify = FALSE)

# remove unparsed locations (status != ok)
condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results <- geocode_results[condition_a]

# remove "any match" locations (match results > 1)
condition_b <- lapply(geocode_results, lapply, length)
condition_b2 <- sapply(condition_b, function(x) x["results"]=="1")
geocode_results <- geocode_results[condition_b2]


# get timeline per follower
followers_tweets <- sapply(followers_df[1]$id, function(x) { 
  userTimeline(user = x, n = 3200)
  Sys.sleep(30)
})

followers_tweets <- sapply(followers_tweets, function(x) twListToDF(x))



#######################################
# Fetch tweets with AT related keywords
#######################################
getCurRateLimitInfo()

collate = function(data, size) {
  .groups = if(is.data.frame(data)) function(d) seq_len(nrow(d)) else seq_along
  split(data, ceiling(.groups(data) / size))
}

keywords <- '#roadtrip OR #driving OR #testdrive OR #automotive OR #motoring OR #automobile OR #car OR #cars OR #vintagecars OR #newcar OR "new car" OR #caroftheday OR #carstagram OR #cargram OR #instacar OR #carsofinstagram'
tweets <- searchTwitter(keywords, n = 20000, geocode='53.81982,-2.406348,500km', lang = "en", retryOnRateLimit = 180)

# Filter out tweets from verified accounts
users <- sapply(tweets, function(t) { 
  t$screenName # Get authors screennames
}) %>% 
  unique() %>%
  collate(50) %>% # Split into lists of size 50
  sapply(function(u) {
    users <- lookupUsers(u, retryOnRateLimit = 180) # Lookup users for each list
  }) %>% 
  unlist() %>% # Flatten
  twListToDF() %>% # Convert to data frame
  filter(verified == FALSE) # Filter out verified users

tweets_df <- twListToDF(tweets)
tweets_df <- tweets_df[tweets_df$screenName %in% users$screenName, ]
write.csv(tweets_df, "ATD_tweets.csv")

###################
# Text Analytics
###################
load_data <- function(path) { 
  files <- list.files(path, pattern = '[0-9]\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_tweets <- load_data("ATD_TWEETS/") %>%
  .[!duplicated(.[,2]),]

tweet_text <- sapply(all_tweets$text, as.character)

# Pre-processing
tweet_text <- iconv(tweet_text, to = "ASCII", sub = " ") #Convert to basic ASCII text to avoid silly characters
tweet_text <- tolower(tweet_text)
tweet_text <- gsub("rt", " ", tweet_text) # Remove the "RT" (retweet) so duplicates are duplicates
tweet_text <- gsub("http\\S+", "", tweet_text) #Remove URLs
tweet_text <- gsub("[[:punct:]]", " ", tweet_text) #Remove punctuation
tweet_text <- gsub("[ |\t]{2,}", " ", tweet_text) #Remove tabs
tweet_text <- gsub("amp", " ", tweet_text) #"&" is "&amp" in HTML, so after punctuation removed ...
tweet_text <- unique(tweet_text)

tweet_corpus <- Corpus(VectorSource(tweet_text)) %>% # Create our tweets text corpus
  tm_map(removeWords, c(stopwords("en"), "car", "cars", "newcar", "new", "vehicle", "van", "roadtrip", "driving", "drive", "driver", "automotive", "motoring", "automobile", "ebay", "amazonuk", "job", "carsforsale", "can", "got", "get", "getting", "just")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(stemDocument) %>%
  tm_map(PlainTextDocument) #Convert the corpus to a plain text document.

wordcloud(tweet_corpus, scale=c(5,0.5), max.words=150, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

####################
# Topic Modeling
####################

# Get the lengths and make sure we only create a DTM for tweets with some actual content
dtm <- DocumentTermMatrix(tweet_corpus)
#doc_lengths <- apply(dtm, 1, sum) #Find the sum of words in each Document
doc_lengths <- rowSums(as.matrix(dtm))
dtm <- dtm[doc_lengths > 0, ] #Remove all docs without words
model <- LDA(dtm, 10) #Test a simple model



# Now for some topics
SEED = sample(1:1000000, 1) #Pick a random seed for replication
k = 5  #Number of topics

models <- list(
  CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000, thin = 100, iter = 1000))
)

# Top 10 terms of each topic for each model
lapply(models, terms, 10)

# matrix of tweet assignments to predominate topic on that tweet
# for each of the models, in case you wanted to categorize them
assignments <- sapply(models, topics)
