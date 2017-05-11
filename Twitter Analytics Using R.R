setwd("C:\\Users\\tom.trinh\\Desktop\\R\\tweets mining")

library("twitteR")
library("ROAuth")
library("tm")
library("wordcloud")

# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object "cred" that will save the authenticated object that we can use for later sessions
cred <- OAuthFactory$new(consumerKey='xxxx',
                         consumerSecret='xxxxx',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter authentication.Rdata")

#EXTRACT TWEETS
load("twitter authentication.Rdata")

registerTwitterOAuth(cred)
setup_twitter_oauth(consumer_key = "8ddhn4x2Bzy9J0TCPEhwgCdOc",
                    consumer_secret = "VrkyEKAN5CUtJNLWgFybnuzoJgVZ9CpDz6VSiz0dFFIhyZTiEL", 
                    access_token= "94469587-qL3MpZKXbq99dOoAjBguSrTnK1hvxCXdoFI5Kelb2", 
                    access_secret="jqmLYW35A5oTE1oOJgo7YPWGOO8g9A93rY2tJEOlDwsHl")

tweets <- searchTwitter("paysafecard", n=100, lang="en")
tweets
tweets.text <- sapply(tweets, function(x) x$getText())
x <- as.data.frame(tweets.text)
write.csv(x,file = "PW.tweets")

tweets <- searchTwitter("stripe", n=100, lang="en")
tweets.text <- sapply(tweets, function(x) x$getText())
x <- as.data.frame(tweets.text)
write.csv(x,file = "stripe.tweets")
#CLEAN UP TEXT
#convert all text to lower case
tweets.text <- tolower(tweets.text)

# Replace blank space ("rt")
tweets.text <- gsub("rt", "", tweets.text)

# Replace @UserName
tweets.text <- gsub("@\\w+", "", tweets.text)

# Remove punctuation
tweets.text <- gsub("[[:punct:]]", "", tweets.text)

# Remove links
tweets.text <- gsub("http\\w+", "", tweets.text)

# Remove tabs
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)

# Remove blank spaces at the beginning
tweets.text <- gsub("^ ", "", tweets.text)

# Remove blank spaces at the end
tweets.text <- gsub(" $", "", tweets.text)

#REMOVE STOP WORDS

#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))

#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

# GENERATE WORD CLOUD
#generate wordcloud
wordcloud(tweets.text.corpus,min.freq = 1, scale=c(3,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)
