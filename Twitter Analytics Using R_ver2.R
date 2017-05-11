#Tutorial: Sentiment Analysis of Airlines Using the syuzhet Package and Twitter
setwd("C:\\Users\\tom.trinh\\Desktop\\R\\tweets mining")

#Step 1: Load the tweets and load the relevant packages
library(foreign)
library(syuzhet)
library(lubridate)
library(plyr)
library(ggplot2)
library(tm)
library(wordcloud)

# get the data for the tweets
pw <- read.csv("PW_tweets.csv",stringsAsFactors = F)
stripe <- read.csv("stripe_tweets.csv",stringsAsFactors = F)

pw$company <- "Paymentwall"
stripe$company <- "Stripe"

tweets <- rbind(pw,stripe)
tweets$X <- NULL
names(tweets) <- c("Text","Company")
#Step 2: Do Sentiment Scoring using the syuzhet package
# function to get various sentiment scores, using the syuzhet package
scoreSentiment = function(tab)
{
        tab$syuzhet = get_sentiment(tab$Text, method="syuzhet")
        tab$bing = get_sentiment(tab$Text, method="bing")
        tab$afinn = get_sentiment(tab$Text, method="afinn")
        tab$nrc = get_sentiment(tab$Text, method="nrc")
        emotions = get_nrc_sentiment(tab$Text)
        n = names(emotions)
        for (nn in n) tab[, nn] = emotions[nn]
        return(tab)
}

# get the sentiment scores for the tweets
tweets = scoreSentiment(tweets)
tweets = tweets[tweets$TimeStamp < as.Date('19-04-2017', format = '%d-%m-%Y'),]

# function to find the week in which a date occurs
round_weeks <- function(x)
{
        require(data.table)
        dt = data.table(i = 1:length(x), day = x, weekday = weekdays(x))
        offset = data.table(weekday = c('Sunday', 'Monday', 'Tuesday', 'Wednesday',
                                        'Thursday', 'Friday', 'Saturday'),
                            offset = -(0:6))
        dt = merge(dt, offset, by="weekday")
        dt[ , day_adj := day + offset]
        setkey(dt, i)
        return(dt[ , day_adj])
}
# get daily summaries of the results
daily = ddply(tweets, ~ Airline + TimeStamp, summarize, num_tweets = length(positive), ave_sentiment = mean(bing),
              ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))

# plot the daily sentiment
ggplot(daily, aes(x=TimeStamp, y=ave_sentiment, colour=Airline)) + geom_line() +
        ggtitle("Airline Sentiment") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')
# get weekly summaries of the results
weekly = ddply(tweets, ~ Airline + week, summarize, num_tweets = length(positive), ave_sentiment = mean(bing),
               ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))

# plot the weekly sentiment
ggplot(weekly, aes(x=week, y=ave_sentiment, colour=Airline)) + geom_line() +
        ggtitle("Airline Sentiment") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')

# compare the sentiment for across the algorithms
algorithms = tweets[rep(1, nrow(tweets) * 4), c("week", "syuzhet", "Airline", "Airline")]
names(algorithms) = c("TimeStamp", "Sentiment", "Algorithm", "Airline")
algorithms$Algorithm = "syuzhet"
algorithms[seq_len(nrow(tweets)), c("TimeStamp", "Sentiment", "Airline")] = tweets[,c("TimeStamp", "syuzhet", "Airline")]
algorithms[nrow(tweets) + seq_len(nrow(tweets)), c("TimeStamp", "Sentiment", "Airline")] = tweets[,c("TimeStamp", "bing", "Airline")]
algorithms$Algorithm[nrow(tweets) + seq_len(nrow(tweets))] = "bing"
algorithms[2 * nrow(tweets) + seq_len(nrow(tweets)), c("TimeStamp", "Sentiment", "Airline")] = tweets[,c("TimeStamp", "afinn", "Airline")]
algorithms$Algorithm[2 * nrow(tweets) + seq_len(nrow(tweets))] = "afinn"
algorithms[3 * nrow(tweets) + seq_len(nrow(tweets)), c("TimeStamp", "Sentiment", "Airline")] = tweets[,c("TimeStamp", "nrc", "Airline")]
algorithms$Algorithm[3 * nrow(tweets) + seq_len(nrow(tweets))] = "nrc"

# get the algorithm averages for each airline
averages = ddply(algorithms, ~ Airline + Algorithm, summarize, ave_sentiment = mean(Sentiment))
averages$ranking = 1
for (alg in c("syuzhet", "bing", "afinn", "nrc")) averages$ranking[averages$Algorithm == alg] = 5 - rank(averages$ave_sentiment[averages$Algorithm == alg])
averages = averages[order(averages$Airline, averages$Algorithm), ]

ggplot(weekly, aes(x=week, y=ave_negative, colour=Airline)) + geom_line() +
        ggtitle("Airline Sentiment (Positive Only)") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')

ggplot(weekly, aes(x=week, y=ave_positive, colour=Airline)) + geom_line() +
        ggtitle("Airline Sentiment (Negative Only)") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')

ggplot(weekly, aes(x=week, y=ave_anger, colour=Airline)) + geom_line() +
        ggtitle("Airline Sentiment (Anger Only)") + xlab("Date") + ylab("Sentiment") + scale_x_date(date_labels = '%d-%b-%y')

# function to make the text suitable for analysis
clean.text = function(x)
{
        # tolower
        x = tolower(x)
        # remove rt
        x = gsub("rt", "", x)
        # remove at
        x = gsub("@\\w+", "", x)
        # remove punctuation
        x = gsub("[[:punct:]]", "", x)
        # remove numbers
        x = gsub("[[:digit:]]", "", x)
        # remove links http
        x = gsub("http\\w+", "", x)
        # remove tabs
        x = gsub("[ |\t]{2,}", "", x)
        # remove blank spaces at the beginning
        x = gsub("^ ", "", x)
        # remove blank spaces at the end
        x = gsub(" $", "", x)
        return(x)
}

# emotion analysis: anger, anticipation, disgust, fear, joy, sadness, surprise, trust
# put everything in a single vector
all = c(
        paste(tweets$Text[tweets$anger > 0], collapse=" "),
        paste(tweets$Text[tweets$anticipation > 0], collapse=" "),
        paste(tweets$Text[tweets$disgust > 0], collapse=" "),
        paste(tweets$Text[tweets$fear > 0], collapse=" "),
        paste(tweets$Text[tweets$joy > 0], collapse=" "),
        paste(tweets$Text[tweets$sadness > 0], collapse=" "),
        paste(tweets$Text[tweets$surprise > 0], collapse=" "),
        paste(tweets$Text[tweets$trust > 0], collapse=" ")
)
# clean the text
all = clean.text(all)
# remove stop-words
# adding extra domain specific stop words
all = removeWords(all, c(stopwords("english"), 'singapore', 'singaporeair',
                         'emirates', 'united', 'airlines', 'unitedairlines',
                         'cathay', 'pacific', 'cathaypacific', 'airline',
                         'airlinesunited', 'emiratesemirates', 'pacifics'))
#
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
#
# add column names
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
#
# Plot comparison wordcloud
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, 'Emotion Comparison Word Cloud')
comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1.5, max.words=250)


# get the user summaries of the results
users = ddply(tweets, ~ Airline + UserName, summarize, num_tweets = length(positive), ave_sentiment = mean(bing),
              ave_negative = mean(negative), ave_positive = mean(positive), ave_anger = mean(anger))
sizeSentiment = ddply(users, ~ num_tweets, summarize, ave_sentiment = mean(ave_sentiment),
                      ave_negative = mean(ave_negative), ave_positive = mean(ave_positive), ave_anger = mean(ave_anger))
sizeSentiment$num_tweets = as.numeric(sizeSentiment$num_tweets)

# plot users positive versus negative with bubble plot
cutoff = sort(users$num_tweets, decreasing = TRUE)[100]
ggplot(users[users$num_tweets > cutoff,], aes(x = ave_positive, y = ave_negative, size = num_tweets, fill = Airline)) +
        geom_point(shape = 21) +
        ggtitle("100 Most Prolific Tweeters About Airlines") +
        labs(x = "Positive Sentiment", y = "Negative Sentiment")
#
ggplot(sizeSentiment, aes(x = num_tweets, y = ave_sentiment)) + geom_point() + stat_smooth(method = "loess", size = 1, span = 0.35) +
        ggtitle("Number of Tweets versus Sentiment") + scale_x_log10() +
        labs(x = "Positive Sentiment", y = "Negative Sentiment")

# Join texts in a vector for each company
txt1 = paste(tweets$Text[tweets$Airline == 'United'], collapse=" ")
txt2 = paste(tweets$Text[tweets$Airline == 'SingaporeAir'], collapse=" ")
txt3 = paste(tweets$Text[tweets$Airline == 'Emirates'], collapse=" ")
txt4 = paste(tweets$Text[tweets$Airline == 'Cathay Pacific'], collapse=" ")
#
# put everything in a single vector
all = c(clean.text(txt1), clean.text(txt2), clean.text(txt3), clean.text(txt4))
#
# remove stop-words
# adding extra domain specific stop words
all = removeWords(all, c(stopwords("english"), 'singapore', 'singaporeair',
                         'emirates', 'united', 'airlines', 'unitedairlines',
                         'cathay', 'pacific', 'cathaypacific', 'airline',
                         'airlinesunited', 'emiratesemirates', 'pacifics'))
#
# create corpus
corpus = Corpus(VectorSource(all))
#
# create term-document matrix
tdm = TermDocumentMatrix(corpus)
#
# convert as matrix
tdm = as.matrix(tdm)
#
# add column names
colnames(tdm) = c('United', 'Singapore Air', 'Emirates', 'Cathay Pacific')
#
# Plot comparison wordcloud
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, 'Word Comparison by Airline')
comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"),
                 title.size=1.5, max.words=250)
#
# Plot commonality cloud
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, 'Word Commonality by Airline')
commonality.cloud(tdm, random.order=FALSE,
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5, max.words=250)