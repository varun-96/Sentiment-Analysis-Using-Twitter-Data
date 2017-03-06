library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(NLP)
library(tm)
library(ggplot2)
library(topicmodels)
library(devtools)

#Sentiment Analysis libraries
library(rjson)
library(plyr)
library(sentiment)


consumer_key <- 'YlDj917en0qwjvE0mytxxxxxx'
consumer_secret <- 'Rw8QzUL5JEe3qmVMv9VfG1iTofxf5yrTmOiuBBrTtBzzzzzzzz'
access_token <- '768486637240287232-sSWR62cLUGeFS48niHGcKOHzzzzzzz'
access_secret <- 'xmsfttGKSPA031uYKlvHOds0kmY2O8otARl801JKYzzzzzzz'

setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                    access_token = access_token, access_secret = access_secret)

  
BFCTweets <- searchTwitter("Virat", n = 15,lang = "en")

tweets = userTimeline("FCBarcelona", n = 500)
tweets.df = twListToDF(tweets)

mycorpus <- Corpus(VectorSource(tweets.df$text))
mycorpus <- tm_map(mycorpus, PlainTextDocument)

removeURL <- function(x) gsub("http[^[:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(removeURL))


removeNUM <- function(x) gsub('[^[:alpha:][:space:]]*','',x)
mycorpus <- tm_map(mycorpus, content_transformer(removeNUM))
mycorpus <- tm_map(mycorpus, tolower)
mycorpus <- tm_map(mycorpus, PlainTextDocument)


mycorpus <- tm_map(mycorpus, removeWords , stopwords(kind = "en"))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpusCopy = mycorpus

mycorpus <- tm_map(mycorpus, stemDocument)

stemCompletion2 <- function(x, dictionary){
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion2(x, dictionary =  dictionary)
  x <- paste(x, sep = "", collapse = " ")
  PlainTextDocument(stripWhitespace(x))
}

mycorpus = lapply(mycorpus, stemCompletion2, dictionary = mycorpusCopy)
mycorpus = Corpus(VectorSource(mycorpus))

#Term Document Matrix
tdm <- TermDocumentMatrix(mycorpus)
freq.terms = findFreqTerms(tdm, lowfreq = 20)
term.freq = rowSums(as.matrix(tdm))
term.freq = subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("terms") + ylab("Count") + coord_flip()


#WordCloud

pal = brewer.pal(8,"BuGn")[-(1:3)]
wordcloud(mycorpus, min.freq = 3, max.words = 100, random.order = F, colors = pal)

findAssocs(tdm, "messi", 0.2)
findAssocs(tdm, "suarez", 0.25)


#Topic modelling

dtm = as.DocumentTermMatrix(tdm)
rowTotal <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotal > 0,]
lda <- LDA(dtm.new, k=8)
term <- terms(lda, 7)
term <- apply(term,2, paste, collapse = ", ")

topics <- topics(lda)
tweets.df$rowTotal <- rowTotal
tweets.df <- tweets.df[tweets.df$rowTotal > 0,]
topics <- data.frame(Date = as.Date(tweets.df$created), Topic = topics)

ggplot(topics, aes(Date, fill = term[Topic])) + geom_density(position = "stack")

##Sentiment Analysis
sentiments = sentiment(tweets.df$text)
table(sentiments$polarity)

sentiments$score = 0
sentiments$score[sentiments$polarity == "positive"] = 1
sentiments$score[sentiments$polarity == "negative"] = -1
sentiments$date = as.Date(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

