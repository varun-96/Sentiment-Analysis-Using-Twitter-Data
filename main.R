library(NLP)
library(tm)
library(plyr)
library(ggplot2)
library(e1071)


#Unsupervised Analysis
set.seed(123)
reuters <- read.table("r8-train-all-terms.txt", header = F, sep = '\t')

reut <- reuters[which(reuters$V1 %in% c("trade","crude","money-fx")), ]
corpus <- Corpus(VectorSource(reut$V2))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

mat <- DocumentTermMatrix(corpus)

mat4 <- weightTfIdf(mat)
mat4 <- as.matrix(mat4)

#calculating euclidean distance

norm_euc <- function(m)
  m/apply(m,1,function(x) sum(x^2)^0.5)

mat_norm <- norm_euc(mat4)

#applyind K-means algorithm

k<- 3
kmeanalgo <- kmeans(mat_norm, k)

result <- data.frame('actual' = reut$V1, 'predicted' = kmeanalgo$cluster)
result <- result[order(result[,1]),]
result$counter = 1
resultagg <- aggregate(counter ~ actual + predicted, data = result, FUN = sum)

ggplot(data = resultagg, aes(x = actual, y = predicted, size = counter)) + geom_point()

#LDA
ldaalgo  <- LDA(mat,k)

x <- topics(ldaalgo)
new.df <- data.frame('response' = names(x), 'topic' = x, row.names = NULL)


