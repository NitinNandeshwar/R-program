# Read File
apple <- read.csv(file.choose())
str(apple)

# Build corpus
library(tm)
corpus <- iconv(apple$text,to="UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#clean text
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus,removeWords,stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset,removeWords,c('aapl','apple'))

cleanset <- tm_map(cleanset,
                   gsub,
                   pattern='stocks',
                   replacement='stock')

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot
w <- rowSums(tdm)
w <- subset(w,w>=25)
barplot(w,
        las=2,
        col=rainbow(50))

# Word Cloud
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(123)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.1),
          rot.per = 0.1)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.1,
           minSize = 1)

letterCloud(w,
            word = "A",
            size=2)

#Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

# Read file
apple <- read.csv(file.choose())
tweets <- iconv(apple$text,to='UTF-8')

#obtain sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]

get_nrc_sentiment('delay')

# Bar plot
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        main='Sentiment scores for apple')
