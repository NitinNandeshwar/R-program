# Read file
A <- read.csv(file.choose())
str(A)

# Build Corpus
library(tm)
A_text <- iconv(A$text,to='UTF-8')
head(A_text)

A_text <- Corpus(VectorSource(A_text))
inspect(A_text[1:5])

# Cleaning

A_clean <- tm_map(A_text,tolower)
inspect(A_clean[1:5])

A_clean <- tm_map(A_clean,removePunctuation)
inspect(A_clean[1:5])

A_clean <- tm_map(A_clean,removeNumbers)
inspect(A_clean[1:5])

A_clean_stopwords <- tm_map(A_clean,removeWords,stopwords('english'))
inspect(A_clean_stopwords[1:5])

A_clean_stopwords <- tm_map(A_clean_stopwords,
                            gsub,
                            pattern='years',
                            replacement='year')

A_clean_stopwords <- tm_map(A_clean_stopwords,
                            gsub,
                            pattern=c('fires','wildfire'),
                            replacement='fire')

A_clean_stopwords <- tm_map(A_clean_stopwords,stripWhitespace)
inspect(A_clean_stopwords[1:5])

#Term document matrix
tdm <- TermDocumentMatrix(A_clean_stopwords)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]

# Bar Plot
w <- rowSums(tdm)

w <- subset(w,w>=40)
hist(w,col=rainbow(50))

barplot(w,
        las=2,
        col=rainbow(50))

# world Cloud
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(123)
wordcloud(words = names(w),
          freq = w,
          max.words = 200,
          random.order = F,
          min.freq = 10,
          colors = brewer.pal(8,'Dark2'),
          scale = c(3,0.2),
          rot.per = 0.1)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,
           size = 0.4,
           shape = 'Circle',
           rotateRatio = 0.1,
           minSize = 1)

#Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

# Read file
#apple <- read.csv(file.choose())
#tweets <- iconv(apple$text,to='UTF-8')

#obtain sentiment scores
s <- get_nrc_sentiment(A_clean_stopwords)
head(s)

# Bar plot
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        main='Sentiment scores for Diaster')
