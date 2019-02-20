data=read.csv("C:/Users/imsau/Desktop/6th Sem/ML/ML_lab/Lab5(13-Feb)/sms_spam.csv")
summary(data)
str(data)

library(SnowballC)
library(ggplot2)
library(tm)
library(wordcloud)

corpus <- (VectorSource(data$text))
corpus <- Corpus(corpus)
summary(corpus)

for (i in 1:6)
  print (corpus[[i]]$content)

#Remove Punctuation
corpus1 <- tm_map(corpus, content_transformer(removePunctuation))
for (i in 1:6) print (corpus1[[i]]$content)

corpus2 <- tm_map(corpus1, content_transformer(removeWords), 
                 stopwords("english"))
for (i in 1:6) print (corpus2[[i]]$content)

corpus3 <- tm_map(corpus2, content_transformer(tolower))
for (i in 1:6) print (corpus3[[i]]$content)

corpus4 <- tm_map(corpus3, content_transformer(removeWords), 
                 stopwords("english"))
for (i in 1:6) print (corpus4[[i]]$content)

corpus5 <- tm_map(corpus4, stemDocument)
for (i in 1:6) print (corpus5[[i]]$content)

corpus6 <- tm_map(corpus5, stripWhitespace) 
for (i in 1:6) print (corpus6[[i]]$content)

corpus7 <- tm_map(corpus6, content_transformer(removeNumbers))
for (i in 1:6) print (corpus7[[i]]$content)

library(wordcloud)
wordcloud(corpus7, min.freq = 50, random.order = FALSE)

dtm=DocumentTermMatrix(corpus)
dtm2=DocumentTermMatrix(corpus7)


dtm2_train <- dtm2[1:4169, ]
dtm2_test  <- dtm2[4170:5559, ]


train_labels <- data[1:4169, ]$type
test_labels  <- data[4170:5559, ]$type
train_labels

prop.table(table(train_labels))
prop.table(table(test_labels))


wordcloud(corpus7, min.freq = 50, random.order = FALSE)

spam <- subset(data, type == "spam")
ham  <- subset(data, type == "ham")


wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))

wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
