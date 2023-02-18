Apple_Tweets <- read_excel("E:/College/DTU/Sem 2/202-Intro to Big Data System/Apple_Tweets.xlsx")
View(Apple_Tweets)
install.packages("tm")
install.packages("rJava")
install.packages("wordcloud")
install.packages("textir")
install.packages("Rweka")
install.packages("qdap")
install.packages("maptx")

library("tm")
library("rJava")
library("wordcloud")
library("textir")
library("Rweka")
library("qdap")
library("maptx")

# Built Corpus
c1 <- iconv(Apple_Tweets$Tweet, to = "utf-8")
c1 <- Corpus(VectorSource(c1))
inspect(c1[1:5])

# clean text
tm_map(c1, tolower) -> c1
tm_map(c1, removePunctuation) -> c1
tm_map(c1, removeNumbers) -> c1
tm_map(c1, removeWords, stopwords('english')) -> c2
c2 <- tm_map(c2, removeWords, c('apple'))
#removeURL <- function(x) gsub("http[[:alnum]]*","",x)
#c2 <- tm_map(c2, content_transformer(removeURL))
c2 <- tm_map(c2, stripWhitespace)
inspect(c2[1:5])

# term document matrix
tdm <- TermDocumentMatrix(c2)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar Plot
w <- rowSums(tdm)
w
w <- subset(w, w>=20)
w
barplot(w,
        las = 2,
        col = rainbow(50))

# Word cloud
word_cloud <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(word_cloud),
          freq = word_cloud,
          max.words = 150,
          random.order = FALSE,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.7)


# library 
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# sentiment analysis
tweets <- iconv(Apple_Tweets$Tweet, to = "utf-8")
score <- get_nrc_sentiment(tweets)
head(score)



tweets[1]
get_nrc_sentiment('customer')
get_nrc_sentiment('received')

# Bar Plot
barplot(colSums(score),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Score for Apple Tweets')

barplot(
        sort((table(score))), 
        horiz = TRUE, 
        cex.names = 0.7, 
        las = 1, 
        main = "Emotions in Sample text", xlab="Percentage"
)

syuzhet_vector <- get_sentiment(tweets, method="syuzhet")
head(syuzhet_vector)

bing_vector <- get_sentiment(tweets, method = "bing")
head(bing_vector)

afinn_vector <- get_sentiment(tweets, method = "afinn")
head(afinn_vector)

nrc_vector <- get_sentiment(tweets, method = "nrc", lang = "english")
head(nrc_vector)

rbind(
        sign(head(syuzhet_vector)),
        sign(head(bing_vector)),
        sign(head(afinn_vector)),
        sign(head(nrc_vector))
)

sum(syuzhet_vector)

mean(syuzhet_vector)

summary(syuzhet_vector)

s_v <- get_sentences(tweets)
s_v_sentiment <- get_sentiment(s_v)
plot(
        s_v_sentiment, 
        type="l", 
        main="Plot Trajectory", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence"
)

percent_vals <- get_percentage_values(syuzhet_vector, bins = 10)
plot(
        tweets, 
        type="l", 
        main="Joyce's Portrait Using Percentage-Based Means", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence", 
        col="red"
)

ft_values <- get_transformed_values(
        syuzhet_vector, 
        low_pass_size = 3, 
        x_reverse_len = 100,
        padding_factor = 2,
        scale_vals = TRUE,
        scale_range = FALSE
)

plot(
        ft_values, 
        type ="l", 
        main ="Joyce's Portrait using Transformed Values", 
        xlab = "Narrative Time", 
        ylab = "Emotional Valence", 
        col = "red"
)

dct_values <- get_dct_transform(
        syuzhet_vector, 
        low_pass_size = 5, 
        x_reverse_len = 100,
        scale_vals = F,
        scale_range = T
)

plot(
        dct_values, 
        type ="l", 
        main ="Joyce's Portrait using Transformed Values", 
        xlab = "Narrative Time", 
        ylab = "Emotional Valence", 
        col = "red"
)

path_to_a_text_file <- system.file("extdata", "bovary.txt", package = "syuzhet")
bovary <- get_text_as_string(path_to_a_text_file)
bovary_v <- get_sentences(bovary)
bovary_sentiment <- get_sentiment(bovary_v)

simple_plot(bovary_sentiment)

