# This program reads in a text file and generates a word cloud of the most popular words that appear in the file

# The following four packages only need to be installed once - after that the libraries can be used to load them
# install.packages("tm")           # text mining
# install.packages("SnowballC")    # text stemming
# install.packages("wordcloud")    # word-cloud generator
# install.packages("RColorBrewer") # colour palettes

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Read in a text file
text <- readLines(file.choose())

# Create a corpus of all the words in the text
docs <- Corpus(VectorSource(text))
inspect(docs)


# Text Transformation - Convert certain characters to spaces
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Text Cleaning
# Convert the text to lowercase
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english and common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stopwords
# Specify your own stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Remove extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)


# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word=names(v),freq=v)
head(d, 10)


# Generate the word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))


# Find words that occur at least four times
findFreqTerms(dtm, lowfreq = 4)

# Association between terms
findAssocs(dtm, terms = "freedom", corlimit = 0.3)

head(d, 10)
# Plot word frquencies
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col = "lightblue", main = "Most Frequent Words", ylab = "Word frequencies")


