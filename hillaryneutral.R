library(twitteR)

for (i in c(1:2, 10000)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(neutext[i], 60))
}

library(tm)
library(NLP)
# build a corpus, and specify the source to be character vectors
hNCorpus <- Corpus(VectorSource(neutext))

# convert to lower case
# tm v0.6
hNCorpus <- tm_map(hNCorpus, content_transformer(tolower))

# tm v0.5-10
# myCorpus <- tm_map(myCorpus, tolower)
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
# tm v0.6
hNCorpus <- tm_map(hNCorpus, content_transformer(removeURL))

# tm v0.5-10
# myCorpus <- tm_map(myCorpus, removeURL)
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
hNCorpus <- tm_map(hNCorpus, content_transformer(removeNumPunct))

# remove punctuation
hNCorpus <- tm_map(hNCorpus, removePunctuation)

# remove numbers
hNCorpus <- tm_map(hNCorpus, removeNumbers)

# add two extra stop words: "available" and "via"
Stopwords <- c(stopwords('english'), "available", "via")
# remove "r" and "big" from stopwords
Stopwords <- setdiff(Stopwords, c("r", "big"))
# remove stopwords from corpus
hNCorpus <- tm_map(hNCorpus, removeWords, Stopwords)

# remove extra whitespace
hNCorpus <- tm_map(hNCorpus, stripWhitespace)

# keep a copy of corpus to use later as a dictionary for stem completion
hNCorpusCopy <- hNCorpus

# stem words
hNCorpus <- tm_map(hNCorpus, stemDocument)
inspect(hCorpus[1:5])

# The code below is used for to make text fit for paper width
for (i in c(1:2, 1000)) {
  cat(paste0("[", i, "] "))
  writeLines(strwrap(as.character(vCorpus[[i]]), 60))
}
# tm v0.5-10
# myCorpus <- tm_map(myCorpus, stemCompletion)
# tm v0.6
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
hNCorpus <- lapply(hNCorpus, stemCompletion2, dictionary=hNCorpusCopy)
hNCorpus <- Corpus(VectorSource(hNCorpus))

# count frequency of "verizon"
hillaryCases <- lapply(hCorpusCopy,
                       function(x) { grep(as.character(x), pattern = "\\<hillary")} )
sum(unlist(hillaryCases))
trumpCases <- lapply(tCorpusCopy,
                     function(x) { grep(as.character(x), pattern = "\\<trump")} )
sum(unlist(trumpCases))

# count frequency of "miner"

tdmhN <- TermDocumentMatrix(hNCorpus,
                           control = list(wordLengths = c(1, Inf)))
tdmhN

## <<TermDocumentMatrix (terms: 822, documents: 320)>>
## Non-/sparse entries: 2460/260580
## Sparsity : 99%
## Maximal term length: 27
## Weighting : term frequency (tf)

# inspect frequent words
(freq.terms <- findFreqTerms(tdmhN, lowfreq = 15))

term.freqhN <- rowSums(as.matrix(tdmhN))

term.freqhN <- subset(term.freqhN, term.freq >= 15)

dfhN<- data.frame(term = names(term.freqhN), freq = term.freqh)

install.packages("ggplot2")
library(ggplot2)
ggplot(dfh, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  +   xlab("Terms") + ylab("Count") + coord_flip()
# which words are associated with 'r'?
findAssocs(tdmhN, "hillary", 0.2)
mN <- as.matrix(tdmhN)
# calculate the frequency of words and sort it by frequency
word.freqhN<- sort(rowSums(mN), decreasing = T)
library(wordcloud)
require(RColorBrewer)
wordcloud(words = names(word.freqhN), freq = word.freqhN, min.freq = 80,
          random.order = F, colors = "red")
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  +   xlab("Terms") + ylab("Count") + coord_flip()
# which words are associated with 'r'?
findAssocs(tdmt, "trump", 0.2)
t <- as.matrix(tdmt)
# calculate the frequency of words and sort it by frequency
word.freqt <- sort(rowSums(t), decreasing = T)
library(wordcloud)
pal2 <- brewer.pal(6,"Dark2")
wordcloud(words = names(word.freqt), freq = word.freqt, min.freq = 80,
          random.order = F, colors = pal2)

wordcloud(words = names(word.freqt),  freq = word.freqt, min.freq = 80, 
          random.order = FALSE, colors=rev(colorRampPalette(brewer.pal(8,"Set1"))(32)[seq(12,32,6)]) )

library(wordcloud)


wordcloud(hNCorpus, min.freq = 80, random.order = FALSE, colors=rev(colorRampPalette(brewer.pal(9,"Dark2"))(32)[seq(8,32,6)]) )


