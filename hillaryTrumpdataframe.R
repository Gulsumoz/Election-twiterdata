
pos = readLines("positive-words.txt")
neg = readLines("negative-words.txt")
library("stringr")
library("plyr")
#calculating the score of each tweet
score.sentiment = function(sentences, pos.words, neg.words,
                           .progress='none')
{
  #Parameters
  #- sentences: vector of text to score
  #- pos.words: vector of words of postive sentiment
  #- neg.words: vector of words of negative sentiment
  #- .progress: passed to laply() to control of progress bar
  
  #- create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   #- remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   #- remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   #- remove digits
                   sentence = gsub('\\d+', '', sentence)
                   #- define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     #- create missing value
                     y = NA
                     #- tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     #- if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # - result
                     return(y)
                   }
                   #- use tryTolower with sapply
                   sentence = sapply(sentence, tryTolower)
                   
                   #- split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   #- compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   #- get the position of the matched term or NA
                   #- we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   #- final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                   
                 }
                 , pos.words, neg.words, .progress=.progress )
  
  #- data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

#convert tweetslist to array by using sapply.
hillary_txt = sapply(hillarytweets, function(x) x$getText())
trump_txt = sapply(trumptweets, function(x) x$getText())
#how many tweets of each candidate
nd = c(length(hillary_txt), length(trump_txt))
#join the text
candidates = c(hillary_txt, trump_txt)
#Apply score.sentiment function  and calculate  results
scores = score.sentiment(candidates, pos, neg, .progress='text')

#add variables to data frame
scores$candidates = factor(rep(c("Hillary", "Trump"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)
head(scores)
scores$pos = as.numeric(scores$score >0)
scores$neg = as.numeric(scores$score <0)
scores$neut = as.numeric(scores$score ==0 )
head(scores)
#boxplot 
boxplot(score~candidates, data=scores)
#labeling the data 
combinednew$HP<-(candidates=="Hillary" & score>=0) || (candidates=="Trump" & score<0)
combinednew$TP<-candidates=="Trump"&  score>=0 || (candidates=="Hillary"& score<0)
combinedntr$HP<-(candidates=="Hillary"& score>0) || (candidates=="Trump"& score<0)
combinedntr$TP<-(candidates=="Trump" & score>0 ) || (candidates=="Hillary"&  score<0)
combinednew$NT<- score==0
save(combinednew,  file="combinednew.RData")
attach(combinednew)
#load train data
train<-read.csv("train_data.csv", header = T)
label<-read.csv("train_data_label.csv", header = T)
str(label)
str(train)
whldata<-data.frame(cbind(train, label))
str(whldata)
label<-as.factor(label)
head(whldata$label)
#load Test data
test<-read.csv("test_data.csv", header = T)
str(test)
tlabel<-read.csv("test_data_label.csv", header = T)
testdata<-cbind(test, tlabel)
str(testdata)
str(tlabel)
#
# random Forest model
#
library(randomForest)
partdata<-whldata[1:5000, ]
set.seed(71)
modelrf<- randomForest(partdata[, 145] ~ ., data=partdata, ntree=500, keep.forest=TRUE)
print(modelrf)
preiction <- predict(modelrf, partdata)
rndfaccuracy<-table(preiction, testdata[, 145])
confusionMatrix(rndfaccuracy)
#
# Naive bayes Model 
#
Predmodel1<-naiveBayes(partdata[, 145]~., data=partdata)
pred <- predict(Predmodel1, testdata)
NaiveBacc<-table(pred, testdata[, 145])
confusionMatrix((NaiveBacc))
#
#KNN Model with k=1 
#
library(caret)
library(class)
Knnmodel2<-knn(partdata[, -145], testdata[, -145], partdata[, 145], k = 1)
k1acc<-table(Knnmodel2, testdata[,  145])
confusionMatrix(k1acc)
