library(twitteR)
library(httr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(stringr)
library(lattice)

key="xxxxxxxxxxxxxxxxx"
secret="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
mytoken = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
mysecret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"


setup_twitter_oauth(key, secret, mytoken, mysecret)

appletweets <- searchTwitter("#iPhones", n=5000, lang = 'en')
samsungtweets <- searchTwitter("#Samsung+Galaxy", n=5000, lang = 'en')
oneplustweets <- searchTwitter("#OnePlus", n=5000, lang = 'en')
nokiatweets <- searchTwitter("#Nokia",n=5000, lang = 'en')

#class(appletweets)
#tail(appletweets)
#head(appletweets)
#length(appletweets)

appletext <- sapply(appletweets,function(x) x$getText())
samsungtext <- sapply(samsungtweets,function(x) x$getText())
oneplustext <- sapply(oneplustweets,function(x) x$getText())
nokiatext <- sapply(nokiatweets,function(x) x$getText())

#appletweetcorpus <- Corpus(VectorSource(appletext))
appletext <- gsub("[[:punct:]]","",appletext)
appletweetcorpus <- Corpus(VectorSource(appletext)) %>% 
                      tm_map(removePunctuation) %>% 
                      tm_map(removeNumbers) %>% 
                      tm_map(tolower) %>% 
                      tm_map(function(x)removeWords(x,c('the','this',stopwords())))


samsungtext <- gsub("[[:punct:]]","",samsungtext)
samsungtweetcorpus <- Corpus(VectorSource(samsungtext)) %>% 
                      tm_map(removePunctuation) %>% 
                      tm_map(removeNumbers) %>% 
                      tm_map(tolower) %>% 
                      tm_map(function(x)removeWords(x,c('the','this',stopwords())))


                     
oneplustext <- gsub("[[:punct:]]","",oneplustext)
oneplustweetcorpus <- Corpus(VectorSource(oneplustext)) %>% 
                      tm_map(removePunctuation) %>% 
                      tm_map(removeNumbers) %>% 
                      tm_map(tolower) %>% 
                      tm_map(function(x)removeWords(x,c('the','this',stopwords())))


nokiatext <- gsub("[[:punct:]]","",nokiatext)
nokiatweetcorpus <- Corpus(VectorSource(nokiatext)) %>% 
                      tm_map(removePunctuation) %>% 
                      tm_map(removeNumbers) %>% 
                      tm_map(tolower) %>% 
                      tm_map(function(x)removeWords(x,c('the','this',stopwords())))





col<- brewer.pal(5,"Set1")

par(mfrow=c(2,2))
wordcloud(appletweetcorpus,min.freq = 5,max.words = 45,random.order = F,random.color = T,rot.per = 0.5,colors = col,scale = c(5,1))
wordcloud(samsungtweetcorpus,min.freq = 5,max.words = 45,random.order = F,random.color = T,rot.per = 0.5,colors = col,scale = c(5,1))
wordcloud(oneplustweetcorpus,min.freq = 5,max.words = 45,random.order = F,random.color = T,rot.per = 0.5,colors = col,scale = c(5,1))
wordcloud(nokiatweetcorpus,min.freq = 5,max.words = 45,random.order = F,random.color = T,rot.per = 0.5,colors = col,scale = c(5,1))
par(mfrow=c(1,1))

appletdm <- TermDocumentMatrix(appletweetcorpus)
samsungtdm <- TermDocumentMatrix(samsungtweetcorpus)
oneplustdm <- TermDocumentMatrix(oneplustweetcorpus)
nokiatdm <- TermDocumentMatrix(nokiatweetcorpus)

findFreqTerms(appletdm,lowfreq = 200)
findFreqTerms(samsungtdm,lowfreq = 150)
findFreqTerms(oneplustdm,lowfreq = 150)
findFreqTerms(nokiatdm,lowfreq = 105)

#findAssocs(applesdtdm,"battery",0.6)
appletdm
samsungtdm
oneplustdm
nokiatdm

npappletdm <- removeSparseTerms(appletdm, 0.9)
npsamsungtdm <- removeSparseTerms(samsungtdm, 0.9)
nponeplustdm <- removeSparseTerms(oneplustdm, 0.9)
npnokiatdm <- removeSparseTerms(nokiatdm, 0.9)

appletdmscale <- scale(npappletdm)
appletdmdist <- dist(appletdmscale,method='euclidean')
appletdmfit <- hclust(appletdmdist)

samsungtdmscale <- scale(npsamsungtdm)
samsungtdmdist <- dist(samsungtdmscale,method='euclidean')
samsungtdmfit <- hclust(samsungtdmdist)

oneplustdmscale <- scale(nponeplustdm)
oneplustdmdist <- dist(oneplustdmscale,method='euclidean')
oneplustdmfit <- hclust(oneplustdmdist)

nokiatdmscale <- scale(npnokiatdm)
nokiatdmdist <- dist(nokiatdmscale,method='euclidean')
nokiatdmfit <- hclust(nokiatdmdist)

#par(mai=c(1,1.2,1,0.5))
plot(appletdmfit, xlab="", sub="", col.main = "Grey",main = "Hierarchical Clustering for iPhones")
plot(samsungtdmfit, xlab="", sub="", col.main = "Blue",main = "Hierarchical Clustering for Samsung")
plot(oneplustdmfit, xlab="", sub="", col.main = "Red",main = "Hierarchical Clustering for One Plus")
plot(nokiatdmfit, xlab="", sub="", col.main = "Green",main = "Hierarchical Clustering for Nokia")

#cutree(applesdtdmfit,4)
#rect.hclust(applesdtdmfit, k=4, border = 'salmon')



pos = readLines("G:\\RProject\\twitter Sentiment Analysis\\Positive-Words.txt")
neg = readLines("G:\\RProject\\twitter Sentiment Analysis\\Negative-Words.txt")

sentiment.getScore = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation - using global substitute
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

# how many tweets of each company
companytweets_length = c(length(appletext), length(samsungtext), length(oneplustext), length(nokiatext))

# join texts
all_tweets = c(appletext, samsungtext,oneplustext,nokiatext) 

# apply function score.sentiment
scores = sentiment.getScore(all_tweets, pos, neg, .progress='text')

# add variables to data frame
scores$company = factor(rep(c("Apple", "Samsung", "One Plus", "Nokia"), companytweets_length))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
totalpos = sum(scores$very.pos)
totalneg = sum(scores$very.neg)

overall_score = round( 100 * totalpos / (totalpos + totalneg) )

boxplot(score ~ company, data = scores, col = c("blue","grey"))

histogram(data=scores, ~score|company, main="Sentiment Analysis of 4 Companies", xlab="", sub="Sentiment Score", col=c("blue","grey"))
