# Part 1

#Load CSV
precorpus <- read.csv("~/Desktop/Third Semester/BA/Trump and Forbe 500/Part1.csv",header=TRUE, stringsAsFactors = FALSE)
dim(precorpus) # dim of file
names(precorpus)
str(precorpus)

#### Corpus mission and Core Value

# create a corpus of mission statement
require(quanteda)
newscorpus1<- corpus(precorpus$Mission,
                     docnames=precorpus$Document_ID,
                     docvar=data.frame(name=precorpus$Name,Subject= precorpus$Description))
#explore the corpus of mission
names(newscorpus1)   #to explore 
summary(newscorpus1)  #summary of corpus
head(newscorpus1)

# create a corpus of Core value
newscorpus2<- corpus(precorpus$Description,
                     docnames=precorpus$Document_ID,
                     docvar=data.frame(name=precorpus$Name,Subject= precorpus$Mission))
#explore the corpus of Core Value
names(newscorpus2)   #to explore 
summary(newscorpus2)  #summary of corpus
head(newscorpus2)

#### Clean corpus

#mission
newscorpus1<- toLower(newscorpus1, keepAcronyms = FALSE) 
cleancorpus1 <- tokenize(newscorpus1, 
                         removeNumbers=TRUE,  
                         removePunct = TRUE,
                         removeSeparators=TRUE,
                         removeTwitter=FALSE,
                         verbose=TRUE)
# core value
newscorpus2<- toLower(newscorpus2, keepAcronyms = FALSE) 
cleancorpus2 <- tokenize(newscorpus2, 
                         removeNumbers=TRUE,  
                         removePunct = TRUE,
                         removeSeparators=TRUE,
                         removeTwitter=FALSE,
                         verbose=TRUE)
### create DFM
dfm.mission<- dfm(cleancorpus1,
                  toLower = TRUE, 
                  ignoredFeatures =stopwords("english"),  # stopword
                  verbose=TRUE, 
                  stem=FALSE)

dfm.core<- dfm(cleancorpus2,
               toLower = TRUE, 
               ignoredFeatures =stopwords("english"),  # stopword
               verbose=TRUE, 
               stem=FALSE)
#### To display most frequent terms in dfm
topfeatures1<-topfeatures(dfm.mission, n=50)
topfeatures1  # words:can,s every,way

topfeatures2<-topfeatures(dfm.core, n=50)
topfeatures2  # words:s,make use

###to create a custom dictionary  list of stop words
swlist1 = c("can", "s", "every","way")  # remove those words
dfm_mission.stem<- dfm(cleancorpus1, toLower = TRUE, 
                       ignoredFeatures = c(swlist1, stopwords("english")), #put stopwords
                       verbose=TRUE, # show process
                       stem=TRUE) # roots of word
topfeatures1.stem<-topfeatures(dfm_mission.stem, n=50) # top 50 words
topfeatures1.stem

quartz()
plot(dfm_mission.stem)


swlist2 =c("make", "s", "use")  # remove those words
dfm_core.stem<- dfm(cleancorpus2, toLower = TRUE, 
                    ignoredFeatures = c(swlist2, stopwords("english")), #put stopwords
                    verbose=TRUE, # show process
                    stem=TRUE) #roots of word
topfeatures2.stem<-topfeatures(dfm_core.stem, n=50) # top 50 words
topfeatures2.stem

quartz()
plot(dfm_core.stem)

#### Dfm with bigrams

#mission bigrams
cleancorpus1 <- tokenize(newscorpus1, 
                         removeNumbers=TRUE,  
                         removePunct = TRUE,
                         removeSeparators=TRUE,
                         removeTwitter=FALSE, 
                         ngrams=2, verbose=TRUE) # ngrams=2 ,combinged 2 words

dfm_mission.bigram<- dfm(cleancorpus1, toLower = TRUE, 
                         ignoredFeatures = c(swlist1, stopwords("english")),
                         verbose=TRUE, 
                         stem=FALSE)
topfeatures1.bigram<-topfeatures(dfm_mission.bigram, n=50)
topfeatures1.bigram

# Core bigrams
cleancorpus2 <- tokenize(newscorpus2, 
                         removeNumbers=TRUE,  
                         removePunct = TRUE,
                         removeSeparators=TRUE,
                         removeTwitter=FALSE, 
                         ngrams=2, verbose=TRUE) # ngrams=2 ,combinged 2 words

dfm_core.bigram<- dfm(cleancorpus2, toLower = TRUE, 
                      ignoredFeatures = c(swlist2, stopwords("english")),
                      verbose=TRUE, 
                      stem=FALSE)
topfeatures1.bigram<-topfeatures(dfm_core.bigram, n=50)
topfeatures1.bigram

#specifying a correlation limit of 0.5 
library(tm)
#mission
dfm_mission.tm<-convert(dfm_mission.stem, to="tm")
findAssocs(dfm_mission.tm, 
           c("data", "analyt", "busi"),  # correlation
           corlimit=0.6)
# core
dfm_core.tm<-convert(dfm_core.stem, to="tm")
findAssocs(dfm_core.tm, 
           c("data","analytics", "world" ), # correlation
           corlimit=0.7)

##########################
### Topic Modeling
##########################
library(stm)

###### Mission #######

#Process the data for analysis.
help("textProcessor")
temp<-textProcessor(documents=precorpus$Mission, metadata = precorpus)
names(temp)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

#running stm for top 20 topics
prevfit <-stm(docs , vocab , 
              K=20, 
              verbose=TRUE,
              data=meta, 
              max.em.its=25)

topics1 <-labelTopics(prevfit , topics=c(1:20))
topics1   #shows topics with highest probability words

#explore the topics in context.  Provides an example of the text 
help("findThoughts")
findThoughts(prevfit, texts = precorpus$Mission,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type="summary")
plot.STM(prevfit, type="labels", topics=c(3,12,13))
plot.STM(prevfit, type="perspectives", topics = c(3,12))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations topic
plot.topicCorr(mod.out.corr)

######## Core ############

#Process the data for analysis.
help("textProcessor")
temp<-textProcessor(documents=precorpus$core, metadata = precorpus)
names(temp)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

#running stm for top 20 topics
prevfit <-stm(docs , vocab , 
              K=20, 
              verbose=TRUE,
              data=meta, 
              max.em.its=25)

topics2 <-labelTopics(prevfit , topics=c(1:20))
topics2   #shows topics with highest probability words

#explore the topics in context.  Provides an example of the text 
help("findThoughts")
findThoughts(prevfit, texts = precorpus$core,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type="summary")
plot.STM(prevfit, type="labels", topics=c(3,12,13))
plot.STM(prevfit, type="perspectives", topics = c(3,12))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations topic
plot.topicCorr(mod.out.corr)

# Part 2

#Load CSV
Part2 <- read.csv("~/Desktop/Third Semester/BA/Trump and Forbe 500/Part2.csv", header=TRUE, stringsAsFactors = FALSE)

#### Corpus mission and Core Value

require(quanteda)
newscorpus3<- corpus(Part2$Speech,
                     docnames=Part2$Document_ID,
                     docvar=data.frame(name=Part2$Name))
names(newscorpus3)   #to explore the output of the corpus function: "documents" "metadata"  "settings"  "tokens" 
summary(newscorpus3)  #summary of corpus

#clean corpus: removes punctuation, digits, converts to lower case
newscorpus3<- toLower(newscorpus3, keepAcronyms = FALSE) 
cleancorpus3 <- tokenize(newscorpus3, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)
# DFM
dfm.speech<- dfm(cleancorpus3,
                 toLower = TRUE, 
                 ignoredFeatures =stopwords("english"),  # stopword
                 verbose=TRUE, 
                 stem=FALSE)

### Frequency analysis of word usage

topfeatures3<-topfeatures(dfm.speech, n=50)
topfeatures3

quartz()
plot(dfm.speech)

#to create a custom dictionary  list of stop words
swlist3 = c("will", "s", "t","come","day","put","just")  # remove these words
dfm_speech.stem<- dfm(cleancorpus3, toLower = TRUE, 
               ignoredFeatures = c(swlist3, stopwords("english")), 
               verbose=TRUE, 
               stem=TRUE)
topfeatures3.stem<-topfeatures(dfm_speech.stem, n=50) # top 50 words
topfeatures3.stem

#Sentiment Analysis
help(dfm)
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*"),
                          postive = c("good", "great", "super*", "excellent", "yay"))) 
dfm_speech.sentiment <- dfm(cleancorpus3, dictionary = mydict)
topfeatures(dfm_speech.sentiment)
View(dfm_speech.sentiment)


##########################
### Topic Modeling
##########################

library(stm)
temp<-textProcessor(documents=Part2$Speech, metadata = Part2)
names(temp)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta<-temp$meta
vocab<-temp$vocab
docs<-temp$documents
out <- prepDocuments(docs, vocab, meta)
docs<-out$documents
vocab<-out$vocab
meta <-out$meta

temp
#running stm for top 20 topics
help("stm")
prevfit <-stm(docs , vocab , 
              K=3, 
              verbose=TRUE,
              data=meta, 
              max.em.its=20)

topics <-labelTopics(prevfit, topics=c(1:3))
topics   #shows topics with highest probability words

#explore the topics in context.  Provides an example of the text 
findThoughts(prevfit, texts = Part2$Speech,  topics = 3,  n = 2)

plot.STM(prevfit, type="summary")
plot.STM(prevfit, type="labels", topics=c(1,2,3))
plot.STM(prevfit, type="perspectives", topics = c(1,2))
plot.STM(prevfit, type="perspectives", topics = c(2,3))

# to aid on assigment of labels & intepretation of topics
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations topic
plot.topicCorr(mod.out.corr)
