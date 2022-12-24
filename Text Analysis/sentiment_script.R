#Sentiment analysis with Quanteda 


#Steps:
#1: import and clean text
#2: convert to a document term matrix (count words per document (in this case document = news post))
#3: apply the counts
#4: convert back to data frame, merge with original data set, create variables as needed 

#Libraries
library(quanteda) #might also need to update Rcpp package if there are problems 
library(quanteda.textstats) #newer package for stats
library(tm) #classic text manipulation package 


#data & WD
#setwd("~/Desktop/Text Analysis")
load("~/Desktop/Text Analysis/sample_posts.Rdata")

#new variable for text analysis
d$text = d$snippet

#quick manual standardization of text
d$text = removePunctuation(d$text) #remove punctuation 
d$text = char_tolower(d$text) #lower case
d$text = gsub(' j |-|[^\x01-\x7F]', " ", d$text)#remove dashes and emoticons (gsub uses grep commands)

#quick inspect cleaning
d[ 1,c(3,12)]

#Sentiment Analysis

#create document feature matrix (dfm) and apply the counts 
#getting warnings b/c the package updated and commands are being phased out 
sent_dfm = dfm(d$text, dictionary = data_dictionary_LSD2015) #LSD = Lexicoder Sentiment Dictionary (built in package!)
head(sent_dfm) #check the 'head' of the matrix

#convert document feature matrix back into the data frame, merge sentiment counts to the data
d2 = convert(sent_dfm, to = "data.frame")
d = cbind(d, d2[ ,2:5])

#a handful of stop words are in the LSD, so truncate before setting word count
d$text = removeWords(d$text, stopwords(kind = "en"))#remove stop words
d$wc = ntoken(d$text)#word count

#net sentiment based on previous work
d$net_sent = ((d$positive - d$neg_positive)/d$wc) - ((d$negative - d$neg_negative)/d$wc)

rm(d2, sent_dfm)


