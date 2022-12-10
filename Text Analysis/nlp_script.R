#Machine Coding and Reliability Testing

##Run Bayes classifier on the training sets and apply to new data 
##Training sets from 'News Drivers' ICA Paper (Diehl, 2019); data source (https://data.world/martinchek/2012-2016-facebook-posts)

#Steps
#1st: run some pre-processing on the text and create the classifier variable 
#2nd: create a DTM with Quanteda (create the training data sets) and run models
#3th: Run performance analysis
#4th: Apply model to the larger data set

#Libraries
library(quanteda) #might also need to update Rcpp package if there are problems 
library(quanteda.textstats) #newer package for stats
library(tm)

#Data & WD  
setwd("~/Desktop/Text Analysis")
load("~/Desktop/Text Analysis/coded_posts.Rdata") #750 human coded posts, 5 categories of News Drivers 

d1 = d_coded

####1 Pre-processing 
#Create new variable that collapses the title, description, and text into one cell
d1$text2 <- paste(d1$headline, d1$text, d1$description, sep=" ")
#Check our new cell versus prior cells
d1$headline[1] ; d1$text[1] ; d1$description[1]  ; d1$text2[1]

#Clean text (order of cleaning matters, so check that the cleaning process is working)
d1$text2 = gsub("NA", "", d1$text2) #replace NAs
d1$text2 = gsub("NULL", "", d1$text2)
d1$text2 = char_tolower(d1$text2) #lower case
d1$text2 = gsub("-", " ", d1$text2)#replace dashes w/spaces
d1$text2 = gsub(' j |-|[^\x01-\x7F]', " ", d1$text2)#remove emoticons 
d1$text2 = removePunctuation(d1$text2) #remove other punctuation
d1$text2 = removeWords(d1$text2, stopwords(kind = "en"))#remove stop words
d1$text2 = removeNumbers(d1$text2) #numbers 

#Check text
d1$text2[2]

#create single classifier variable (f = folded variable)
d1$f = 0
d1$f[d1$event==1] <- "1"
d1$f[d1$entertainment==1] <- "2"
d1$f[d1$lifestyle==1] <- "3"
d1$f[d1$report==1] <- "4"
d1$f[d1$government==1] <- "5"
d1$f[d1$`dont know`== 1] <- "0"

d1$f = as.numeric(d1$f)

####2 RUN the classifier models
#See: https://tutorials.quanteda.io/machine-learning/nb/

library(quanteda)#text analysis
library(quanteda.textstats)#text stats
library(quanteda.textmodels)#for text models
library(caret)#predictive models
library(e1071)#probability testing, TU Wien

#isolate the vars to test
#sub-sample text w/coding scheme
d2 = d1[c(11,12)]
d2 <- subset(d2, d2$f > 0 ) #drop 'don't know' 

#create a corpus for the  machine counts and assign a document variable
c1 = corpus(d2$text2)
summary(c1)

docvars(c1, "f") = d2$f
summary(c1)

#create the document feature matrix
dtm = dfm(c1) #warning: need to update, commands are being phased out 
dtm

#create training (n = 600) and test (N = 150) data sets (*larger training sets = better models)
train_dtm = dfm_sample(dtm, size = 600)
test_dtm = dtm[setdiff(docnames(dtm), docnames(train_dtm)), ]

#specify the models
nb_model <- textmodel_nb(train_dtm, docvars(train_dtm, "f")) #training 
pred_nb = predict(nb_model, newdata = test_dtm) #prediction model
summary(pred_nb)
summary(nb_model)

#match the features
dfmat_matched <- dfm_match(test_dtm, features = featnames(train_dtm)) #need to match features across data sets for Bayes

####3 Performance analysis
actual_class <- dfmat_matched$f #actual
predicted_class <- predict(nb_model, newdata = dfmat_matched) #predicted
tab_class <- table(actual_class, predicted_class) #table of comparisons
confusionMatrix(tab_class, mode = "everything") #performance results 

####53% overall accuracy and low performance in some categories
####Let's fold our human coding into Hard and Soft news and re-test

#create classifier variable (f = folded variable)
d1$f = 0
d1$f[d1$event==1] <- "1"
d1$f[d1$entertainment==1] <- "2"
d1$f[d1$lifestyle==1] <- "2"
d1$f[d1$report==1] <- "1"
d1$f[d1$government==1] <- "1"
d1$f[d1$`dont know`== 1] <- "0"

d1$f = as.numeric(d1$f)

#isolate the vars to test
#sub-sample text w/coding scheme
d2 = d1[c(11,12)]
d2 <- subset(d2, d2$f > 0 ) #drop 'don't know' 

#create a corpus for the  machine counts and assign a document variable
c1 = corpus(d2$text2)
summary(c1)

docvars(c1, "f") = d2$f
summary(c1)

#create the document feature matrix
dtm = dfm(c1) #warning: need to update, commands are being phased out 
dtm

#create training (n = 600) and test (N = 150) data sets 
train_dtm = dfm_sample(dtm, size = 600)
test_dtm = dtm[setdiff(docnames(dtm), docnames(train_dtm)), ]

#specify the models
nb_model <- textmodel_nb(train_dtm, docvars(train_dtm, "f")) #training 
pred_nb = predict(nb_model, newdata = test_dtm) #prediction model
summary(pred_nb)
summary(nb_model)

#match the features
dfmat_matched <- dfm_match(test_dtm, features = featnames(train_dtm)) #Apply the test to prediction model (match features)

#Tables and performance analysis
actual_class <- dfmat_matched$f #actual
predicted_class <- predict(nb_model, newdata = dfmat_matched) #predicted
tab_class <- table(actual_class, predicted_class) #table of comparisons
confusionMatrix(tab_class, mode = "everything") #performance results 

###4: Apply the model to a new dataset#
#create new corpus and text data frame (dfm) from the large sample

load("~/Desktop/Text Analysis/sample_posts.Rdata") #load a fresh data set

#d = full data set; c2 = the new corpus

d3 = d[c(12)]#pull col. 12, the text variable

#quick clean/standardization
d3$text = gsub("NA", "", d$text) #replace NAs
d3$text = gsub("NULL", "", d$text)
d3$text = char_tolower(d$text) #lower case
d3$text = gsub("-", " ", d$text)#replace dashes w/spaces
d3$text = gsub(' j |-|[^\x01-\x7F]', " ", d$text)#remove emoticons 
d3$text = removePunctuation(d$text) #remove other punctuation
d3$text = removeWords(d$text, stopwords(kind = "en"))#remove stop words
d3$text = removeNumbers(d$text) #numbers 

c2 = corpus(d3$text)#create a corpus
summary(c2)

#create a term matrix 
news_dfm <- dfm(c2)
summary(news_dfm)

#match the features & run the model (based on test_dtm above)
dfmat_matched <- dfm_match(test_dtm, features = featnames(news_dfm)) #Apply the classifier to new data
news_driver <- predict(nb_model, newdata = news_dfm, force = TRUE) #warning: larger data set has more words, so classifier doesn't recognize those

#combine the variable with the original data set 
d4 = as.data.frame(news_driver)
d5 = cbind(d, d4)
d = d5

#save file as needed


