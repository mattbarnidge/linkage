#Using custom dictionaries in Quandeta 

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

#Apply the populism dictionary (Lexicoder format, see the txt file for details)
popdic <- dictionary(file = "populism_dic.txt", format = "lexicoder") #import the word lists

pop_dfm = dfm(d$text, dictionary = popdic) #create the dfm and apply counts
head(pop_dfm) #check the 'head' of the matrix

#convert document feature matrix back into the data frame, merge sentiment counts to the data
d2 = convert(pop_dfm, to = "data.frame")
d = cbind(d, d2[ ,2:6])
rm(d2, pop_dfm, popdic)


