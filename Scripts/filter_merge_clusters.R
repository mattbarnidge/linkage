###Truncated Script for Filtering###

#2020 Election Survey: Filter the News Posts, Cluster Coding, and Merge
#Import, label, sentiment analysis as needed and merge data

#Instructions:
#1) Load each wave separately (ensures accurate text analysis, simplifies variable creation and error checking)
#2) Script calls on the sentiment/engagement variable creator script (filter_var_creator.R)
#3) Merge w/respondent level in the 'merge_levels' script

setwd("C:/Users/diehl/Desktop/Research/News on FB/Election News Posts") #set to folder with excel files

library(readxl)
library(quanteda) #might also need to update Rcpp package
library(quanteda.textstats)
library(tm)
library(dplyr)

#wave 1
d1 <- read_excel("0902_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)
d1$wave = 1

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w1 = d1
#clean
rm(d1)

#wave 2
d1 <- read_excel("0906_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)


d1$wave = 2

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w2 = d1
#clean
rm(d1)

#wave 3
d1 <- read_excel("0909_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)



d1$wave = 3

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w3 = d1
#clean
rm(d1)


#wave 4
d1 <- read_excel("0913_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 4

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w4 = d1
#clean
rm(d1)


#wave 5
d1 <- read_excel("0916_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)


d1$wave = 5

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w5 = d1
#clean
rm(d1)


#wave 6 
d1 <- read_excel("0920_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)


d1$wave = 6

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w6 = d1
#clean
rm(d1)


#wave 7
d1 <- read_excel("0923_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)


d1$wave = 7

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w7 = d1
#clean
rm(d1)


#wave 8 
d1 <- read_excel("0927_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 8

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w8 = d1
#clean
rm(d1)


#wave 9
d1 <- read_excel("0930_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 9

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w9 = d1
#clean
rm(d1)


#wave 10
d1 <- read_excel("1004_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 10

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w10 = d1
#clean
rm(d1)


#wave 11
d1 <- read_excel("1007_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 11

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w11 = d1
#clean
rm(d1)



#wave 12
d1 <- read_excel("1011_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 12

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w12 = d1
#clean
rm(d1)


#wave 13
d1 <- read_excel("1014_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 13

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w13 = d1
#clean
rm(d1)
#wave 14
d1 <- read_excel("1018_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)
d1$wave = 14

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w14 = d1
#clean
rm(d1)

#wave 15
d1 <- read_excel("1021_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 15

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w15 = d1
#clean
rm(d1)

#wave 16
d1 <- read_excel("1025_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 16

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w16 = d1
#clean
rm(d1)

#wave 17
d1 <- read_excel("1028_Newsposts.xlsx", col_types = c("numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "text"),
                 col_names = c("rank", "outlet", "title", "snippet", "url", "author", "date", "likes", "comments", "shares", "type"), skip = 10)

d1$wave = 17

#run the variable creator 
source("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/scripts/filter_var_creator.R")

#check cluster means + group assignments 
aggregate(engagement ~ org_cluster, d1, mean)
d1 %>%
  group_by(org_cluster, group_engage) %>%
  summarize(Freq=n())

#label for merge
w17 = d1
#clean
rm(d1)

#merge
posts = rbind(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17)
rm(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15, w16, w17)

#save
#setwd("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/data")
#save(posts, file = "cluster_posts.Rdata")
