#This is the variable creator script for filtering posts
#Should be ran with the filter_merge_clusters.R script
#That script will call on this one to filter the posts

#Creates aggregate measures for engagement and sentiment @ wave and niche level
#Filters stories by percentile ranking of within niche engagement levels 

#Code engagement and Outlet
d1$engagement = (d1$likes + (d1$comments*3) + (d1$shares*2)) #weighted metric

#label the clusters based on network analysis
d1$outlet = factor(d1$outlet)
d1$type = factor(d1$type)
table(d1$outlet)

clusterlabels = c(1,1,3,99,2,1,1,2,1,2,1,2,2,3,1,1,2,2,1,2,2,3,3,2,2,2,3) #99 = Business Insider (not mentioned in the survey)
table(clusterlabels)

d1$org_cluster = factor(d1$outlet, labels = clusterlabels)
table(d1$org_cluster)

#check your coding
orgs = as.data.frame(table(d1$outlet))
orgs$code = clusterlabels

#drop the NA rows
d1 = droplevels(d1[!d1$org_cluster == "99",])

#Create the ranking variable
d1 = mutate(d1, percentile_rank = ntile(d1$engagement,100))#get the percentile/rank

d1 = d1 %>% group_by(org_cluster) %>%
  mutate(rank_by_group = ntile(engagement,100))#percentile rank within cluster


#Sentiment analysis
#new variable for text analysis
d1$text = d1$snippet
d1$text = removePunctuation(d1$text)
d1$text = char_tolower(d1$text)
d1$text = gsub(' j |-|[^\x01-\x7F]', " ", d1$text)#remove dashes and emoticons
#Sentiment Counts
sent_dfm = dfm(d1$text, dictionary = data_dictionary_LSD2015)
head(sent_dfm)
d2 = convert(sent_dfm, to = "data.frame")
d1 = cbind(d1, d2[ ,2:5])
d1$text = removeWords(d1$text, stopwords(kind = "en"))
d1$wc = ntoken(d1$text)
#net sentiment
d1$net_sent = ((d1$positive - d1$neg_positive)/d1$wc) - ((d1$negative - d1$neg_negative)/d1$wc)
rm(d2)

#group and wave means
d1$mean.eng = aggregate(engagement ~ 1, d1[which(d1$percentile_rank > 98), ], mean)#get mean wave engagement for top 2%
d1$mean.sent = aggregate(net_sent ~ 1, d1[which(d1$percentile_rank > 98), ], mean)#get mean wave sentiment for top 2%

d1$wave_engage = as.numeric(unlist(d1$mean.eng))
d1$wave_sent = as.numeric(unlist(d1$mean.sent))

d1$mean.eng = NULL
d1$mean.sent = NULL

#sentiment by cluster
d2 = d1[which(d1$rank_by_group > 98), ]#cut by percentile rank in group 
group_sent = aggregate(net_sent ~ org_cluster, d2, mean) #get the group means for checking the mutate funtionality 
group_engage = aggregate(engagement ~ org_cluster, d2, mean)

d2 = d2 %>% group_by(org_cluster) %>%
  mutate(group_engage = mean(engagement))#engagement by cluster
table(d2$group_engage, d2$org_cluster)


d2 = d2 %>% group_by(org_cluster) %>%
  mutate(group_sent = mean(net_sent))#sentiment by cluster
table(d2$group_sent, d2$org_cluster)

d1 = d2
rm(d2, group_engage, group_sent, sent_dfm, clusterlabels, orgs)

##end:: now goe back to filter_merge_clusters.R script





