#Merge post-level data w/respondent-level data
#Using the cluster_posts data: mean sentiment and engagement by wave & cluster

#load data sets
load("C:/Users/diehl/Desktop/Research/Papers/CommDetection/CommDetection/data/cluster_posts.Rdata")
load('EMCP20_coded.Rdata') #already coded for cluster membership via the revised variable creator 


d1 = d

#pull vars needed to create the niche variables 
vars = c("ResponseId","frame","cluster.1", "cluster.2", "cluster.3")


d1 = d1[vars]

#Prepare the respondent=level data
#create the cluster membership vars at the respondent level
#Cluster membership/strength
#in-group strength
d1$cluster_r1 = ifelse(d1$cluster.1 == 1, 1, 0)#cluster 1
d1$cluster_r2 = ifelse(d1$cluster.2 == 1, 1, 0)#cluster 1
d1$cluster_r3 = ifelse(d1$cluster.3 == 1, 1, 0)#cluster 1
d1$cluster_r4 = ifelse(d1$cluster.1 == 2, 1, 0)#cluster 2
d1$cluster_r5 = ifelse(d1$cluster.2 == 2, 1, 0)#cluster 2
d1$cluster_r6 = ifelse(d1$cluster.3 == 2, 1, 0)#cluster 2
d1$cluster_r7 = ifelse(d1$cluster.1 == 3, 1, 0)#cluster 3
d1$cluster_r8 = ifelse(d1$cluster.2 == 3, 1, 0)#cluster 3
d1$cluster_r9 = ifelse(d1$cluster.3 == 3, 1, 0)#cluster 3

#bonding: membership strength & assign case to single cluster by bond strength
d1$bonding_1 = rowSums(d1[,c('cluster_r1', 'cluster_r2', 'cluster_r3')], na.rm =T)#bonding cluster 1
d1$bonding_2 = rowSums(d1[,c('cluster_r4', 'cluster_r5', 'cluster_r6')], na.rm =T)#bonding cluster 2
d1$bonding_3 = rowSums(d1[,c('cluster_r7', 'cluster_r8', 'cluster_r9')], na.rm =T)#bonding cluster 3

#Extract the 'strongest' cluster, competing categories are treated as 0
d1$org_cluster = ifelse(d1$bonding_1 > d1$bonding_2 & d1$bonding_1 > d1$bonding_3, 1, 0) #cable
d1$org_cluster = ifelse(d1$bonding_2 > d1$bonding_1 & d1$bonding_2 > d1$bonding_3, 2, d1$org_cluster) #elite
d1$org_cluster = ifelse(d1$bonding_3 > d1$bonding_1 & d1$bonding_3 > d1$bonding_2, 3, d1$org_cluster) #local
d1$org_cluster = ifelse(d1$org_cluster == 0, NA, d1$org_cluster)#drop non-categorized
table(d1$org_cluster)


#create the ID variable for wave*cluster
d1$wave = d1$frame
ID = as.data.frame(with(d1, interaction(wave,  org_cluster)))
names(ID)[1] = 'ID' #rename
d1 = cbind(d1, ID)



#Prepare the organizational-level/wave aggregate descriptions 
names(posts)
posts$org_cluster = as.numeric(posts$org_cluster)
vars = c("org_cluster", "group_engage", "group_sent", "wave", "wave_engage", "wave_sent")
p1 = posts[vars]

#group-level means
group_means = aggregate(cbind(group_engage, group_sent) ~ wave + org_cluster, data = p1, min)#using FUN = min but they are all the same 
ID = as.data.frame(with(group_means, interaction(wave,  org_cluster)))
names(ID)[1] = 'ID'
group_means = cbind(group_means, ID)
group_means
#wave means
wave_means = aggregate(cbind(wave_engage, wave_sent) ~ wave, data = p1, min)#using FUN = min but they are all the same 
wave_means

#merge by the aggregate index data sets 
d2 = merge(wave_means, d1, by = "wave", all.y=TRUE)
d3 = merge(group_means[,3:5], d2, by = "ID", all.y=TRUE)
d3 = d3[ ,c(1:3,5:7,24)]
#save file
#save(d3, file = "newsfeed_data.Rdata")
#d3 = merge(d3, d, by = "ResponseId", all.y=TRUE)
