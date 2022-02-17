library(ggplot2)

setwd("~/Documents/GitHub/linkage")
org <- read.csv("orgs_coded.csv")

#Grand Descriptives
with(org, mean(code)) 
with(org, var(code)) 

#Group Descriptives
org$cluster <- as.factor(org$cluster)
with(org, aggregate(code, by=list(cluster), mean)) 
with(org, aggregate(code, by=list(cluster), var)) 

#Anova
aov.org <- aov(code ~ cluster, data=org)
summary(aov.org)

#Relevel Factor for Visualization
levels(org$cluster)[levels(org$cluster)=="1"] = "cable"
levels(org$cluster)[levels(org$cluster)=="2"] = "elite"
levels(org$cluster)[levels(org$cluster)=="3"] = "local"
org$cluster <- factor(org$cluster, c("local", "elite" ,"cable"))

#Boxplot
p1 <- ggplot(org, aes(x=cluster, y=code, fill=cluster)) + 
  geom_boxplot(show.legend = FALSE) + 
  labs(title = "", x = "", y = "Editorial Valence")
p1 + scale_fill_grey() + theme_classic()
#Note: Combine this plot with individual-level plot

