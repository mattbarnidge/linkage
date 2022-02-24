#Load Libraries
library(dplyr)
library(tidyr)

#Set WD and Load Data
setwd("~/Documents/GitHub/linkage/INE")
load("EMCP20_coded.Rdata")

#Exposure and Incidentality: 
d$pol = d$sminc - 1 #social media political information; making "never" = 0
d$iny = d$smincexp2 - 1 #incidentality; making "never" = 0
d$recall = d$story.aware #recall of embedded story (state)
d$incexp = abs(d$story.purp - 1) #incidentally exposed to story (no skips)
d$incexp.f1 = ifelse(d$recall == 1 & d$incexp == 1, "inc", 
                     ifelse(d$recall == 1 & d$incexp == 0, "purp", 
                            "none"))
d$incexp.f1 = factor(d$incexp.f1, levels=c("none", "inc", "purp"))
d$incexp.f2 = factor(d$incexp.f1, levels=c("inc", "none", "purp"))

#Engagement
table(d$engage)
table(d$story.engage)

#Control Variables
summary(d$age) 
summary(d$female)
summary(d$poc) 
summary(d$edu) 
summary(d$inc) 
summary(d$ideo) 
summary(d$pid) 
summary(d$sm.freq)

#Interests and Involvement
d$mot = d$sm.newsintent #social media as news source
summary(d$int) #interest
d$fol = d$smfollow #following accounts for news
d$alg[d$alg==2] <- 1 #algorithmic categorization

#Social Networks
d$size = log(d$sm.ns) #network size, logged
d$div = d$sm.div.occ #network diversity, sociostructural
d$grp = log(d$grp) #social media groups, logged
d$cur = d$smcur #social curation

#Rolling Covariates
table(d$story.mult) #multiple exposures
d$cong <- NA
d$cong = ifelse(d$story.part == 1 & d$ideo > 0, 1, d$cong)
d$cong = ifelse(d$story.part == 0 & d$ideo < 0, 1, d$cong)
d$cong = ifelse(d$ideo == 0, 0, d$cong)
d$cong = ifelse(d$story.part == 1 & d$ideo < 0, -1, d$cong)
d$cong = ifelse(d$story.part == 0 & d$ideo > 0, -1, d$cong) #ideological congruence
d$storypart = abs(d$storypart - 6) #perceived partisanship (extremity)
table(d$storycandeval) #relevance for evaluating candidates
with(d, ltm::cronbach.alpha(cbind(storyemo.joy, 
                                  storyemo.enth, 
                                  storyemo.ang, 
                                  storyemo.fear), na.rm=TRUE))
d$storyemo = with(d, rowMeans(cbind(storyemo.joy, 
                                    storyemo.enth, 
                                    storyemo.ang, 
                                    storyemo.fear), na.rm=TRUE)) #emotional response
table(d$curatoreval) #evaluation of curator

#Select Variables
x <- d %>% select(pol, iny, engage,
                  age, female, poc, edu, inc, ideo, pid, sm.freq, 
                  mot, int, fol, alg, 
                  size, div, grp, cur,
                  recall, incexp, story.engage, 
                  story.mult, cong, storypart, 
                  storycandeval, storyemo, curatoreval,
                  frame, weights)

#Multiple Imputation
t1 <- mice::mice(x[,1:19], m=1, maxit=50, meth='pmm', seed=500)
t2 <- mice::mice(x[,20:28], m=1, maxit=50, meth='pmm', seed=500)
x[,1:19] <- mice::complete(t1, 1)
x[,20:28] <- mice::complete(t2, 1)
rm(t1, t2)
x <- na.omit(x)

#Create Incidental Exposure Factor (didn't want this in multiple imputation)
x$incexp.f1 = ifelse(x$recall == 1 & x$incexp == 1, "inc", 
                     ifelse(x$recall == 1 & x$incexp == 0, "purp", 
                            "none"))
x$incexp.f1 = factor(x$incexp.f1, levels=c("none", "inc", "purp"))
x$incexp.f2 = factor(x$incexp.f1, levels=c("inc", "none", "purp"))

