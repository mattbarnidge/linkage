#Missingness Checks for Digital Journalism R/R 

#Sources
#https://www.gerkovink.com/miceVignettes/Missingness_inspection/Missingness_inspection.html
#http://naniar.njtierney.com/


#1) First pull the data and create the model vars
#2) Then create data set w/ model vars 
#3) Quick missingness checks 

#Load Libraries (may need to install)
library(dplyr)
library(tidyr)
library(mice) #imputation 
install.packages("naniar")
library(naniar) #alternative missing check

#1: Pull the data and create the model vars

#Set WD and Load Data
setwd("~/Documents/linkage/INE") #maybe different on your PC
load("EMCP20_coded.Rdata")


#2: Create new dataset w/model vars (from INE Data Treatment Script)
#Exposure and Incidentality: 
#trait-like variables
d$news = d$smnews #social media news
d$pol = d$sminc - 1 #social media political information; making "never" = 0
d$iny = d$smincexp2 - 1 #extent of incidentality; making "never" = 0
d$mot = abs(d$sm.newsintent - 1) #background motivation something other than news
d$ipe = sqrt((d$sminc-1)*(d$smincexp2-1)) #pol info by incidentality
d$ine = sqrt((d$smnews-1)*(d$smincexp2-1)) #news use by incidentality

d$recall = d$story.aware #recall of embedded story (state)
d$incexp = abs(d$story.purp - 1) #incidentally exposed to story (no skips)
d$incexp.sk = ifelse(d$recall == 1 & d$incexp == 1, 1, 0) #include skips

d$incexp.f1 = ifelse(d$recall == 1 & d$incexp == 1, "inc", 
                     ifelse(d$recall == 1 & d$incexp == 0, "purp", 
                            "none"))
d$incexp.f1 = factor(d$incexp.f1, levels=c("none", "inc", "purp"))
d$incexp.f2 = factor(d$incexp.f1, levels=c("inc", "none", "purp"))

#NOTE: These I guessed on, they were not in the scripts 
d$ine1 = d$ine 
d$ine2 = d$incexp.f2


#Engagement Variables
table(d$engage)
table(d$story.engage.sk)

#Individual Factors 1: Cognitive
table(d$know); hist(d$know) #trait; political knowledge
table(d$nml); hist(d$nml) #trait; news media literacy
table(d$story.mult.sk); hist(d$story.mult.sk) #state: multiple exposures (proxy for cognitive load/ prior knowledge)
with(d, ltm::cronbach.alpha(cbind(curfact.self, 
                                  curfact.orgs,
                                  curfact.frnd, 
                                  curfact.hist, 
                                  curfact.popu), na.rm=TRUE))
d$sml = with(d, rowMeans(cbind(curfact.self, 
                               curfact.orgs,
                               curfact.frnd, 
                               curfact.hist, 
                               curfact.popu), na.rm=TRUE))
table(d$sml); hist(d$sml) #trait: social media news literacy

#Individual Factors 2: Identity/Demographics
table(d$age) #trait; age
table(d$female) #trait; gender
table(d$poc) #trait; race
table(d$edu) #trait; ses
table(d$inc) #trait; ses
table(d$ideo) #trait; ideology
table(d$pid) #trait; partisanship
d$ideo.ext = abs(d$ideo) #trait; ideological extremity
d$pid.str = abs(d$pid) #trait; strength of partisanship
table(d$eff.int); hist(d$eff.int) #trait: internal efficacy
d$storypart = d$storypart - 6
d$storypart.ext = abs(d$storypart)
table(d$storypart); hist(d$storypart) #state; perceived ideology of story
table(d$storypart.ext); hist(d$storypart.ext) #state; perceived ideological extremity of story
table(d$storycandeval); hist(d$storycandeval) #state; issue salience

#Environmental Perceptions
table(d$sm.newsintent) #trait
table(d$story.purp.sk) #state

#Motivation
table(d$int); hist(d$int) #trait: interest
table(d$smfollow); hist(d$smfollow) #trait: follow for news
table(d$avoid); hist(d$avoid) #trait; avoid news
d$cong <- NA
d$cong = ifelse(d$story.part == 1 & d$ideo > 0, 1, d$cong)
d$cong = ifelse(d$story.part == 0 & d$ideo < 0, 1, d$cong)
d$cong = ifelse(d$ideo == 0, 0, d$cong)
d$cong = ifelse(d$story.part == 1 & d$ideo < 0, -1, d$cong)
d$cong = ifelse(d$story.part == 0 & d$ideo > 0, -1, d$cong)
table(d$cong) #state: congruence of exposure
table(d$storyemo.pos); hist(d$storyemo.pos) #state: perceived emotionality positive
table(d$storyemo.neg); hist(d$storyemo.neg) #state: perceived emotionality negative
with(d, ltm::cronbach.alpha(cbind(storyemo.joy, 
                                  storyemo.enth, 
                                  storyemo.ang, 
                                  storyemo.fear), na.rm=TRUE))
d$storyemo = with(d, rowMeans(cbind(storyemo.joy, 
                                    storyemo.enth, 
                                    storyemo.ang, 
                                    storyemo.fear), na.rm=TRUE))
table(d$storyemo); hist(d$storyemo) #state: perceived emotionality

#Social Networks
table(d$sm.ns); hist(d$sm.ns) #trait; network size
table(d$sm.div.occ); hist(d$sm.div.occ) #trait: network diversity structural
table(d$sm.div.tie); hist(d$sm.div.tie) #trait: network diversity perceptual
table(d$grp); hist(d$grp) #trait: group membership
table(d$smcur); hist(d$smcur) #state: active curation
d$curatoreval.sk <- NA
d$curatoreval.sk <- ifelse(d$story.aware == 1, d$curatoreval, d$curatoreval.sk)
d$curatoreval.sk <- ifelse(d$story.aware == 0, 50, d$curatoreval.sk)
table(d$curatoreval.sk); hist(d$curatoreval.sk) #state: curator eval

#Media Systems
#engagement #state
#sentiment #state
#salience #state
d$alg[d$alg==2] <- 1 
table(d$alg) #trait; algorithmic interest
table(d$int.freq); hist(d$int.freq) #trait: internet frequency
table(d$sm.freq); hist(d$sm.freq) #trait: social media frequency

#Select Variables: Trait Models
x1 <- d %>% select(ine1, ine2, smnews, engage, 
                   know, nml, sml, int.freq, sm.freq,
                   age, female, poc, edu, inc, ideo, pid, ideo.ext, pid.str, eff.int, 
                   sm.newsintent, 
                   int, smfollow, avoid, 
                   sm.ns, sm.div.occ, sm.div.tie, grp, 
                   alg, 
                   frame, weights)




#3: Quick missingness checks
#check the complete cases
d1 = data_complete <- x1[complete.cases(x1), ] 

#inspect with mice
summary(x1)
md.pattern(x1)#check patterns, note that 1,731 have 0 missing and 5 cases have 8
#this is acceptable

#alternative tests with naniar
miss_var_summary(x1) #only shows 10 rows
print(miss_var_summary(x1), n = 30) #show all rows

mcar_test(x1)#MCAR test (low pvalue indicate data is *NOT* MCAR, but that a high bar)

#export a table for review response
miss_table = as.data.frame(print(miss_var_summary(x1), n = 30))
write.csv(miss_table, "missing_table.csv")



#Run this is moving on to comapre imputed versus observed data (not done here)
#Multiple Imputation: Trait Models
t1 <- mice::mice(x1, m=1, maxit=50, meth='pmm', seed=500)
x1 <- mice::complete(t1, 1)
rm(t1)


