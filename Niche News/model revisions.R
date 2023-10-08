#Niche News
#Alternate Model Testing for Revision
#Adding smcur to the models



#Load Libraries
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(dotwhisker)
library(visreg)

#Set WD and Load Data (may need to update this)
setwd("/Users/trevordiehl/Documents/linkage/Niche News")

load("niche.mi.Rdata") # contains non-niche members
load("niche.mi.sub.Rdata") #filter the non-niche members

#Grand Descriptives
with(m2, mean(newsideo, na.rm=TRUE))
with(m2, var(newsideo, na.rm=TRUE)) 


#Group Descriptives
with(m2, aggregate(newsideo, by=list(niche), mean)) 
with(m2, aggregate(newsideo, by=list(niche), var)) 


#Create Group Means Variables
ideo.means = with(m2, aggregate(ideo, by = list(niche), mean, na.rm=TRUE))[,2]
m2$ideo.g[m2$niche == "local"] <- ideo.means[1]
m2$ideo.g[m2$niche == "elite"] <- ideo.means[2]
m2$ideo.g[m2$niche == "cable"] <- ideo.means[3]

news.means = with(m2, aggregate(newsideo, by = list(niche), mean, na.rm=TRUE))[,2]
m2$news.g[m2$niche == "local"] <- news.means[1]
m2$news.g[m2$niche == "elite"] <- news.means[2]
m2$news.g[m2$niche == "cable"] <- news.means[3]

#Clean Up Environment
rm(ideo.means, news.means)


#Group Mean Center
m2 <- m2 %>%
  group_by(niche) %>%
  mutate(across(ideo:smcur, 
                ~(as.numeric(scale(., scale=FALSE))), 
                .names = "{col}.c"))

#Change Variable Labels for Plotting
m2 <- rename(m2, 
             Individual = ideo.c, 
             Audience = ideo.g, 
             Organization = news.g)


#Original Models
mlm1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm2 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm3 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm4 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Audience + Individual*Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm5 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + Individual + Organization + Individual*Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))


#Summaries
options(scipen=10)
summary(mlm1, cor=FALSE); logLik(mlm1)
summary(mlm2, cor=FALSE); logLik(mlm2)
summary(mlm3, cor=FALSE); logLik(mlm3)
summary(mlm4, cor=FALSE); logLik(mlm4)
summary(mlm5, cor=FALSE); logLik(mlm5)



#Alternate Models for Revision (add curation (other sm vars add noise but no effects))
#Fit Random Intercepts Models
mlm1.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + smcur.c + Individual + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm2.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + smcur.c + Individual + Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm3.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + smcur.c + Individual + Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))

mlm4.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + smcur.c + Individual + Audience + Individual*Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm5.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + int.c + smcur.c + Individual + Organization + Individual*Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))



options(scipen=10)
summary(mlm1.1, cor=FALSE); logLik(mlm1.1)
summary(mlm2.1, cor=FALSE); logLik(mlm2.1)
summary(mlm3.1, cor=FALSE); logLik(mlm3.1)

options(scipen=10)
summary(mlm4.1, cor=FALSE); logLik(mlm4.1)
summary(mlm5.1, cor=FALSE); logLik(mlm5.1)

#check ICC
performance::icc(mlm3.1)




#Plotting Interactions

plot1 = visreg(mlm4.1, "Individual", by = "Audience", 
               overlay=TRUE, jitter=TRUE,
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")),
               breaks=c(-.52, .62), 
               strip.names=c("Aud. Ideo. = -1 SD", "Aud. Ideo. = +1 SD"), 
               xlab = "Individual Ideology", 
               ylab = "Selection Valence")


plot2 = visreg(mlm5.1, "Individual", by = "Organization", 
               overlay=TRUE, jitter=TRUE,
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")),
               breaks=c(-.35, .15), 
               strip.names=c("Org. Ideo. = -1 SD", "Org. Ideo. = +1 SD"), 
               xlab = "", 
               ylab = "")





