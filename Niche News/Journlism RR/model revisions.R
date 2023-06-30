#Alternate Model Testing for R/R
#Adding INE to the models + interactions w/ Political Interest
#New table of correlations w/antecedents 


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
load("niche.mi.Rdata")
load("niche.mi.sub.Rdata")

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
  mutate(across(ideo:ine, 
                ~(as.numeric(scale(., scale=FALSE))), 
                .names = "{col}.c"))


#Change Variable Labels for Plotting
m2 <- rename(m2, 
             Individual = ideo.c, 
             Audience = ideo.g, 
             Organization = news.g,
             Interest = int.c)


#Original Models
mlm1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + Individual + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm2 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + Individual + Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm3 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + Individual + Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm4 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + Individual + Audience + Individual*Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm5 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + Individual + Organization + Individual*Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))


#Summaries
options(scipen=10)
summary(mlm1, cor=FALSE); logLik(mlm1)
summary(mlm2, cor=FALSE); logLik(mlm2)
summary(mlm3, cor=FALSE); logLik(mlm3)
summary(mlm4, cor=FALSE); logLik(mlm4)
summary(mlm5, cor=FALSE); logLik(mlm5)



#Alternate Model Testing for R/R w/INE
#Fit Random Intercepts Models
mlm1.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm2.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm3.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))

#Fit Interactions 
mlm4.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + Audience + Individual*Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm5.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + Organization + Individual*Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm6.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + Audience + Interest*Audience + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))
mlm7.1 = lmer(newsideo ~ age.c + female.c + poc.c + edu.c + inc.c + Interest + ine.c + Individual + Organization + Interest*Organization + (1 + Individual | niche:frame), data = m2, weights = weights, control = lmerControl(optimizer="Nelder_Mead"))


#Summaries
options(scipen=10)
summary(mlm1.1, cor=FALSE); logLik(mlm1.1)
summary(mlm2.1, cor=FALSE); logLik(mlm2.1)
summary(mlm3.1, cor=FALSE); logLik(mlm3.1)


summary(mlm4.1, cor=FALSE); logLik(mlm4.1)
summary(mlm5.1, cor=FALSE); logLik(mlm5.1)
summary(mlm6.1, cor=FALSE); logLik(mlm6.1)
summary(mlm7.1, cor=FALSE); logLik(mlm7.1)

#Test for differences w/original models 
anova(mlm1,mlm1.1) #Pr(>Chisq) = 0.7218 n.s.
anova(mlm2,mlm2.1) #Pr(>Chisq) = 0.5876 n.s.
anova(mlm3,mlm3.1) #Pr(>Chisq) = 0.5841 n.s.




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



plot3 = visreg(mlm6.1, "Interest", by = "Audience", 
               overlay=TRUE, jitter=TRUE,
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")),
               breaks=c(-.52, .62), 
               strip.names=c("Aud. Ideo. = -1 SD", "Aud. Ideo. = +1 SD"), 
               xlab = "Political Interest", 
               ylab = "Selection Valence")

plot4 = visreg(mlm7.1, "Interest", by = "Organization", 
               overlay=TRUE, jitter=TRUE,
               line=list(col=c("black", "grey")), 
               points=list(col=c("black", "grey")),
               breaks=c(-.35, .15), 
               strip.names=c("Org. Ideo. = -1 SD", "Org. Ideo. = +1 SD"), 
               xlab = "", 
               ylab = "")





##Correlation Plot for descriptive relationships 

#Clear the environment
rm(list = ls())

#use the pooled sample data before imputation (there are a few more vars in there)

load("/Users/trevordiehl/Documents/linkage/Niche News/Data/niche_news.RData")


d2$ine = sqrt(d2$sminc*d2$smincexp2) #create the INE var

d = d2[ , c(7:13,37,30,32:36)] #filter model vars

rm(d2)


#create dichotomous variables from the niche factor variable
d$niche = as.character(d$niche) #this is easier to convert type and change NAs
d$niche = ifelse(is.na(d$niche), "none", d$niche) #relabel NAs
d$niche = as.factor(d$niche) #back to factor

#now create membership vars
d = d %>% 
  mutate(niche = factor(niche)) %>% 
  cbind(sapply(levels(.$niche), `==`, .$niche)*1)

d[,c(10, 15:18)] #check it



#correlations with stars for copy/paste into Word via CSV
#https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/#correlation_matrix
#https://cran.r-project.org/web/packages/corrtable/index.html

#install.packages("corrtable")
library(corrtable)

correlation_matrix(d[,c('age', 'female', 'poc', 'edu', 'inc', 'ine', 'int', 'ideo', 'news_avoid', 'news_counter', 
                        'news_pro', 'niche_belong', 'cable', 'elite', 'local')], type = "pearson", digits = 2, show_significance = TRUE, use ='upper')



save_correlation_matrix(df = d[,c('age', 'female', 'poc', 'edu', 'inc', 'ine', 'int', 'ideo', 'news_pro', 'news_counter', 
                                  'niche_belong', 'cable', 'elite', 'local')],
                        filename = 'test.txt', 
                        type = "pearson", digits = 2, show_significance = TRUE, use ='upper')


