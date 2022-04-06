#Load Libraries
library(dplyr)
library(tidyr)

#Set WD and load data
setwd("~/Documents/GitHub/linkage/INE")
load("ine.Rdata")

#Load Libraries
library(lme4)
library(lmerTest)

#Center Controls
x <- x %>% 
  group_by(frame) %>% 
  mutate(age.c = age-mean(age)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(female.c = female-mean(female)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(poc.c = poc-mean(poc)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(edu.c = edu-mean(edu)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(inc.c = inc-mean(inc)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(ideo.c = ideo-mean(ideo)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(pid.c = pid-mean(pid)) 

#DV: Non Social Media News Use
am = lmer(nsmnews ~ int + 
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c +
             (1 | frame), 
           data=x, weights=weights, 
           control=lmerControl(optimizer="bobyqa"))

sqrt(car::vif(am)) > 2
summary(am, cor=FALSE); logLik(am); performance::r2(am); performance::icc(am)

par(mfrow=c(1,1))
visreg::visreg(am, "int", jitter=TRUE, line=list(col="black"),
               ylab="Non-Social Media News Use", xlab="Interest")

#More centering
x <- x %>% 
  group_by(frame) %>% 
  mutate(sm.freq.c = sm.freq-mean(sm.freq)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(size.c = size-mean(size)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(div.c = div-mean(div)) 
x <- x %>% 
  group_by(frame) %>% 
  mutate(grp.c = grp-mean(grp))

#DV: Incidental exposure
lm1 = lmer(ipe ~ int + 
              age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
              sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg1 = glmer(incexp.sk ~ int + 
              age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
              sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa", 
                                 optCtrl=list(maxfun=2e5)))

#DV: Overall/story exposure
lm2 = lmer(pol ~ int +
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg2 = glmer(recall ~ int + 
              age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
              sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",
                                 optCtrl=list(maxfun=2e5)))

#Multicollinearity Diagnostics
sqrt(car::vif(lm1)) > 2
sqrt(car::vif(lg1)) > 2
sqrt(car::vif(lm2)) > 2
sqrt(car::vif(lg2)) > 2

#Model Summaries
summary(lm1, cor=FALSE); logLik(lm1); performance::r2(lm1); performance::icc(lm1)
summary(lg1, cor=FALSE); logLik(lg1); performance::r2(lg1); performance::icc(lg1)
summary(lm2, cor=FALSE); logLik(lm2); performance::r2(lm2); performance::icc(lm2)
summary(lg2, cor=FALSE); logLik(lg2); performance::r2(lg1); performance::icc(lg2)

#Visualizations
par(mfrow=c(2,2))
visreg::visreg(lm1, "int", jitter=TRUE, line=list(col="black"),
               main="Trait-Like DV", ylab="Incidental Exposure", xlab="")
visreg::visreg(lg1, "int", jitter=TRUE, line=list(col="black"), 
               main="State-Like DV", ylab="Incidental Exposure", xlab="")
visreg::visreg(lm2, "int", jitter=TRUE, line=list(col="black"), 
               ylab="Total Exposure", xlab="Interest")
visreg::visreg(lg2, "int", jitter=TRUE, line=list(col="black"),
               ylab="Story Exposure", xlab="Interest")

#Recode incexp variable (state) to be factor (for visualization)
x$incexp = as.factor(x$incexp)

#More centering
x <- x %>% 
  group_by(frame) %>% 
  mutate(ipe.c = ipe-mean(ipe)) 

#Fit models
lg3 = lmer(story.engage ~ int*incexp + 
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
lg4 = lmer(story.engage.he ~ int*incexp + 
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))

#Multicollinearity Diagnostics
sqrt(car::vif(lg3)) > 2
sqrt(car::vif(lg4)) > 2

#Model Summaries
summary(lg3, cor=FALSE); logLik(lg3); performance::r2(lg3); performance::icc(lg3)
summary(lg4, cor=FALSE); logLik(lg4); performance::r2(lg4); performance::icc(lg4)

#Add labels for visualization
x$incexp <- factor(x$incexp,
                levels = c(0,1),
                labels = c("Purp.", "Inc."))

#Refit models
lg3.v = lmer(story.engage ~ int*incexp + 
               age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
               sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
lg4.v = lmer(story.engage.he ~ int*incexp + 
               age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
               sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Visualizations
par(mfrow=c(1,1))
visreg::visreg(lg3.v, "int", by="incexp", jitter=TRUE, line=list(col="black"), 
               ylab="Engagement", xlab="Interest")
visreg::visreg(lg4.v, "int", by="incexp", jitter=TRUE, line=list(col="black"), 
               ylab="High-Effort Engagement", xlab="Interess")


