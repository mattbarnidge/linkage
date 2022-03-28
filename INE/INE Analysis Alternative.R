#Load Libraries
library(dplyr)
library(tidyr)

#Set WD and load data
setwd("~/Documents/GitHub/linkage/INE")
load("ine.Rdata")

#Correlations among indicators of involvement (.34 < r < .52)
with(x, round(cor(cbind(mot, int, fol, alg), 
                  use="complete.obs"), digits=2))

#Load Libraries
library(lme4)
library(lmerTest)

#DV: Non Social Media News Use
am = lmer(nsmnews ~ mot + int + fol + alg + 
             age + female + poc + edu + inc + ideo + pid +
             (1 | frame), 
           data=x, weights=weights, 
           control=lmerControl(optimizer="bobyqa"))

sqrt(car::vif(am)) > 2
summary(am, cor=FALSE); logLik(am); performance::r2(am); performance::icc(am)

#DV: Incidental exposure
lm1 = lmer(ipe ~ mot + int + fol + alg + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg1 = glmer(incexp.sk ~ mot + int + fol + alg + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa", 
                                 optCtrl=list(maxfun=2e5)))

#DV: Overall/story exposure
lm2 = lmer(pol ~ mot + int + fol + alg +
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg2 = glmer(recall ~ mot + int + fol + alg + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur +
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

#Visuals
par(mfrow=c(2,2))
visreg::visreg(lm1, "mot", jitter=TRUE, line=list(col="black"), ylab="Incidental Exposure (Trait-Like)", xlab="Social Media as News Source")
visreg::visreg(lm1, "int", jitter=TRUE, line=list(col="black"), ylab="", xlab="Self-Reported Interest")
visreg::visreg(lm1, "fol", jitter=TRUE, line=list(col="black"), ylab="", xlab="Follow Accounts for News")
visreg::visreg(lm1, "alg", jitter=TRUE, line=list(col="black"), ylab="", xlab="Algorithmic Categorization")

par(mfrow=c(2,2))
visreg::visreg(lg1, "mot", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="Incidental Exposure (State-Like)", xlab="Social Media as News Source")
visreg::visreg(lg1, "int", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="", xlab="Self-Reported Interest")
visreg::visreg(lg1, "fol", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="", xlab="Follow Accounts for News")
visreg::visreg(lg1, "alg", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="", xlab="Algorithmic Categorization")

par(mfrow=c(2,2))
visreg::visreg(lm2, "mot", jitter=TRUE, line=list(col="black"), ylab="Total Exposure (Trait-Like)", xlab="Social Media as News Source")
visreg::visreg(lm2, "int", jitter=TRUE, line=list(col="black"), ylab="", xlab="Self-Reported Interest")
visreg::visreg(lm2, "fol", jitter=TRUE, line=list(col="black"), ylab="", xlab="Follow Accounts for News")
visreg::visreg(lm2, "alg", jitter=TRUE, line=list(col="black"), ylab="", xlab="Algorithmic Categorization")

par(mfrow=c(2,2))
visreg::visreg(lg2, "mot", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="Story Exposure (State-Like)", xlab="Social Media as News Source")
visreg::visreg(lg2, "int", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="", xlab="Self-Reported Interest")
visreg::visreg(lg2, "fol", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="", xlab="Follow Accounts for News")
visreg::visreg(lg2, "alg", jitter=TRUE, line=list(col="black"), scale="response", ylim=c(0,1), ylab="", xlab="Algorithmic Categorization")


#Recode incexp variable (state) to be factor (for visualization)
x$incexp = as.factor(x$incexp)

#Fit models
lg3a = lmer(story.engage ~ mot*incexp + int + fol + alg +
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
lg3b = lmer(story.engage ~ int*incexp + mot + fol + alg +
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
lg3c = lmer(story.engage ~ fol*incexp + mot + int + alg + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
lg3d = lmer(story.engage ~ alg*incexp + mot + int + fol + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))

lg4a = lmer(story.engage.he ~ mot*incexp + int + fol + alg + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
lg4b = lmer(story.engage.he ~ int*incexp + mot + fol + alg + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=subset(x, recall==1),
            control=lmerControl(optimizer="bobyqa",
                                optCtrl=list(maxfun=2e5)))
lg4c = lmer(story.engage.he ~ fol*incexp + mot + int + alg + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=subset(x, recall==1),
            control=lmerControl(optimizer="bobyqa",
                                optCtrl=list(maxfun=2e5)))
lg4d = lmer(story.engage.he ~ alg*incexp + mot + int + fol + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=subset(x, recall==1),
            control=lmerControl(optimizer="bobyqa",
                                optCtrl=list(maxfun=2e5)))

#Multicollinearity Diagnostics
sqrt(car::vif(lg3a)) > 2
sqrt(car::vif(lg3b)) > 2
sqrt(car::vif(lg3c)) > 2
sqrt(car::vif(lg3d)) > 2

sqrt(car::vif(lg4a)) > 2
sqrt(car::vif(lg4b)) > 2
sqrt(car::vif(lg4c)) > 2
sqrt(car::vif(lg4d)) > 2

#Model Summaries
summary(lg3a, cor=FALSE); logLik(lg3a); performance::r2(lg3a); performance::icc(lg3a)
summary(lg3b, cor=FALSE); logLik(lg3b); performance::r2(lg3b); performance::icc(lg3b)
summary(lg3c, cor=FALSE); logLik(lg3c); performance::r2(lg3c); performance::icc(lg3c)
summary(lg3d, cor=FALSE); logLik(lg3d); performance::r2(lg3d); performance::icc(lg3d)

summary(lg4a, cor=FALSE); logLik(lg4a); performance::r2(lg4a); performance::icc(lg4a)
summary(lg4b, cor=FALSE); logLik(lg4b); performance::r2(lg4b); performance::icc(lg4b)
summary(lg4c, cor=FALSE); logLik(lg4c); performance::r2(lg4c); performance::icc(lg4c)
summary(lg4d, cor=FALSE); logLik(lg4d); performance::r2(lg4d); performance::icc(lg4d)


#viz
visreg::visreg(lg3b, "incexp", by="int", jitter=TRUE, line=list(col="black"), 
               ylab="Engagement", xlab="Exposure Type")
visreg::visreg(lg4b, "incexp", by="int", jitter=TRUE, line=list(col="black"), 
               ylab="High-Effort Engagement", xlab="Exposure Type")
