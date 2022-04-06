#Load Libraries
library(dplyr)
library(tidyr)

#Set WD and load data
setwd("~/Documents/GitHub/linkage/INE")
load("ine.Rdata")

#Correlations among indicators of involvement (.34 < r < .72)
with(x, round(cor(cbind(mot, int, fol, alg, cur), 
                  use="complete.obs"), digits=2))

#Recode variables for LCA (LCA requires non-zero integers)
x$x1 = x$mot + 1
x$x2 = round(x$int, digits = 0) 
x$x3 = round(x$fol, digits = 0) 
x$x4 = round(x$cur, digits = 0)
x$x5 = x$alg + 1

#Define LCA function
f <- cbind(x1, x2, x3, x4, x5) ~ 1

#Load Library for LCA
library(poLCA)

#Write function to find best fitting model
#min = 2 classes, max = 5 classes (trouble with convergence above 5)
min_bic <- 100000
for(i in 2:5){
  lc <- poLCA(f, x, nclass=i, 
              maxiter=3000, tol=1e-5, 
              na.rm=FALSE, nrep=10, 
              verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model <- lc
  }
} 

#Print results (3-class model is best, according to BIC)
print(LCA_best_model)

#Estimate Model, re-set order based on predicted probabilities 
#(biggest group first), then re-estimate models
lc <- poLCA(f, x, nclass=4, 
            maxiter=3000, tol=1e-5, 
            na.rm=TRUE,  nrep=10, 
            verbose=TRUE, calc.se=TRUE, 
            graph=TRUE) #initial model

probs.start <- poLCA.reorder(lc$probs.start, 
                             order(lc$P, decreasing=TRUE)) #create object for order

lc <- poLCA(f, x, nclass = 4, 
            maxiter=3000, tol=1e-5, 
            na.rm=TRUE, nrep=10, 
            verbose=TRUE, calc.se=TRUE, 
            probs.start = probs.start, 
            graph=TRUE) #refit model with order object (probs.start)

#Clean up environment
rm(f, 
   i, 
   min_bic, 
   LCA_best_model, 
   probs.start)

detach("package:poLCA", unload=TRUE)

#Extract grouping variable and add to dataset
x$inv = as.factor(lc$predclass)
levels(x$inv) = c("Mod Unmot", "Low", "Mod Mot", "High")
x$inv <- factor(x$inv, levels = c("Low", "Mod Unmot", "Mod Mot", "High"))
table(x$inv)

##########################################################################

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
am = lmer(nsmnews ~ inv + 
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c +
             (1 | frame), 
           data=x, weights=weights, 
           control=lmerControl(optimizer="bobyqa"))

sqrt(car::vif(am)) > 2
summary(am, cor=FALSE); logLik(am); performance::r2(am); performance::icc(am)

par(mfrow=c(1,1))
visreg::visreg(am, "inv", jitter=TRUE, line=list(col="black"),
               ylab="Non-Social Media News Use", xlab="News Attraction")

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
lm1 = lmer(ipe ~ inv + 
              age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
              sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg1 = glmer(incexp.sk ~ inv + 
              age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
              sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa", 
                                 optCtrl=list(maxfun=2e5)))

#DV: Overall/story exposure
lm2 = lmer(pol ~ inv +
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c +
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg2 = glmer(recall ~ inv + 
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
visreg::visreg(lm1, "inv", jitter=TRUE, line=list(col="black"),
               main="Trait-Like DV", ylab="Incidental Exposure", xlab="")
visreg::visreg(lg1, "inv", jitter=TRUE, line=list(col="black"), 
               main="State-Like DV", ylab="Incisdental Exposure", xlab="")
visreg::visreg(lm2, "inv", jitter=TRUE, line=list(col="black"), 
               ylab="Total Exposure", xlab="News Attraction")
visreg::visreg(lg2, "inv", jitter=TRUE, line=list(col="black"),
               ylab="Story Exposure", xlab="News Attraction")

#Recode incexp variable (state) to be factor (for visualization)
x$incexp = as.factor(x$incexp)

#More centering
x <- x %>% 
  group_by(frame) %>% 
  mutate(ipe.c = ipe-mean(ipe)) 

#Fit models
lg3 = lmer(story.engage ~ inv*incexp + 
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
lg4 = lmer(story.engage.he ~ inv*incexp + 
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
lg3.v = lmer(story.engage ~ inv*incexp + 
               age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
               sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
lg4.v = lmer(story.engage.he ~ inv*incexp + 
               age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
               sm.freq.c + size.c + div.c + grp.c + ipe.c +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Visualizations
par(mfrow=c(1,1))
visreg::visreg(lg3.v, "inv", by="incexp", jitter=TRUE, line=list(col="black"), 
               ylab="Engagement", xlab="News Attraction")
visreg::visreg(lg4.v, "inv", by="incexp", jitter=TRUE, line=list(col="black"), 
               ylab="High-Effort Engagement", xlab="News Attraction")


