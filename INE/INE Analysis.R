#Load Libraries
library(dplyr)
library(tidyr)

#Set WD and load data
setwd("~/Documents/GitHub/linkage/INE")
load("ine.Rdata")

#RQ1: How can we categorize people according to "involvement" with the news?

#Analysis overview: perform latent class analysis with four indicators
#of involvement, including environmental perception (social media as news source), 
#interest, following accounts for news, and algorithmic categorization

#Results overview: LCA reveals three groups, which are arrayed 
#in a more or less linear manner from high involvement to low involvement

#Correlations among indicators of involvement (.33 < r < .53)
with(x, round(cor(cbind(mot, int, fol, alg), 
                  use="complete.obs"), digits=2))

#LCA based on indicators of involvement

#Recode variables for LCA (LCA requires non-zero integers)
x$x1 = x$mot + 1
x$x2 = round(x$int, digits = 0) 
x$x3 = round(x$fol, digits = 0) 
x$x4 = x$alg + 1

#Define LCA function
f <- cbind(x1, x2, x3, x4) ~ 1

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
lc <- poLCA(f, x, nclass=3, 
            maxiter=3000, tol=1e-5, 
            na.rm=TRUE,  nrep=10, 
            verbose=TRUE, calc.se=TRUE, 
            graph=TRUE) #initial model

probs.start <- poLCA.reorder(lc$probs.start, 
                             order(lc$P, decreasing=TRUE)) #create object for order

lc <- poLCA(f, x, nclass = 3, 
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
table(x$inv) 
#Group 1 = low involvement (48%) 
#Group 2 = medium involvement (37.5%)
#Group 3 = high involvement (14.5%)

##########################################################################

#RQ2a: Are the uninvolved more likely to report incidental exposure?
#RQ2b: Are there differences between the uninvolved and involved 
#in overall exposure?

#Analysis overview: test main effects of involvement on both 
#trait-like and state-like DVs for incidental exposure and overall exposure

#Results overview: some evidence that 
#low involvement (trait and state) and medium involvement (state) groups 
#have more incidental exposure; but limited evidence that this closes
#gaps in overall exposure; middle involvement group may represent a 
#"sweet spot" for incidental exposure; 
#significantly more incidental exposure 
#than high involvement group (trait), and equal amount of 
#overall exposure (trait and state) as high involvement group

#Load Libraries
library(lme4)
library(lmerTest)

#Fit models: incidental exposure
#note: lm = 'linear model', lg = 'logit'
#note 2: logits are not really logits because of weights, 
#used family = poisson (see: hist(x$incexp.sk*x$weights))
lm1 = lmer(ipe ~ inv + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg1 = glmer(incexp.sk ~ inv + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Fit models: overall/story exposure
lm2 = lmer(pol ~ inv + iny +
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg2 = glmer(recall ~ inv + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

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
               ylab="Total Exposure", xlab="Involvement")
visreg::visreg(lg2, "inv", jitter=TRUE, line=list(col="black"),
               ylab="Story Exposure", xlab="Involvement")

##########################################################################

#RQ3s: Is incidental exposure related to average level of engagement 
#among the uninvolved?

#Analysis overview: test interactions between involvement and 
#incidental exposure for both state-like DV

#Results overview: no significant interactions for average engagement; 
# evidence for closing engagement gaps is pretty limited

#Recode incexp variable (state) to be factor (for visualization)
x$incexp = as.factor(x$incexp)

#Fit models
lg3 = lmer(story.engage ~ inv*incexp + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
lg4 = lmer(story.engage.he ~ inv*incexp + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Multicollinearity Diagnostics
sqrt(car::vif(lg3)) > 2
sqrt(car::vif(lg4)) > 2

#Model Summaries
summary(lg3, cor=FALSE); logLik(lg3); performance::r2(lg3); performance::icc(lg3)
summary(lg4, cor=FALSE); logLik(lg4); performance::r2(lg4); performance::icc(lg4)

#Add labels for visualization and refit models
x$inv <- factor(x$inv,
                    levels = c(1,2,3),
                    labels = c("Low", "Med", "High"))
x$incexp <- factor(x$incexp,
                levels = c(0,1),
                labels = c("Purp.", "Inc."))


lg3.v = lmer(story.engage ~ inv*incexp + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
lg4.v = lmer(story.engage.he ~ inv*incexp + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1),
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))




#Visualizations
par(mfrow=c(1,1))
visreg::visreg(lg3.v, "incexp", by="inv", jitter=TRUE, line=list(col="black"), 
               ylab="Engagement", xlab="Exposure Type")
visreg::visreg(lg4.v, "incexp", by="inv", jitter=TRUE, line=list(col="black"), 
               ylab="Engagement", xlab="Exposure Type")

#Descriptive Breakdowns of State-Like DV
tab1 <- with(subset(x, recall==1), table(story.engage, incexp, inv)) 
tab2 <- with(subset(x, recall==1), table(story.engage.he, incexp, inv)) 

print(tab1)
print(tab2)

#Some Engagement & Incidentally Exposed
#58% of people in low involvement group
#47% of people in middle involvement group
#23% of people in high involvement group

#High Engagement & Incidentally Exposed
#10% of people in low involvement group
#18% of people in medium involvement group
#12% of people in high involvement group



