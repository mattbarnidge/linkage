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

#Clean up environment
rm(LCA_best_model, i, min_bic)

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
visreg::visreg(lm1, "inv", ylab="Incidental Exposure", xlab="Involvement")
visreg::visreg(lg1, "inv", ylab="Incidental Exposure", xlab="Involvement")
visreg::visreg(lm2, "inv", ylab="Total Exposure", xlab="Involvement")
visreg::visreg(lg2, "inv", ylab="Story Exposure", xlab="Involvement")

##########################################################################

#RQ3a: Is incidental exposure related to average level of engagement 
#among the uninvolved?
#RQ3b: Is incidental exposure related to at least some engagement 
#among the uninvolved?

#Analysis overview: test interactions between involvement and 
#incidental exposure for both trait-like and state-like DVs; 
#recode both DVs so that 0 = no engagement and 1 = some engagement and retest

#Results overview: no significant interactions for average engagement; 
#significant interaction for "some" engagement (trait); also evidence for
#similar effect on state-like DV, but interaction is not significant;
#overall evidence for closing engagement gaps is pretty limited

#Recode incexp variable (state) to be factor (for visualization)
x$incexp = as.factor(x$incexp)

#Fit models: average engagement
#note 1: not really interested in main effects here
#note 2: state-like model uses subset of ppl exposed to story (recall==1)
#to account for data-dependencies
#note 3: lg3 is not a logit, but kept naming consistent with above models
lm3 = lmer(engage ~ ipe*inv +
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
lg3 = lmer(story.engage ~ inv*incexp + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1), weights=weights, 
           control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Recode DVs
x$y1 <- NA
x$y1[x$engage == 1] <- 0
x$y1[x$engage > 1] <- 1

x$y2 <- NA
x$y2[x$story.engage == 0] <- 0
x$y2[x$story.engage > 1] <- 1

#Fit models, at least some engagement
#note: using nAGQ=0 to aid convergence
lm4 = glmer(y1 ~ ipe*inv +
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + 
             (1 | frame), 
            data=x, family=poisson, weights=weights, 
            nAGQ=0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
lg4 = glmer(y2 ~ inv*incexp + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + ipe +
             (1 | frame), 
           data=subset(x, recall==1), family=poisson, weights=weights, 
           nAGQ=0, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Multicollinearity Diagnostics
sqrt(car::vif(lm3)) > 2
sqrt(car::vif(lg3)) > 2
sqrt(car::vif(lm4)) > 2
sqrt(car::vif(lg4)) > 2

#Model Summaries
summary(lm3, cor=FALSE)
summary(lg3, cor=FALSE)
summary(lm4, cor=FALSE)
summary(lg4, cor=FALSE)

#Visualizations
par(mfrow=c(1,1))
visreg::visreg(lm3, "ipe", by = "inv", ylab="Engagement", xlab="Incidental Exposure")
visreg::visreg(lg3, "incexp", by="inv", ylab="Engagement", xlab="Exposure Type")
visreg::visreg(lm4, "ipe", by = "inv", ylab="Engagement", xlab="Incidental Exposure")
visreg::visreg(lg4, "incexp", by="inv", ylab="Engagement", xlab="Exposure Type")

#Descriptive Breakdown of State-Like DV
tab <- with(subset(x, recall==1), table(story.engage, incexp, inv)) 
print(tab)

#Some Engagement & Incidentally Exposed
#58% of people in low involvement group
#47% of people in middle involvement group
#23% of people in high involvement group

#High Engagement & Incidentally Exposed
#10% of people in low involvement group
#18% of people in medium involvement group
#12% of people in high involvement group



