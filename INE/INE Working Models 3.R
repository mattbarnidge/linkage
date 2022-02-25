#RQ1: Are the uninvolved more likely to report incidental exposure?
#RQ2: Is incidental exposure related to engagement among the uninvolved?

#Correlations among indicators of involvement
with(x, round(cor(cbind(mot, int, fol, alg), use="complete.obs"), digits=2))

#LCA based on indicators of involvement

#Fix variables
x$x1 = x$mot + 1
x$x2 = round(x$int, digits = 0)
x$x3 = round(x$fol, digits = 0)
x$x4 = x$alg + 1

#Define function
f <- cbind(x1, x2, x3, x4) ~ 1

#Determine Best Model
library(poLCA)
max_II <- -100000
min_bic <- 100000
for(i in 2:5){
  lc <- poLCA(f, x, nclass=i, maxiter=3000, tol=1e-5, na.rm=FALSE,  nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model <- lc
  }
} 
print(LCA_best_model)
rm(LCA_best_model, i, max_II, min_bic)

#Estimate Model, Fix Order, Re-Estimate Model
lc <- poLCA(f, x, nclass=3, maxiter=3000, tol=1e-5, na.rm=TRUE,  nrep=10, verbose=TRUE, calc.se=TRUE, graph=TRUE)
probs.start <- poLCA.reorder(lc$probs.start, order(lc$P, decreasing=TRUE))
lc <- poLCA(f, x, nclass = 3, maxiter=3000, tol=1e-5, na.rm=TRUE, nrep=10, verbose=TRUE, calc.se=TRUE, probs.start = probs.start, graph=TRUE)

rm(probs.start, f)

#Extract Grouping Variable
x$inv = as.factor(lc$predclass)

#Trait-like DV

#Fit models
library(lme4)
mlm1 = lmer(pol ~ inv + iny +
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
mlm2 = lmer(ipe ~ inv + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
mlm3 = lmer(engage ~ ipe*inv +
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))

summary(mlm1, cor=FALSE)
summary(mlm2, cor=FALSE)
summary(mlm3, cor=FALSE)

par(mfrow=c(1,2))
visreg::visreg(mlm1, "inv", ylab="Total Exposure", xlab="Involvement")
visreg::visreg(mlm2, "inv", ylab="Incidental Exposure", xlab="Involvement")

par(mfrow=c(1,1))
visreg::visreg(mlm3, "ipe", by = "inv", ylab="Engagement", xlab="Involvement")

#State-like DVs

#Fit models
lg1 = glmer(recall ~ inv + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
lg2 = glmer(incexp.sk ~ inv + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
x$incexp = as.factor(x$incexp)
lg3 = lmer(story.engage ~ inv*incexp + 
              age + female + poc + edu + inc + ideo + pid + 
              sm.freq + size + div + grp + cur + ipe +
              (1 | frame), 
            data=subset(x, recall==1), weights=weights, 
          control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(lg1)
summary(lg2)
summary(lg3)

par(mfrow=c(1,2))
visreg::visreg(lg1, "inv", ylab="Exposure", xlab="Involvement")
visreg::visreg(lg2, "inv", ylab="Incidental Exposure", xlab="Involvement")

par(mfrow=c(1,1))
visreg::visreg(lg3, "incexp", by="inv", ylab="Engagement", xlab="Exposure Type")

#Breakdown
tab <- with(subset(x, recall==1), table(story.engage, incexp, inv)) 
print(tab)

#Some Engagement
#58% of people in low involvement group
#47% of people in middle involvement group
#23% of people in high involvement group

#High Engagement
#10% of people in low involvement group
#18% of people in medium involvement group
#12% of people in high involvement group






