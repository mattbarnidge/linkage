#RQ: Does incidentality increase exposure among uninvolved?
#A: No, not really. 
#For trait-like DV, negative effect in lowest involvement group, no effect in middle involvement group.
#For state-like DV, no effect in lowest involvement group, negative effect in middle involvement group.

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
x$inv = lc$predclass

#Main Effects Models
library(lme4)
library(lmerTest)
wm1 = lmer(pol ~
             age + female + poc + edu + inc + ideo + pid + sm.freq + 
             size + div + grp + cur +
             inv + iny + 
             (1 | frame), 
           data=x, weights=weights, 
           control=lmerControl(optimizer="bobyqa"))
wm2 = glmer(recall ~ 
              age + female + poc + edu + inc + ideo + pid + sm.freq + 
              size + div + grp + cur + 
              inv + iny + 
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Summaries
summary(wm1, cor=FALSE)
summary(wm2, cor=FALSE)

#Visualizations
par(mfrow=c(1,2))
visreg::visreg(wm1, "inv")
visreg::visreg(wm1, "iny")

visreg::visreg(wm2, "inv")
visreg::visreg(wm2, "iny")

#Conditional Effects Model
wm1c = lmer(pol ~ 
              age + female + poc + edu + inc + ideo + pid + sm.freq +
              size + div + grp + cur +
              inv*iny + 
              (1 | frame), 
            data=x, weights=weights, 
            control=lmerControl(optimizer="bobyqa"))
wm2c = glmer(recall ~ 
              age + female + poc + edu + inc + ideo + pid + sm.freq + 
              size + div + grp + cur + 
              inv*iny + 
              (1 | frame), 
            data=x, family=poisson, weights=weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

#Summaries
summary(wm1c)
summary(wm2c)

#Visualizations
visreg::visreg(wm1c, "iny", by = "inv")
visreg::visreg(wm2c, "iny", by = "inv", scale="response", ylim=c(0,1))

#Group Differences Models
x$inv = as.factor(x$inv)
table(x$inv, x$incexp.f1)
wm3a = nnet::multinom(incexp.f1 ~ inv, data=x)
wm3b = nnet::multinom(incexp.f2 ~ inv, data=x)
summary(wm3a); (1 - pnorm(abs(summary(wm3a)$coefficients/summary(wm3a)$standard.errors), 0, 1)) * 2
summary(wm3b); (1 - pnorm(abs(summary(wm3b)$coefficients/summary(wm3b)$standard.errors), 0, 1)) * 2

par(mfrow=c(1,3))
visreg::visreg(wm3a)

#RQ: Does incidentility increase engagement among uninvolved?
wm3 = lm(story.engage ~ 
           
           
           
           incexp + 
            pol + iny + 
            know + sm.newsintent + avoid +
            age + female + poc + edu + inc, 
          data=subset(d, story.aware == 1))



sqrt(car::vif(wm3a)) > 2
summary(wm3a)




