library(lme4)
library(lmerTest)
setwd("~/Documents/GitHub/linkage/INE")
load("ine1.Rdata")
load("ine2.Rdata")

#Test whether frame matters
null = lm(ine1 ~ 1, data=x1)
full = lmer(ine1 ~ 1 + (1 | frame), data=x1)
AIC(null, full); BIC(null, full); lmtest::lrtest(null, full)
rm(null, full)

null = lm(ine2 ~ 1, data=x1)
full = lmer(ine2 ~ 1 + (1 | frame), data=x1)
AIC(null, full); BIC(null, full); lmtest::lrtest(null, full)
rm(null, full)

null = lm(smnews ~ 1, data=x1)
full = lmer(smnews ~ 1 + (1 | frame), data=x1)
AIC(null, full); BIC(null, full); lmtest::lrtest(null, full)
rm(null, full)

null = lm(engage ~ 1, data=x1)
full = lmer(engage ~ 1 + (1 | frame), data=x1)
AIC(null, full); BIC(null, full); lmtest::lrtest(null, full)
rm(null, full)

null = lm(story.aware ~ 1, data=x2)
full = lmer(story.aware ~ 1 + (1 | frame), data=x2)
AIC(null, full); BIC(null, full); lmtest::lrtest(null, full)
rm(null, full)

null = lm(story.engage.sk ~ 1, data=subset(x2, story.aware == 1))
full = lmer(story.engage.sk ~ 1 + (1 | frame), data=subset(x2, story.aware == 1))
AIC(null, full); BIC(null, full); lmtest::lrtest(null, full)
rm(null, full)

#Trait Models
tm1 = lm(ine1 ~ 
           know.c + nml.c + sm.freq.c + 
           age.c + female.c + poc.c + edu.c + inc.c + pid.str.c + eff.int.c + 
           sm.newsintent.c + 
           int.c + smfollow.c + avoid.c + 
           sm.ns.c + sm.div.occ.c + grp.c + 
           alg.c, 
         data = x1, weights = weights)
tm2 = lm(ine2 ~ 
           know.c + nml.c + sm.freq.c + 
           age.c + female.c + poc.c + edu.c + inc.c + pid.str.c + eff.int.c + 
           sm.newsintent.c + 
           int.c + smfollow.c + avoid.c + 
           sm.ns.c + sm.div.occ.c + grp.c + 
           alg.c, 
         data = x1, weights = weights)
tm3 = lmer(smnews ~ 
             know.c + nml.c + sm.freq.c + 
             age.c + female.c + poc.c + edu.c + inc.c + pid.str.c + eff.int.c + 
             sm.newsintent.c + 
             int.c + smfollow.c + avoid.c + 
             sm.ns.c + sm.div.occ.c + grp.c + 
             alg.c +
             (1 | frame),
           data = x1, weights = weights, 
           control = lmerControl("bobyqa"))
tm4 = lmer(engage ~ 
             know.c + nml.c + sm.freq.c + 
             age.c + female.c + poc.c + edu.c + inc.c + pid.str.c + eff.int.c + 
             sm.newsintent.c + 
             int.c + smfollow.c + avoid.c + 
             sm.ns.c + sm.div.occ.c + grp.c + 
             alg.c +
             (1 | frame),
           data = subset(x1, smnews > 1), weights = weights, 
           control = lmerControl("bobyqa"))

#Multicollinearity Diagnostics
car::vif(tm1)
car::vif(tm2)
car::vif(tm3)
car::vif(tm4)

#Model Summaries
summary(tm1)
summary(tm2)
summary(tm3, cor=FALSE)
summary(tm4, cor=FALSE)

#State Models
sm1 = glmer(story.aware ~ 
              storypart.ext.c + storycandeval.c +
              story.purp.sk.c + 
              cong.c + storyemo.c + storycred.c +
              smcur.c + curatoreval.sk.c +
              engagement + net_sent + topic_freq +
              (1 | frame), 
            data = x2, family = poisson, weights = weights, 
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sm2 = lmer(story.engage.sk ~ 
             storypart.ext.c + storycandeval.c +
             story.purp.sk.c + 
             cong.c + storyemo.c + storycred.c +
             smcur.c + curatoreval.sk.c +
             engagement + net_sent + topic_freq +
             (1 | frame), 
           data = subset(x2, story.aware == 1), weights = weights, 
           control=lmerControl(optimizer="bobyqa"))

#Multicollinearity Diagnostics
car::vif(sm1)
car::vif(sm2)

#Model summaries
summary(sm1, cor=FALSE)
summary(sm2, cor=FALSE)

