#Working Models 1: State-Like DV Binary (1 = Incidentally Exposed, 0 = Purposefully Exposed or Not Exposed)
#What is more important: background frequency of exposure or background intentionality?
wm1 = glm(incexp.sk ~ pol + iny + 
            know + sm.newsintent + avoid +
            age + female + poc + edu + inc, 
          data=d, family = binomial)

sqrt(car::vif(wm1))
summary(wm1)
exp(coef(wm1))

par(mfrow=c(1,2))
visreg::visreg(wm1, "pol", scale="response", ylim=c(0,1))
visreg::visreg(wm1, "iny", scale="response", ylim=c(0,1))

#Test interaction between frequency and incidentality
wm1i = glm(incexp.sk ~ pol*iny + 
             know + sm.newsintent + avoid +
             age + female + poc + edu + inc, 
          data=d, family = binomial)

summary(wm1i)
exp(coef(wm1i))

par(mfrow=c(1,1))
visreg::visreg(wm1i, "pol", by="iny", scale="response", ylim=c(0,1))

#Working Models 2: Trait-Like DVs (Frequency, Incidentality, Frequency*Incidentality)
#What predicts frequency, incidentality, and incidental exposure?
#What informational inequalities can be detected in incidental exposure?
wm2a = lm(pol ~ 
            know + nml + eff.int + 
            age + female + poc + edu + inc + ideo.ext + pid.str + 
            sm.newsintent + 
            int + smfollow + avoid + 
            sm.ns + sm.div.occ + grp + smcur + 
            sm.freq + alg, 
          data=d)
wm2b = lm(iny ~ 
            know + nml + eff.int + 
            age + female + poc + edu + inc + ideo.ext + pid.str + 
            sm.newsintent + 
            int + smfollow + avoid + 
            sm.ns + sm.div.occ + grp + smcur + 
            sm.freq + alg, 
          data=d)
wm2c = lm(ipe ~ 
            know + nml + eff.int + 
            age + female + poc + edu + inc + ideo.ext + pid.str + 
            sm.newsintent + 
            int + smfollow + avoid + 
            sm.ns + sm.div.occ + grp + smcur + 
            sm.freq + alg, 
          data=d)

sqrt(car::vif(wm2a)) > 2
sqrt(car::vif(wm2b)) > 2
sqrt(car::vif(wm2c)) > 2

summary(wm2a)
summary(wm2b)
summary(wm2c)

#Working Models 3: Engagement (State-Like DV)
#How is incidentality related to engagement process among those who were exposed (n = 797)
#Among those incidentally exposed (n = 422), what processes lead to engagement?
wm3a = lm(story.engage ~ incexp + 
           pol + iny + 
           know + sm.newsintent + avoid +
           age + female + poc + edu + inc, 
         data=subset(d, story.aware == 1))
sqrt(car::vif(wm3a)) > 2
summary(wm3a)

wm3b = lm(story.engage ~ 
            story.mult + 
            cong + storypart.ext + 
            storycandeval + storyemo + 
            curatoreval +
            pol + iny + 
            know + sm.newsintent + avoid +
            age + female + poc + edu + inc, 
          data=subset(d, incexp == 1))
sqrt(car::vif(wm3b)) > 2
summary(wm3b)
  
wm3i1 = lm(story.engage ~ incexp*story.mult + 
            pol + iny + 
            know + sm.newsintent + avoid +
            age + female + poc + edu + inc, 
          data=subset(d, story.aware == 1))
wm3i2 = lm(story.engage ~ incexp*storycandeval + 
             pol + iny + 
             know + sm.newsintent + avoid +
             age + female + poc + edu + inc, 
           data=subset(d, story.aware == 1))
wm3i3 = lm(story.engage ~ incexp*storyemo + 
             pol + iny + 
             know + sm.newsintent + avoid +
             age + female + poc + edu + inc, 
           data=subset(d, story.aware == 1))

summary(wm3i1) #ns
summary(wm3i2) #ns
summary(wm3i3) #sig

visreg::visreg(wm3i1, "story.mult", by="incexp")
visreg::visreg(wm3i2, "storycandeval", by="incexp")
visreg::visreg(wm3i3, "storyemo", by="incexp")









#Working Models: State-Like DV Multinomial
#wm1a = nnet::multinom(incexp.f1 ~ pol + inc, data=d)
#wm1b = nnet::multinom(incexp.f2 ~ pol + inc, data=d)
#summary(wm1a); (1 - pnorm(abs(summary(wm1a)$coefficients/summary(wm1a)$standard.errors), 0, 1)) * 2
#summary(wm1b); (1 - pnorm(abs(summary(wm1b)$coefficients/summary(wm1b)$standard.errors), 0, 1)) * 2
