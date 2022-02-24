#Part A: What roles does incidentality play in predicting exposure, while also ...
#accounting for other known factors such as (a) interests and involvement, 
#(b) social networks, and (c) tech use and systems

#Trait-like DV: political information exposure
#State-like DV: story recall

#Zero order correlations
cor(d$iny, d$pol, use="complete.obs")
cor(d$iny, d$recall, use="complete.obs")

#Models
wm1 = lm(pol ~ 
           age + female + poc + edu + inc + ideo.ext + #controls
           sm.newsintent + int + know + nml + #interest and involvement
           log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
           sm.freq + alg + #tech use and systems
           iny, #incidentality
         data=d)
wm2 = glm(recall ~ 
           age + female + poc + edu + inc + ideo.ext + #controls
           sm.newsintent + int + know + nml + #interest and involvement
           log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
           sm.freq + alg + #tech use and systems
           iny + #incidentality
           pol, 
          data=d, family=binomial)

#Diagnostics
sqrt(car::vif(wm1)) > 2
sqrt(car::vif(wm2)) > 2
par(mfrow=c(2,2))
plot(wm1)
plot(wm2)

#Summaries
summary(wm1)
summary(wm2)

#Visualizations
par(mfrow=c(1,2))
visreg::visreg(wm1, "iny", partial=FALSE, ylim=c(0,4))
visreg::visreg(wm2, "iny", scale="response", ylim=c(0,1))

#Interactions
wm1i = lm(pol ~ 
           age + female + poc + edu + inc + ideo.ext + #controls
           sm.newsintent + int + know + nml + #interest and involvement
           log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
           sm.freq + alg + #tech use and systems
           iny + #incidentality
           iny*sm.div.occ, #interactions
         data=d)
wm1ii = lm(pol ~ 
            age + female + poc + edu + inc + ideo.ext + #controls
            sm.newsintent + int + know + nml + #interest and involvement
            log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
            sm.freq + alg + #tech use and systems
            iny + #incidentality
            iny*log(grp), #interactions
          data=d)
wm1iii = lm(pol ~ 
            age + female + poc + edu + inc + ideo.ext + #controls
            sm.newsintent + int + know + nml + #interest and involvement
            log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
            sm.freq + alg + #tech use and systems
            iny + #incidentality
            iny*smcur, #interactions
          data=d)

summary(wm1i) 
summary(wm1ii)
summary(wm1iii)

visreg::visreg(wm1i, "iny", by="sm.div.occ")
visreg::visreg(wm1ii, "iny", by="grp")
visreg::visreg(wm1iii, "iny", by="smcur")
#interactions with network diversity; group membership; active curation



#Interactions
wm2i = glm(recall ~ 
            age + female + poc + edu + inc + ideo.ext + #controls
            sm.newsintent + int + know + nml + #interest and involvement
            log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
            sm.freq + alg + #tech use and systems
            iny + #incidentality
            pol + 
            iny*know, 
          data=d, family=binomial)
wm2ii = glm(recall ~ 
             age + female + poc + edu + inc + ideo.ext + #controls
             sm.newsintent + int + know + nml + #interest and involvement
             log(sm.ns) + sm.div.occ + log(grp) + smfollow + smcur + #social networks
             sm.freq + alg + #tech use and systems
             iny + #incidentality
             pol + 
             iny*sm.freq, 
           data=d, family=binomial)

summary(wm2i)
summary(wm2ii)
#interaction with know, sm.freq

visreg::visreg(wm2i, "iny", by="know", scale="response")
visreg::visreg(wm2ii, "iny", by="sm.freq", scale="response")

#Part B: How much exposure is incidental? 
#What is the relationship between motivations and habits and incidental exposure?
table(d$incexp) #incidental = 422 (~53%), purposeful = 375 (~47%)
table(d$incexp.f1) #none = 1210 (~60% of sample)
prop.test(x = 422, n = 797) #no significant difference between incidental and purposeful
prop.test(x = 1210, n = 2008) #significantly more people were NOT exposed (i.e., no recall)

table(d$pol, d$recall)
table(d$iny, d$recall); cor(d$iny, d$recall, use="complete.obs")




#wm1a = nnet::multinom(incexp.f1 ~ pol + inc, data=d)
#wm1b = nnet::multinom(incexp.f2 ~ pol + inc, data=d)
#summary(wm1a); (1 - pnorm(abs(summary(wm1a)$coefficients/summary(wm1a)$standard.errors), 0, 1)) * 2
#summary(wm1b); (1 - pnorm(abs(summary(wm1b)$coefficients/summary(wm1b)$standard.errors), 0, 1)) * 2














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
wm1i = glm(incexp.sk ~ I(pol*iny) + 
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
wm2a = lm(pol ~ iny + 
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
