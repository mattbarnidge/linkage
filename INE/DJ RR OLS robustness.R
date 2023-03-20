rm1 = lm(ipe ~ inv + 
           age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
           sm.freq.c + size.c + div.c + grp.c, 
           data=x, weights=weights)


summary(lm1, cor=FALSE)
summary(rm1)
anova(lm1, rm1)

rm2 = lm(pol ~ inv +
           age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
           sm.freq.c + size.c + div.c + grp.c, 
           data=x, weights=weights)


summary(lm2, cor=FALSE)
summary(rm2)
anova(lm2, rm2)


