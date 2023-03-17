rm1 = lm(ipe ~ inv + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur, 
           data=x, weights=weights)


summary(lm1, cor=FALSE)
summary(rm1)
anova(lm1, rm1)

rm2 = lm(pol ~ inv +
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur, 
           data=x, weights=weights)

summary(lm2, cor=FALSE)
summary(rm2)
anova(lm2, rm2)


