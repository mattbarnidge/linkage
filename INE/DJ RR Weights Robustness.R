rm3 = lmer(ipe ~ inv + 
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur + 
             (1 | frame), 
           data=x, 
           control=lmerControl(optimizer="bobyqa"))

rm4 = lmer(pol ~ inv +
             age + female + poc + edu + inc + ideo + pid + 
             sm.freq + size + div + grp + cur +
             (1 | frame), 
           data=x, 
           control=lmerControl(optimizer="bobyqa"))

summary(lm1, cor=FALSE)
summary(rm3, cor=FALSE)

summary(lm2, cor=FALSE)
summary(rm4, cor=FALSE)