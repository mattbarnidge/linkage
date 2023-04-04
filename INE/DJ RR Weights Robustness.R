rm3 = lmer(ipe ~ inv + 
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c +
             (1 | frame), 
           data=x, 
           control=lmerControl(optimizer="bobyqa"))

rm4 = lmer(pol ~ inv +
             age.c + female.c + poc.c + edu.c + inc.c + ideo.c + pid.c + 
             sm.freq.c + size.c + div.c + grp.c +
             (1 | frame), 
           data=x, 
           control=lmerControl(optimizer="bobyqa"))

summary(rm3, cor=FALSE)
summary(rm4, cor=FALSE)