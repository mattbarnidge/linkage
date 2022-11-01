#Emotional Engagement w/News, Outcomes, and Moderating Role of Literacy

#Exploratory Analysis:
  # 1) ID summary stats and create key variables (polarization: enthusiasm/aversion + news: counter/pro attitudinal exposure)
  # 2) Look at relationships (network attr. and AP)
  # 3) Basic model specification w/regression
  # 4) Interactions w/network attr. and AP 


#WD and Data
setwd("~/Desktop/Emmotional Engagement")

#coded/cleaned data from time of collection (variable creator +  valence of open-ended news items)
load("~/Desktop/Emmotional Engagement/EMCP20_coded.Rdata")

library(ltm)

#Create/Check key vars

#Polarization
table(d$ap.cand)
hist(d$ap.cand)

table(d$ap.party)
hist(d$ap.party)

table(d$ap.ideo)
hist(d$ap.ideo)

d$ap = with(d, rowMeans(cbind(ap.cand, ap.ideo, ap.party), na.rm = TRUE))
hist(d$ap)
with(d, cronbach.alpha(cbind(ap.cand, ap.ideo, ap.party), na.rm = TRUE)) # alpha = .845

#pos/neg feeling to in party/out party

#enthusiasm 
hist(d$ap.cand.in)
hist(d$ap.party.in)
hist(d$ap.ideo.in)
d$ap_pos = with(d,rowMeans(cbind(ap.cand.in, ap.party.in, ap.ideo.in), na.rm=TRUE))

with(d,cronbach.alpha(cbind(ap.cand.in, ap.party.in, ap.ideo.in), na.rm=TRUE)) # alpha = .717

#aversion
hist(d$ap.cand.out)
hist(d$ap.party.out)
hist(d$ap.ideo.out)
d$ap_neg = with(d,rowMeans(cbind(ap.cand.out, ap.party.out, ap.ideo.out), na.rm=TRUE))

d$ap_neg = abs(d$ap_neg -1) #reverse code; + = less favorable 
hist(d$ap_neg)
with(d,cronbach.alpha(cbind(ap.cand.out, ap.party.out, ap.ideo.out), na.rm=TRUE)) # alpha = .926

#Incidental Exp (syntax from the INE paper)
d$pol = d$sminc - 1 #social media political information; making "never" = 0
d$iny = d$smincexp2 - 1 #incidentality; making "never" = 0
d$ipe = sqrt(d$pol*d$iny)
d$recall = d$story.aware #recall of embedded story (state)
d$incexp = abs(d$story.purp - 1) #incidentally exposed to story (no skips)

#INE to pro-counter attitude news (NEW Syntax)
#d$storypart #perceived partisanship 1 = Left, 6 = Neutral, 11 = Right
#d$story.part #fox/non fox story 1 = Fox

#Pro-attitudinal INE
d$paie_r = ifelse(d$pid > 0 & d$storypart > 6, 1, 0) #pro right IE
d$paie_l = ifelse(d$pid < 0 & d$storypart < 6, 1, 0) #pro left IE
d$paie = rowSums(d[,c('paie_r','paie_l')], na.rm = T) #pro INE
table(d$paie)
hist(d$paie)

#Counter-attitudinal INE
d$caie_r = ifelse(d$pid > 0 & d$storypart < 6, 1, 0) #counter right IE
d$caie_l = ifelse(d$pid < 0 & d$storypart > 6, 1, 0) #counter left IE
d$caie = rowSums(d[,c('caie_r','caie_l')], na.rm = T) #counter INE
table(d$caie)
hist(d$caie)


#network size vars
d$size = log(d$sm.ns) #network size, logged
d$soc_div = d$sm.div.occ #network diversity, socio-structural
d$div = d$sm.div.tie


#Pro and counter attitude exposure (from open-ended responses)

#partisan news vars
d$news_code1_r = ifelse(d$news_code1 == 0, 1, 0)#neutral
d$news_code2_r = ifelse(d$news_code2 == 0, 1, 0)#neutral
d$news_code3_r = ifelse(d$news_code3 == 0, 1, 0)#neutral
d$news_code1_r.1 = ifelse(d$news_code1 > 0, 1, 0)#right
d$news_code2_r.1 = ifelse(d$news_code2 > 0, 1, 0)#right
d$news_code3_r.1 = ifelse(d$news_code3 > 0, 1, 0)#right
d$news_code1_r.2 = ifelse(d$news_code1 < 0, 1, 0)#left
d$news_code2_r.2 = ifelse(d$news_code2 < 0, 1, 0)#left
d$news_code3_r.2 = ifelse(d$news_code3 < 0, 1, 0)#left

#Left/Right/Neutral
d$neut_news = rowSums(d[,c('news_code1_r', 'news_code2_r', 'news_code3_r')], na.rm =T)#neutral cluster
d$right_news = rowSums(d[,c('news_code1_r.1', 'news_code2_r.1', 'news_code3_r.1')], na.rm =T)#right cluster
d$left_news = rowSums(d[,c('news_code1_r.2', 'news_code2_r.2', 'news_code3_r.2')], na.rm =T)#left cluster

#Counter-attitudinal 
d$right_cross = ifelse(d$pid > 0 & d$left_news > 0, 1, 0)
d$left_cross = ifelse(d$pid < 0 & d$right_news > 0, 1, 0)
d$case = rowSums(d[,c('left_cross','right_cross')], na.rm = T)

#Pro-attitudinal confirmation
d$right_conf = ifelse(d$pid > 0 & d$right_news > 0, 1, 0)
d$left_conf = ifelse(d$pid < 0 & d$left_news > 0, 1, 0)
d$pase = rowSums(d[,c('left_conf','right_conf')], na.rm = T)


#Regression models
library(lme4)
library(lmerTest)

#Exposure models 

lm1 = lmer(paie ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div +
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 



lm2 = lmer(caie ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + 
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 


lm3 = lmer(paie ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + sm.freq + int + alg + case + pase + 
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa"))



lm4 = lmer(caie ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + sm.freq + int + alg + case + pase + 
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 


summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)

#polarization Models

lm5 = lmer(ap_pos ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + 
           (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 



lm6 = lmer(ap_neg ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + 
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 



lm7 = lmer(ap_pos ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + sm.freq  + int + alg + case + pase + caie + paie +
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 


lm8 = lmer(ap_neg ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + size + div + sm.freq  + int + alg + case + pase + caie + paie +
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 


summary(lm5)
summary(lm6)
summary(lm7)
summary(lm8)

#tools for html tables and plotting models
#see: http://www.strengejacke.de/sjPlot/index.html
library(sjPlot)
library(sjmisc)
library(sjlabelled)

#Tables (takes a minute when computing odds ratios)
tab_model(lm1, lm2, lm3, lm4, show.ci = FALSE, show.intercept = FALSE, auto.label = FALSE)
tab_model(lm5, lm6, lm7, lm8, show.ci = FALSE, show.intercept = FALSE, auto.label = FALSE)

#Interactions Tests
library(visreg)
int1 = lm(ap_pos ~ caie*alg, data = d) #lm models
summary(int1)
visreg(int1, "caie", by="alg", overlay=TRUE)

#Full models

lm10.1 = lmer(ap_neg ~ age + female + poc + edu + inc + ideo + pid +
             soc_div + grp + div + sm.freq  + int + alg + case + pase + caie*size + paie +
             (1 | frame),
           data=d, weights=weights,
           control=lmerControl(optimizer="bobyqa")) 

lm10.2 = lmer(ap_neg ~ age + female + poc + edu + inc + ideo + pid +
              soc_div + size + grp + sm.freq  + int + alg + case + pase + caie*div + paie +
              (1 | frame),
            data=d, weights=weights,
            control=lmerControl(optimizer="bobyqa")) 



lm11.1 = lmer(ap_pos ~ age + female + poc + edu + inc + ideo + pid +
                soc_div + grp + div + sm.freq  + int + alg + case + pase + paie*size + caie +
                (1 | frame),
              data=d, weights=weights,
              control=lmerControl(optimizer="bobyqa")) 

lm11.2 = lmer(ap_pos ~ age + female + poc + edu + inc + ideo + pid +
                soc_div + size + grp + sm.freq  + int + alg + case + pase + paie*div + caie +
                (1 | frame),
              data=d, weights=weights,
              control=lmerControl(optimizer="bobyqa")) 


summary(lm10.1)
summary(lm10.2)
summary(lm11.1)
summary(lm11.2)

visreg(lm10.1, "caie", by="size", overlay=TRUE)
visreg(lm10.2, "caie", by="div", overlay=TRUE)
visreg(lm11.1, "paie", by="size", overlay=TRUE)
visreg(lm11.2, "paie", by="div", overlay=TRUE)










 



